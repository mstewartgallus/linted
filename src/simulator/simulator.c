/*
 * Copyright 2013, 2014 Steven Stewart-Gallus
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#define _POSIX_C_SOURCE 199309L

#include "config.h"

#include "linted/asynch.h"
#include "linted/error.h"
#include "linted/controller.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/log.h"
#include "linted/start.h"
#include "linted/updater.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <sys/syscall.h>
#include <time.h>

#include <linux/filter.h>
#include <linux/seccomp.h>

#define ROTATION_SPEED 512U
#define DEAD_ZONE (LINTED_UPDATER_INT_MAX / 8)

enum { ON_READ_TIMER,
       ON_RECEIVE_CONTROLLER_EVENT,
       ON_SENT_UPDATER_EVENT,
       MAX_TASKS };

struct action_state
{
	linted_updater_int x_tilt;
	linted_updater_int y_tilt;

	int x : 2U;
	int z : 2U;

	bool jumping : 1U;
};

struct differentiable
{
	linted_updater_int value;
	linted_updater_int old;
};

struct simulator_state
{
	struct differentiable position[3U];

	linted_updater_angle x_rotation;
	linted_updater_angle y_rotation;

	bool update_pending : 1U;
	bool write_in_progress : 1U;
};

struct updater_task;

struct tick_task
{
	struct linted_asynch_task_sleep_until parent;
	struct linted_asynch_pool *pool;
	struct updater_task *updater_task;
	struct action_state const *action_state;
	struct simulator_state *simulator_state;
	linted_ko updater;
};
#define TICK_UPCAST(X) LINTED_ASYNCH_SLEEP_UNTIL_UPCAST(LINTED_UPCAST(X))
#define TICK_DOWNCAST(X)                                                       \
	LINTED_DOWNCAST(struct tick_task, LINTED_ASYNCH_SLEEP_UNTIL_DOWNCAST(X))

struct controller_task
{
	struct linted_controller_task_receive parent;
	struct linted_asynch_pool *pool;
	struct action_state *action_state;
};
#define CONTROLLER_UPCAST(X) LINTED_CONTROLLER_RECEIVE_UPCAST(LINTED_UPCAST(X))
#define CONTROLLER_DOWNCAST(X)                                                 \
	LINTED_DOWNCAST(struct controller_task,                                \
	                LINTED_CONTROLLER_RECEIVE_DOWNCAST(X))

struct updater_task
{
	struct linted_updater_task_send parent;
	struct simulator_state *simulator_state;
	struct linted_asynch_pool *pool;
	linted_ko updater;
};
#define UPDATER_UPCAST(X) LINTED_UPDATER_SEND_UPCAST(LINTED_UPCAST(X))
#define UPDATER_DOWNCAST(X)                                                    \
	LINTED_DOWNCAST(struct updater_task, LINTED_UPDATER_SEND_DOWNCAST(X))

static linted_ko kos[3U];
static struct sock_fprog const seccomp_filter;
struct linted_start_config const linted_start_config = {
    .canonical_process_name = PACKAGE_NAME "-simulator",
    .kos_size = LINTED_ARRAY_SIZE(kos),
    .kos = kos,
    .seccomp_bpf = NULL}; // &seccomp_filter};

static linted_error dispatch(struct linted_asynch_task *completed_task);

static linted_error on_read_timer(struct linted_asynch_task *completed_task);
static linted_error on_controller_receive(struct linted_asynch_task *task);
static linted_error on_sent_update(struct linted_asynch_task *completed_task);

static void maybe_update(linted_updater updater,
                         struct simulator_state *simulator_state,
                         struct updater_task *updater_task,
                         struct linted_asynch_pool *pool);

static void simulate_tick(struct simulator_state *simulator_state,
                          struct action_state const *action_state);
static void simulate_rotation(linted_updater_angle *rotation,
                              linted_updater_int tilt);
static void simulate_clamped_rotation(linted_updater_angle *rotation,
                                      linted_updater_int tilt);

static linted_updater_int circle(linted_updater_int x, linted_updater_int y);
static linted_updater_uint absolute(linted_updater_int x);
static linted_updater_int min_int(linted_updater_int x, linted_updater_int y);
static linted_updater_int sign(linted_updater_int x);

unsigned char linted_start(char const *const process_name, size_t argc,
                           char const *const argv[const])
{
	linted_log log = kos[0U];
	linted_controller controller = kos[1U];
	linted_updater updater = kos[2U];

	linted_error errnum;

	{
		static char const message[] = "starting simulator";
		linted_log_write(log, message, sizeof message - 1U);
	}

	struct action_state action_state = {.x = 0, .z = 0, .jumping = false};

	struct simulator_state simulator_state = {
	    .update_pending = true, /* Initialize the gui at start */
	    .write_in_progress = false,
	    .position = {{.value = 0, .old = 0},
	                 {.value = 0, .old = 0},
	                 {.value = 3 * 1024, .old = 3 * 1024}},
	    .x_rotation = LINTED_UPDATER_ANGLE(1U, 2U),
	    .y_rotation = LINTED_UPDATER_ANGLE(0U, 1U)};

	struct linted_asynch_pool *pool;
	{
		struct linted_asynch_pool *xx;
		errnum = linted_asynch_pool_create(&xx, MAX_TASKS);
		if (errnum != 0)
			goto exit;
		pool = xx;
	}

	struct tick_task timer_task;
	struct controller_task controller_task;
	struct updater_task updater_task;

	{
		struct timespec now;
		clock_gettime(CLOCK_MONOTONIC, &now);

		linted_asynch_task_sleep_until(LINTED_UPCAST(&timer_task),
		                               ON_READ_TIMER, TIMER_ABSTIME,
		                               &now);
	}
	timer_task.pool = pool;
	timer_task.updater_task = &updater_task;
	timer_task.action_state = &action_state;
	timer_task.simulator_state = &simulator_state;
	timer_task.updater = updater;

	linted_controller_receive(LINTED_UPCAST(&controller_task),
	                          ON_RECEIVE_CONTROLLER_EVENT, controller);
	controller_task.pool = pool;
	controller_task.action_state = &action_state;

	linted_asynch_pool_submit(pool, TICK_UPCAST(&timer_task));
	linted_asynch_pool_submit(pool, CONTROLLER_UPCAST(&controller_task));

	/* TODO: Detect SIGTERM and exit normally */
	for (;;) {
		struct linted_asynch_task *completed_task;
		{
			struct linted_asynch_task *xx;
			linted_asynch_pool_wait(pool, &xx);
			completed_task = xx;
		}

		errnum = dispatch(completed_task);
		if (errnum != 0)
			goto destroy_pool;
	}

destroy_pool : {
	linted_asynch_pool_stop(pool);

	for (;;) {
		struct linted_asynch_task *completed_task;
		linted_error poll_errnum;
		{
			struct linted_asynch_task *xx;
			poll_errnum = linted_asynch_pool_poll(pool, &xx);
			if (EAGAIN == poll_errnum)
				break;
			completed_task = xx;
		}

		linted_error dispatch_errnum = completed_task->errnum;
		if (0 == errnum)
			errnum = dispatch_errnum;
	}

	linted_error destroy_errnum = linted_asynch_pool_destroy(pool);
	if (0 == errnum)
		errnum = destroy_errnum;
	/* Insure that the tasks are in proper scope until they are
	 * terminated */
	(void)timer_task;
	(void)controller_task;
	(void)updater_task;
}

exit:
	return errnum;
}

#define ALLOW(XX)                                                              \
	BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_##XX, 0U, 1U),                \
	    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW)

static struct sock_filter const real_filter[] = {
    /*  */ BPF_STMT(BPF_LD | BPF_W | BPF_ABS,
                    offsetof(struct seccomp_data, nr)),
    /*  */ ALLOW(access),
    /*  */ ALLOW(arch_prctl),
    /*  */ ALLOW(brk),
    /*  */ ALLOW(chdir),
    /*  */ ALLOW(clock_nanosleep),
    /*  */ ALLOW(clone),
    /*  */ ALLOW(close),
    /*  */ ALLOW(dup2),
    /*  */ ALLOW(execve),
    /*  */ ALLOW(exit_group),
    /*  */ ALLOW(fcntl),
    /*  */ ALLOW(fstat),
    /*  */ ALLOW(futex),
    /*  */ ALLOW(getdents),
    /*  */ ALLOW(geteuid),
    /*  */ ALLOW(getpid),
    /*  */ ALLOW(getrlimit),
    /*  */ ALLOW(gettid),
    /*  */ ALLOW(getuid),
    /*  */ ALLOW(lseek),
    /*  */ ALLOW(mmap),
    /*  */ ALLOW(mprotect),
    /*  */ ALLOW(mq_timedreceive),
    /*  */ ALLOW(mq_timedsend),
    /*  */ ALLOW(munmap),
    /*  */ ALLOW(open),
    /*  */ ALLOW(openat),
    /*  */ ALLOW(poll),
    /*  */ ALLOW(prctl),
    /*  */ ALLOW(read),
    /*  */ ALLOW(restart_syscall),
    /*  */ ALLOW(rt_sigaction),
    /*  */ ALLOW(rt_sigprocmask),
    /*  */ ALLOW(sched_getaffinity),
    /*  */ ALLOW(setrlimit),
    /*  */ ALLOW(set_robust_list),
    /*  */ ALLOW(set_tid_address),
    /*  */ ALLOW(stat),
    /*  */ ALLOW(tgkill),
    /*  */ BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_KILL)};

static struct sock_fprog const seccomp_filter = {
    .len = LINTED_ARRAY_SIZE(real_filter),
    .filter = (struct sock_filter *)real_filter};

static linted_error dispatch(struct linted_asynch_task *completed_task)
{
	switch (completed_task->task_action) {
	case ON_READ_TIMER:
		return on_read_timer(completed_task);

	case ON_RECEIVE_CONTROLLER_EVENT:
		return on_controller_receive(completed_task);

	case ON_SENT_UPDATER_EVENT:
		return on_sent_update(completed_task);

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}

static linted_error on_read_timer(struct linted_asynch_task *completed_task)
{
	linted_error errnum;

	if ((errnum = completed_task->errnum) != 0)
		return errnum;

	struct tick_task *timer_task = TICK_DOWNCAST(completed_task);

	struct linted_asynch_pool *pool = timer_task->pool;
	linted_ko updater = timer_task->updater;
	struct updater_task *updater_task = timer_task->updater_task;
	struct action_state const *action_state = timer_task->action_state;
	struct simulator_state *simulator_state = timer_task->simulator_state;

	time_t requested_sec = LINTED_UPCAST(timer_task)->request.tv_sec;
	long requested_nsec = LINTED_UPCAST(timer_task)->request.tv_nsec;

	long const second = 1000000000;
	requested_nsec += second / 60;
	if (requested_nsec >= second) {
		requested_nsec -= second;
		requested_sec += 1;
	}

	LINTED_UPCAST(timer_task)->request.tv_sec = requested_sec;
	LINTED_UPCAST(timer_task)->request.tv_nsec = requested_nsec;

	linted_asynch_pool_submit(pool, completed_task);

	simulate_tick(simulator_state, action_state);

	maybe_update(updater, simulator_state, updater_task, pool);

	return 0;
}

static linted_error on_controller_receive(struct linted_asynch_task *task)
{
	linted_error errnum;

	if ((errnum = task->errnum) != 0)
		return errnum;

	struct controller_task *controller_task = CONTROLLER_DOWNCAST(task);

	struct linted_asynch_pool *pool = controller_task->pool;
	struct action_state *action_state = controller_task->action_state;

	struct linted_controller_message message;
	errnum =
	    linted_controller_decode(LINTED_UPCAST(controller_task), &message);
	if (errnum != 0)
		return errnum;

	linted_asynch_pool_submit(pool, task);

	action_state->x = message.right - message.left;
	action_state->z = message.back - message.forward;

	action_state->x_tilt = -message.x_tilt;
	action_state->y_tilt = -message.y_tilt;

	action_state->jumping = message.jumping;

	return 0;
}

static linted_error on_sent_update(struct linted_asynch_task *completed_task)
{
	linted_error errnum;

	if ((errnum = completed_task->errnum) != 0)
		return errnum;

	struct updater_task *updater_task = UPDATER_DOWNCAST(completed_task);

	struct linted_asynch_pool *pool = updater_task->pool;
	linted_ko updater = updater_task->updater;
	struct simulator_state *simulator_state = updater_task->simulator_state;

	simulator_state->write_in_progress = false;

	maybe_update(updater, simulator_state, updater_task, pool);

	return 0;
}

static void maybe_update(linted_updater updater,
                         struct simulator_state *simulator_state,
                         struct updater_task *updater_task,
                         struct linted_asynch_pool *pool)
{
	if (!simulator_state->update_pending)
		return;

	if (simulator_state->write_in_progress)
		return;

	{
		struct linted_updater_update update = {
		    .x_position = simulator_state->position[0U].value,
		    .y_position = simulator_state->position[1U].value,
		    .z_position = simulator_state->position[2U].value,
		    .x_rotation = simulator_state->x_rotation,
		    .y_rotation = simulator_state->y_rotation};

		linted_updater_send(LINTED_UPCAST(updater_task),
		                    ON_SENT_UPDATER_EVENT, updater, &update);
	}

	updater_task->simulator_state = simulator_state;
	updater_task->pool = pool;
	updater_task->updater = updater;

	linted_asynch_pool_submit(pool, UPDATER_UPCAST(updater_task));

	simulator_state->update_pending = false;
	simulator_state->write_in_progress = true;
}

static linted_updater_int resolve(linted_updater_int x)
{
	return (INTMAX_C(16) * x) / LINTED_UPDATER_INT_MAX;
}

static void simulate_tick(struct simulator_state *simulator_state,
                          struct action_state const *action_state)
{

	linted_updater_angle x_rotation = simulator_state->x_rotation;
	struct differentiable *positions = simulator_state->position;
	size_t positions_size = LINTED_ARRAY_SIZE(simulator_state->position);

	linted_updater_int x = action_state->x;
	linted_updater_int z = action_state->z;

	linted_updater_int cos_x = linted_updater_cos(x_rotation);
	linted_updater_int sin_x = linted_updater_sin(x_rotation);

	linted_updater_int forward_thrusts[3U] = {
		-resolve(sin_x * z),
		0,
		-resolve(cos_x * z)
	};

	linted_updater_int strafe_thrusts[3U] = {
		-resolve(cos_x * x),
		0,
		resolve(sin_x * x)
	};

	linted_updater_int jump_thrusts[3U] = {
		0,
		resolve(-LINTED_UPDATER_INT_MAX * action_state->jumping),
		0
	};

	linted_updater_int thrusts[3U];
	for (size_t ii = 0U; ii < positions_size; ++ii) {
		thrusts[ii] = strafe_thrusts[ii]
			+ forward_thrusts[ii]
			+ jump_thrusts[ii];
	}

	for (size_t ii = 0U; ii < positions_size; ++ii) {
		struct differentiable *pos = &positions[ii];

		linted_updater_int position = pos->value;
		linted_updater_int old_position = pos->old;

		linted_updater_int old_velocity = position - old_position;
		linted_updater_int thrust = thrusts[ii];

		linted_updater_int guess_velocity =
		    linted_updater_isatadd(thrust, old_velocity);

		linted_updater_int friction =
		    min_int(absolute(guess_velocity), 5 /* = μ Fₙ */) *
		    -sign(guess_velocity);

		linted_updater_int new_velocity =
		    linted_updater_isatadd(guess_velocity, friction);

		linted_updater_int new_position =
		    linted_updater_isatadd(position, new_velocity);

		pos->value = new_position;
		pos->old = position;
	}

	simulate_rotation(&simulator_state->x_rotation, action_state->x_tilt);
	simulate_clamped_rotation(&simulator_state->y_rotation,
	                          action_state->y_tilt);

	simulator_state->update_pending = true;
}

static void simulate_rotation(linted_updater_angle *rotation,
                              linted_updater_int tilt)
{

	linted_updater_angle increment =
	    LINTED_UPDATER_ANGLE(1, ROTATION_SPEED);

	*rotation = linted_updater_angle_add(
	    (absolute(tilt) > DEAD_ZONE) * sign(tilt), *rotation, increment);
}

static void simulate_clamped_rotation(linted_updater_angle *rotation,
                                      linted_updater_int tilt)
{
	linted_updater_int tilt_sign = sign(tilt);

	linted_updater_angle new_rotation;

	if (absolute(tilt) <= DEAD_ZONE) {
		new_rotation = *rotation;
	} else {
		linted_updater_angle minimum = LINTED_UPDATER_ANGLE(15U, 16U);
		linted_updater_angle maximum = LINTED_UPDATER_ANGLE(3U, 16U);
		linted_updater_angle increment =
		    LINTED_UPDATER_ANGLE(1U, ROTATION_SPEED);

		new_rotation = linted_updater_angle_add_clamped(
		    tilt_sign, minimum, maximum, *rotation, increment);
	}

	*rotation = new_rotation;
}

static linted_updater_int circle(linted_updater_int x, linted_updater_int y)
{
	/* Just cheat and use the double sqrt for now */
	return sqrt(((intmax_t)x) * x + ((intmax_t)y) * y);
}

static linted_updater_int min_int(linted_updater_int x, linted_updater_int y)
{
	return x < y ? x : y;
}

static linted_updater_int sign(linted_updater_int x)
{
	return x > 0 ? 1 : 0 == x ? 0 : -1;
}

static linted_updater_uint absolute(linted_updater_int x)
{
	/* The implicit cast to unsigned is okay, obviously */
	return LINTED_UPDATER_INT_MIN == x
	           ? -(int_fast64_t)LINTED_UPDATER_INT_MIN
	           : imaxabs(x);
}
