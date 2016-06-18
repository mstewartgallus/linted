/*
 * Copyright 2015 Steven Stewart-Gallus
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 * implied.  See the License for the specific language governing
 * permissions and limitations under the License.
 */
#include "config.h"

#include "async.h"
#include "lntd/node.h"
#include "lntd/ko-stack.h"
#include "lntd/stack.h"
#include "lntd/util.h"

#include <errno.h>
#include <libgen.h>
#include <pthread.h>
#include <sched.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <sys/mman.h>
#include <sys/poll.h>
#include <sys/timerfd.h>
#include <sys/types.h>
#include <unistd.h>

#define MAXPOLLERS uniqueCount(LNTD_ASYNC_POLLER)
#define MAXCMDS uniqueCount(LNTD_ASYNC_COMMAND)
#define MAXTASKS uniqueCount(LNTD_TASK_UNIQUE)

module LntdNonblockPoolLinuxP
{
	uses interface LntdLogger;

	provides interface LntdTask[lntd_task_id task_id];
	provides interface LntdAsyncCommand[lntd_async_command_id id];
	provides interface LntdMainLoop;
}
implementation
{
	enum { TASK, COMMAND, SIGNAL };
	struct myevent {
		struct lntd_node node;
		unsigned char type;
		bool is_pending;
	};

	struct taskstate {
		struct myevent myevent;
	};

	struct cmd {
		struct myevent myevent;
		void *data;
		lntd_error err;
		volatile uint32_t finished;
		lntd_async_cmd_type type;
		unsigned short in_use : 1U;
		unsigned short have_waiter : 1U;
		unsigned short cancelled : 1U;
	};

	size_t global_argc;
	char const *const *global_argv;

	sigset_t listen_to_signals;

	volatile sig_atomic_t sigint_received;

	struct lntd_ko_stack *cmd_queue = 0;
	struct lntd_stack *event_queue = 0;

	struct myevent sigint_node;

	struct pollfd pollfds[MAXPOLLERS];
	struct cmd *poller_cmd[MAXPOLLERS];

	lntd_async_poller_id const newcmd_poller =
	    unique(LNTD_ASYNC_POLLER);
	lntd_async_command_id const exit_node_id =
	    unique(LNTD_ASYNC_COMMAND);

	bool time_to_exit;

	bool handle_command(void);
	lntd_error poll_for_io(void);

	void handle_sigint(int signo);
	void do_poll_command(lntd_async_poller_id ii, short revents);
	void finish_cmd(lntd_async_command_id id, lntd_error err);

	void *start_routine(void *foo);
	void *user_mainloop_routine(void *);

	struct taskstate *get_task(lntd_task_id id);
	lntd_task_id get_task_id(struct taskstate const *taskstate);

	struct cmd *get_cmd(lntd_async_command_id id);
	lntd_async_command_id get_cmd_id(struct cmd const *cmd);

	void push_cmd(struct lntd_ko_stack * stack, struct cmd * cmd);
	struct cmd *pop_cmd(struct lntd_ko_stack * stack);

	bool push_event(struct lntd_stack * stack, struct myevent * cmd,
	                unsigned char type);
	struct myevent *pop_event(struct lntd_stack * stack);

	command bool LntdTask.post_task[lntd_task_id task_id](void)
	{
		struct taskstate *taskstate;

		taskstate = get_task(task_id);

		return push_event(event_queue, &taskstate->myevent,
		                  TASK);
	}

	command void LntdAsyncCommand.execute[lntd_async_command_id id](
	    lntd_async_cmd_type type, void *data)
	{
		struct cmd *cmd;

		cmd = get_cmd(id);

		LNTD_ASSERT(!cmd->in_use);

		cmd->type = type;
		cmd->data = data;

		cmd->finished = false;
		cmd->have_waiter = false;
		cmd->cancelled = false;
		cmd->in_use = true;

		push_cmd(cmd_queue, cmd);
	}

	command void LntdAsyncCommand.cancel[lntd_async_command_id id](
	    void)
	{
		get_cmd(id)->cancelled = true;
	}

	command lntd_error
	    LntdAsyncCommand.execute_sync[lntd_async_command_id id](
	        lntd_async_cmd_type type, void *data)
	{
		struct cmd *cmd;

		cmd = get_cmd(id);

		LNTD_ASSERT(!cmd->in_use);

		cmd->type = type;
		cmd->data = data;

		cmd->finished = false;
		cmd->cancelled = false;
		cmd->in_use = true;
		cmd->have_waiter = true;

		push_cmd(cmd_queue, cmd);

		for (;;) {
			sched_yield();
			if (cmd->finished)
				break;
		}

		cmd->data = 0;
		cmd->have_waiter = false;
		cmd->in_use = false;

		return cmd->err;
	}

	command void LntdMainLoop.exit(lntd_error status)
	{
		push_cmd(cmd_queue, get_cmd(exit_node_id));
	}

	struct start_args {
		pthread_t parent;
		size_t argc;
		char const *const *argv;
	};

	int main(int argc, char **argv) @C() @spontaneous()
	{
		static pthread_t child;
		static struct start_args start_args = {0};
		lntd_error err = 0;

		start_args.parent = pthread_self();
		start_args.argc = argc;
		start_args.argv = (char const *const *)argv;

		err = pthread_create(&child, 0, start_routine,
		                     &start_args);
		if (err != 0)
			return EXIT_FAILURE;

		pthread_exit(0);
	}

	void *start_routine(void *foo)
	{
		int status;
		struct sigaction old_action;
		char const *program_name;
		lntd_error err = 0;

		struct start_args *args = foo;
		pthread_t parent = args->parent;
		size_t argc = args->argc;
		char const *const *argv = args->argv;

		err = pthread_join(parent, 0);
		if (err != 0) {
			exit(EXIT_FAILURE);
		}

		err = pthread_detach(pthread_self());
		if (err != 0)
			exit(EXIT_FAILURE);

		program_name = argv[0U];

		{
			char *dupstr = strdup(program_name);
			char const *base;
			if (0 == dupstr) {
				exit(EXIT_FAILURE);
			}

			base = basename(dupstr);

			call LntdLogger.init(base);

			free(dupstr);
		}

		{
			size_t ii;

			for (ii = 0U; ii < LNTD_ARRAY_SIZE(pollfds);
			     ++ii) {
				pollfds[ii].fd = -1;
				pollfds[ii].events = 0;
			}
		}

		err = lntd_ko_stack_create(&cmd_queue);
		if (err != 0)
			exit(EXIT_FAILURE);

		err = lntd_stack_create(&event_queue);
		if (err != 0)
			exit(EXIT_FAILURE);

		pollfds[newcmd_poller].fd = lntd_ko_stack_ko(cmd_queue);
		pollfds[newcmd_poller].events = POLLIN;

		{
			sigset_t signals;
			sigemptyset(&signals);
			sigaddset(&signals, SIGINT);

			pthread_sigmask(SIG_BLOCK, &signals,
			                &listen_to_signals);
		}

		{
			struct sigaction act = {0};
			sigemptyset(&act.sa_mask);
			act.sa_handler = handle_sigint;
			sigaction(SIGINT, &act, &old_action);
		}

		global_argc = argc;
		global_argv = (char const *const *)argv;
		{
			pthread_t xx;
			err = pthread_create(&xx, 0,
			                     user_mainloop_routine, 0);
			if (err != 0)
				exit(EXIT_FAILURE);
		}

		for (;;) {
			size_t ii;
			bool cancelled_a_poller = false;

			if (time_to_exit) {
				status = 0;
				goto exit_mainloop;
			}

			if (sigint_received) {
				sigint_received = false;
				push_event(event_queue, &sigint_node,
				           SIGNAL);
				continue;
			}

			for (ii = 0U; ii < LNTD_ARRAY_SIZE(pollfds);
			     ++ii) {
				struct cmd *cmd;

				cmd = poller_cmd[ii];
				if (0 == cmd)
					continue;

				if (cmd->cancelled) {
					cancelled_a_poller = true;
					pollfds[ii].fd = -1;
					pollfds[ii].events = 0;
					finish_cmd(
					    get_cmd_id(cmd),
					    LNTD_ERROR_CANCELLED);
					continue;
				}
			}
			if (cancelled_a_poller)
				continue;

			err = poll_for_io();
			if (err != 0) {
				status = EXIT_FAILURE;
				goto exit_mainloop;
			}
		}

	exit_mainloop:
		sigaction(SIGINT, &old_action, 0);

		pthread_sigmask(SIG_SETMASK, &listen_to_signals, 0);

		exit(signal LntdMainLoop.shutdown(status));
	}

	bool handle_command(void)
	{
		lntd_error err = 0;
		lntd_ko wait_ko;
		bool got_at_least_one = false;

		wait_ko = lntd_ko_stack_ko(cmd_queue);

		for (;;) {
			uint64_t xx;
			if (-1 == read(wait_ko, &xx, sizeof xx)) {
				err = errno;
				LNTD_ASSERT(err != 0);
				if (EINTR == err)
					continue;
				if (EAGAIN == err)
					break;
				LNTD_ASSERT(0);
			}
			break;
		}

		for (;;) {
			struct cmd *cmd;
			void *data;
			lntd_async_command_id id;
			lntd_async_poller_id poller;
			short events;
			int fd;

			cmd = pop_cmd(cmd_queue);
			if (0 == cmd)
				break;

			got_at_least_one = true;

			id = get_cmd_id(cmd);
			if (id == exit_node_id) {
				time_to_exit = true;
				return true;
			}
			data = cmd->data;

			if (cmd->cancelled) {
				err = LNTD_ERROR_CANCELLED;
				goto finish_command;
			}

			switch (cmd->type) {
			case LNTD_ASYNC_CMD_TYPE_WRITE: {
				struct lntd_async_cmd_write *w;
				lntd_ko ko;

				w = data;
				ko = w->ko;

				if (ko > INT_MAX) {
					err =
					    LNTD_ERROR_INVALID_PARAMETER;
					goto finish_command;
				}

				fd = ko;
				poller = w->poller;
				events = POLLOUT;
				goto poll_command;
			}

			case LNTD_ASYNC_CMD_TYPE_POLL: {
				struct lntd_async_cmd_poll *p;
				lntd_ko ko;
				short trans_events;

				p = data;

				trans_events = p->events;
				poller = p->poller;
				ko = p->ko;

				if (ko > INT_MAX) {
					err =
					    LNTD_ERROR_INVALID_PARAMETER;
					goto finish_command;
				}

				events = 0;
				if ((trans_events &
				     LNTD_ASYNC_POLLER_IN) != 0)
					events |= POLLIN;
				if ((trans_events &
				     LNTD_ASYNC_POLLER_OUT) != 0)
					events |= POLLOUT;

				fd = ko;
				goto poll_command;
			}

			case LNTD_ASYNC_CMD_TYPE_TIMER: {
				struct lntd_async_cmd_timer *p;
				struct itimerspec spec;
				lntd_ko ko;

				p = data;

				ko = p->ko;
				poller = p->poller;

				if (ko > INT_MAX) {
					err =
					    LNTD_ERROR_INVALID_PARAMETER;
					goto finish_command;
				}

				spec.it_interval.tv_sec = 0;
				spec.it_interval.tv_nsec = 0;

				spec.it_value = p->request;

				if (-1 == timerfd_settime(
				              ko, TFD_TIMER_ABSTIME,
				              &spec, 0)) {
					err = errno;
					goto finish_command;
				}

				fd = p->ko;
				events = POLLIN;
				goto poll_command;
			}

			case LNTD_ASYNC_CMD_TYPE_READ: {
				struct lntd_async_cmd_read *r;
				lntd_ko ko;

				r = data;

				ko = r->ko;
				poller = r->poller;

				if (ko > INT_MAX) {
					err =
					    LNTD_ERROR_INVALID_PARAMETER;
					goto finish_command;
				}

				fd = r->ko;
				events = POLLIN;
				goto poll_command;
			}

			case LNTD_ASYNC_CMD_TYPE_IDLE:
				err = 0;
				goto finish_command;

			default:
				LNTD_ASSERT(0);
			}

		finish_command:
			finish_cmd(id, err);
			continue;

		poll_command:
			LNTD_ASSERT(-1 == pollfds[poller].fd);
			pollfds[poller].fd = fd;
			pollfds[poller].events = events;
			poller_cmd[poller] = cmd;
			continue;
		}
		return got_at_least_one;
	}

	lntd_error poll_for_io(void)
	{
		int nfds;
		size_t ii;
		lntd_error err = 0;

		nfds = ppoll(pollfds, LNTD_ARRAY_SIZE(pollfds), 0,
		             &listen_to_signals);
		if (-1 == nfds) {
			err = errno;

			if (EINTR == err)
				return 0;

			return err;
		}

		for (ii = 0U; ii < LNTD_ARRAY_SIZE(pollfds); ++ii) {
			short revents;

			revents = pollfds[ii].revents;

			if (0 == revents)
				continue;

			if ((revents & POLLNVAL) != 0)
				continue;

			do_poll_command(ii, revents);

			--nfds;
			if (0 == nfds)
				break;
		}

		return 0;
	}

	void do_poll_command(lntd_async_poller_id ii, short revents)
	{
		struct cmd *cmd;
		lntd_async_command_id id;
		lntd_error err = 0;

		if (newcmd_poller == ii) {
			handle_command();
			return;
		}

		cmd = poller_cmd[ii];
		id = get_cmd_id(cmd);
		switch (cmd->type) {
		case LNTD_ASYNC_CMD_TYPE_WRITE: {
			struct lntd_async_cmd_write *w;
			ssize_t ret;
			size_t bytes_wrote = 0;

			w = cmd->data;

			ret = write(w->ko,
			            w->bytes + w->size - w->bytes_left,
			            w->bytes_left);
			if (-1 == ret) {
				err = errno;
			} else {
				err = 0;
				bytes_wrote = ret;
			}

			/* Just let the polling
			 * continue
			 */
			if (EINTR == err || EAGAIN == err ||
			    EWOULDBLOCK == err)
				break;

			if ((w->bytes_left -= bytes_wrote) != 0)
				break;

			pollfds[ii].fd = -1;
			pollfds[ii].events = 0;
			poller_cmd[ii] = 0;
			finish_cmd(id, err);
			break;
		}

		case LNTD_ASYNC_CMD_TYPE_POLL: {
			struct lntd_async_cmd_poll *p;
			uint_fast64_t trans_revents;

			trans_revents = 0U;
			if ((revents & POLLIN) != 0)
				trans_revents |= LNTD_ASYNC_POLLER_IN;
			if ((revents & POLLOUT) != 0)
				trans_revents |= LNTD_ASYNC_POLLER_OUT;

			p = cmd->data;

			p->revents = trans_revents;

			pollfds[ii].fd = -1;
			pollfds[ii].events = 0;
			poller_cmd[ii] = 0;
			finish_cmd(id, 0);
			break;
		}

		case LNTD_ASYNC_CMD_TYPE_TIMER: {
			struct lntd_async_cmd_timer *p;
			lntd_async_command_id overrun;

			p = cmd->data;

			if (-1 ==
			    read(p->ko, &overrun, sizeof overrun)) {
				err = errno;
			} else {
				err = 0;
			}

			/* Just let the polling continue */
			if (EINTR == err || EAGAIN == err ||
			    EWOULDBLOCK == err)
				break;

			pollfds[ii].fd = -1;
			pollfds[ii].events = 0;
			poller_cmd[ii] = 0;
			finish_cmd(id, 0);
			break;
		}

		case LNTD_ASYNC_CMD_TYPE_READ: {
			struct lntd_async_cmd_read *w;
			ssize_t ret;
			size_t bytes_read = 0;

			w = cmd->data;

			ret = read(w->ko,
			           w->bytes + w->size - w->bytes_left,
			           w->bytes_left);
			if (-1 == ret) {
				err = errno;
			} else {
				bytes_read = ret;
			}

			/* Just let the polling continue */
			if (EINTR == err || EAGAIN == err ||
			    EWOULDBLOCK == err)
				break;

			/* Short reads can happen */
			if (bytes_read != 0) {
				if ((w->bytes_left -= bytes_read) != 0)
					break;
			}

			pollfds[ii].fd = -1;
			pollfds[ii].events = 0;
			poller_cmd[ii] = 0;
			finish_cmd(id, err);
			break;
		}

		default:
			LNTD_ASSUME_UNREACHABLE();
		}
	}

	void finish_cmd(lntd_async_command_id id, lntd_error err)
	{
		struct cmd *cmd;

		cmd = get_cmd(id);

		cmd->finished = true;
		cmd->in_use = false;
		cmd->err = err;
		cmd->data = 0;

		if (!cmd->have_waiter) {
			push_event(event_queue, &cmd->myevent, COMMAND);
		}
		cmd->have_waiter = false;
	}

default event
	void LntdAsyncCommand.done[lntd_async_command_id id](
	    lntd_error err)
	{
		LNTD_CRASH_FAST();
	}

default event
	void LntdTask.run_task[lntd_task_id id](void)
	{
		LNTD_CRASH_FAST();
	}

	void handle_sigint(int signo)
	{
		sigint_received = true;
	}

	void *user_mainloop_routine(void *foo)
	{
		signal LntdMainLoop.boot(global_argc, global_argv);

		for (;;) {
			struct myevent *myevent;

			myevent = pop_event(event_queue);

			switch (myevent->type) {
			case TASK: {
				struct taskstate *taskstate;

				taskstate = (void *)myevent;
				signal LntdTask
				    .run_task[get_task_id(taskstate)]();
				continue;
			}

			case COMMAND: {
				struct cmd *cmd;

				cmd = (void *)myevent;
				cmd->in_use = false;

				signal LntdAsyncCommand
				    .done[get_cmd_id(cmd)](cmd->err);
				continue;
			}

			case SIGNAL:
				signal LntdMainLoop.recv_cancel();
				continue;

			default:
				LNTD_CRASH_FAST();
			}
		}
	}

	void push_cmd(struct lntd_ko_stack * stack, struct cmd * cmd)
	{
		lntd_ko_stack_send(stack, &cmd->myevent.node);
	}

	struct cmd *pop_cmd(struct lntd_ko_stack * stack)
	{
		struct lntd_node *node;
		if (EAGAIN == lntd_ko_stack_try_recv(stack, &node))
			return 0;
		return (void *)node;
	}

	bool push_event(struct lntd_stack * stack,
	                struct myevent * myevent, unsigned char type)
	{
		if (myevent->is_pending)
			return 1;

		myevent->type = type;
		myevent->is_pending = true;

		lntd_stack_send(stack, &myevent->node);

		return 0;
	}

	struct myevent *pop_event(struct lntd_stack * stack)
	{
		struct lntd_node *node;
		{
			struct lntd_node *xx;
			lntd_stack_recv(stack, &xx);
			node = xx;
		}

		((struct myevent *)node)->is_pending = false;

		return (void *)node;
	}

	struct taskstate tasks[MAXTASKS];

	struct taskstate *get_task(lntd_task_id id)
	{
		LNTD_ASSERT(id < LNTD_ARRAY_SIZE(tasks));
		return &tasks[id];
	}

	lntd_task_id get_task_id(struct taskstate const *taskstate)
	{
		return taskstate - tasks;
	}

	struct cmd cmds[MAXCMDS];

	struct cmd *get_cmd(lntd_async_command_id id)
	{
		LNTD_ASSERT(id < LNTD_ARRAY_SIZE(cmds));
		return &cmds[id];
	}

	lntd_async_command_id get_cmd_id(struct cmd const *cmd)
	{
		return cmd - cmds;
	}
}
