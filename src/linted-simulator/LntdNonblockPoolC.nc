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
#include "lntd/util.h"

#include <errno.h>
#include <libgen.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <sys/mman.h>
#include <sys/poll.h>
#include <sys/timerfd.h>
#include <sys/types.h>
#include <ucontext.h>
#include <unistd.h>

#ifndef ARRAY_SIZE
#define ARRAY_SIZE(...) (sizeof(__VA_ARGS__) / sizeof(__VA_ARGS__)[0U])
#endif

typedef uint8_t lntd_task_id;
typedef uint8_t lntd_nonblock_pool_id;

#define MAXCMDS uniqueCount(LNTD_ASYNC_COMMAND)
#define MAXTASKS                                                       \
	uniqueCount(                                                   \
	    "LntdScheduler-5167a5c7-2eb2-43cc-8e58-b0c6be0012c8")

module LntdNonblockPoolC
{
	uses interface LntdLogger;

	provides interface LntdTask[lntd_task_id task_id];
	provides interface LntdAsyncCommand[lntd_nonblock_pool_id id];
	provides interface LntdMainLoop;
}
implementation
{
	enum { TASK, COMMAND, SIGNAL };
	struct node {
		struct node *next;
		unsigned char type;
		bool is_pending;
	};

	struct taskstate {
		struct node node;
		lntd_task_id task_id;
	};

	struct cmd {
		struct node node;
		struct cmd *next;
		lntd_async_cmd_type type;
		void *data;
		lntd_nonblock_pool_id id;
		lntd_error err;
		bool in_use : 1U;
		bool have_waiter : 1U;
		bool cancelled : 1U;
	};

	ucontext_t io_context;
	ucontext_t user_context;

	sigset_t listen_to_signals;

	bool time_to_exit;
	lntd_error exit_status;

	volatile sig_atomic_t sigint_received;

	struct cmd *waiter = 0;
	struct node *event_queue = 0;
	struct cmd *cmd_queue = 0;

	struct node sigint_node;
	struct taskstate tasks[MAXTASKS];
	struct cmd cmds[MAXCMDS];
	struct pollfd pollfds[MAXCMDS];
	struct cmd *pollcmds[MAXCMDS];

	size_t get_default_stack_size(void);
	size_t get_page_size(void);

	void handle_sigint(int signo);
	void finish_cmd(struct cmd * cmd, lntd_nonblock_pool_id id,
	                lntd_error err);

	void user_mainloop_routine(size_t argc,
	                           char const *const *argv);

	void push_cmd(struct cmd * *stack, struct cmd * cmd);
	struct cmd *pop_cmd(struct cmd * *stack);

	bool push_node(struct node * *stack, struct node * cmd,
	               unsigned char type);
	struct node *pop_node(struct node * *stack);

	command bool LntdTask.post_task[lntd_task_id task_id](void)
	{
		struct taskstate *taskstate = &tasks[task_id];

		taskstate->task_id = task_id;

		return push_node(&event_queue, &taskstate->node, TASK);
	}

	command void LntdAsyncCommand.execute[lntd_nonblock_pool_id id](
	    lntd_async_cmd_type type, void *data)
	{
		struct cmd *cmd;

		LNTD_ASSERT(id < MAXCMDS);

		cmd = &cmds[id];

		LNTD_ASSERT(!cmd->in_use);

		cmd->type = type;
		cmd->data = data;

		cmd->have_waiter = false;
		cmd->cancelled = false;
		cmd->id = id;
		cmd->in_use = true;

		push_cmd(&cmd_queue, cmd);
	}

	command void LntdAsyncCommand.cancel[lntd_nonblock_pool_id id](
	    void)
	{
		cmds[id].cancelled = true;
	}

	command lntd_error
	    LntdAsyncCommand.execute_sync[lntd_nonblock_pool_id id](
	        lntd_async_cmd_type type, void *data)
	{
		struct cmd *cmd;

		LNTD_ASSERT(id < MAXCMDS);

		cmd = &cmds[id];

		LNTD_ASSERT(!cmd->in_use);

		cmd->type = type;
		cmd->data = data;

		cmd->cancelled = false;
		cmd->id = id;
		cmd->in_use = true;
		cmd->have_waiter = true;

		push_cmd(&cmd_queue, cmd);

		for (;;) {
			swapcontext(&user_context, &io_context);

			if (waiter != 0)
				break;
		}
		waiter = 0;

		cmd->data = 0;
		cmd->have_waiter = false;
		cmd->in_use = false;

		return cmd->err;
	}

	command void LntdMainLoop.exit(lntd_error status)
	{
		time_to_exit = true;
		exit_status = status;
	}

	int main(int argc, char **argv) @C() @spontaneous()
	{
		size_t stack_size;
		size_t page_size;
		int status;
		struct sigaction old_action;
		char const *program_name;
		char *stack;
		char *stack_start;
		size_t useable_stack_size;

		program_name = argv[0U];

		{
			char *dupstr = strdup(program_name);
			char const *base;
			if (0 == dupstr) {
				return EXIT_FAILURE;
			}

			base = basename(dupstr);

			call LntdLogger.init(base);

			free(dupstr);
		}

		{
			size_t ii;

			for (ii = 0U; ii < ARRAY_SIZE(pollfds); ++ii) {
				pollfds[ii].fd = -1;
				pollfds[ii].events = 0;
			}
		}

		useable_stack_size = get_default_stack_size();
		page_size = get_page_size();

		stack_size = useable_stack_size + 2U * page_size;

		stack = mmap(0, stack_size, PROT_READ | PROT_WRITE,
		             MAP_PRIVATE | MAP_ANONYMOUS |
		                 MAP_GROWSDOWN | MAP_STACK,
		             -1, 0);
		if (0 == stack) {
			return EXIT_FAILURE;
		}

		mprotect(stack, page_size, PROT_NONE);
		mprotect(stack + page_size + useable_stack_size,
		         page_size, PROT_NONE);

		stack_start = stack + page_size;

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

		getcontext(&user_context);
		user_context.uc_stack.ss_sp = stack_start;
		user_context.uc_stack.ss_size = useable_stack_size;
		user_context.uc_link = &io_context;
		sigdelset(&user_context.uc_sigmask, SIGINT);
		makecontext(&user_context,
		            (void (*)(void))user_mainloop_routine, 2,
		            argc, (char const *const *)argv);

		for (;;) {
			int nfds;
			size_t ii;
			struct cmd *cmd;

			if (sigint_received) {
				sigint_received = false;
				push_node(&event_queue, &sigint_node,
				          SIGNAL);
			}

			/* Poll for events to trigger */
			swapcontext(&io_context, &user_context);

			if (time_to_exit) {
				status = exit_status;
				goto exit_mainloop;
			}

			cmd = pop_cmd(&cmd_queue);
			if (cmd != 0) {
				void *data;
				lntd_nonblock_pool_id id;

				id = cmd->id;
				data = cmd->data;

				if (cmd->cancelled) {
					finish_cmd(cmd, id, ECANCELED);
					continue;
				}

				switch (cmd->type) {
				case LNTD_ASYNC_CMD_TYPE_WRITE: {
					struct lntd_async_cmd_write *w;

					w = data;

					LNTD_ASSERT(-1 ==
					            pollfds[id].fd);
					pollfds[id].fd = w->ko;
					pollfds[id].events = POLLOUT;
					pollcmds[id] = cmd;
					break;
				}

				case LNTD_ASYNC_CMD_TYPE_POLL: {
					struct lntd_async_cmd_poll *p;
					uint_fast64_t events;
					short pollflags;

					p = data;

					events = p->events;

					pollflags = 0;
					if ((events &
					     LNTD_ASYNC_POLLER_IN) != 0)
						pollflags |= POLLIN;
					if ((events &
					     LNTD_ASYNC_POLLER_OUT) !=
					    0)
						pollflags |= POLLOUT;

					LNTD_ASSERT(-1 ==
					            pollfds[id].fd);
					pollfds[id].fd = p->ko;
					pollfds[id].events = pollflags;
					pollcmds[id] = cmd;
					break;
				}

				case LNTD_ASYNC_CMD_TYPE_TIMER: {
					struct lntd_async_cmd_timer *p;
					struct itimerspec spec;

					p = data;

					spec.it_interval.tv_sec = 0;
					spec.it_interval.tv_nsec = 0;

					spec.it_value = p->request;

					if (-1 == timerfd_settime(
					              p->ko,
					              TFD_TIMER_ABSTIME,
					              &spec, 0)) {
						finish_cmd(cmd, id,
						           errno);
						break;
					}

					LNTD_ASSERT(-1 ==
					            pollfds[id].fd);
					pollfds[id].fd = p->ko;
					pollfds[id].events = POLLIN;
					pollcmds[id] = cmd;
					break;
				}

				case LNTD_ASYNC_CMD_TYPE_READ: {
					struct lntd_async_cmd_read *r;

					r = data;

					LNTD_ASSERT(-1 ==
					            pollfds[id].fd);
					pollfds[id].fd = r->ko;
					pollfds[id].events = POLLIN;
					pollcmds[id] = cmd;
					break;
				}

				case LNTD_ASYNC_CMD_TYPE_IDLE: {
					finish_cmd(cmd, id, 0);
					break;
				}

				default:
					LNTD_ASSERT(0);
				}
				continue;
			}

			{
				size_t ii;
				bool cancelled_a_poller = false;
				for (ii = 0U; ii < ARRAY_SIZE(pollcmds);
				     ++ii) {
					struct cmd *cmd;

					cmd = pollcmds[ii];
					if (0 == cmd)
						continue;

					if (cmd->cancelled) {
						cancelled_a_poller =
						    true;
						pollfds[ii].fd = -1;
						pollfds[ii].events = 0;
						pollcmds[ii] = 0;
						finish_cmd(cmd, cmd->id,
						           ECANCELED);
						continue;
					}
				}
				if (cancelled_a_poller)
					continue;
			}

			nfds = ppoll(pollfds, ARRAY_SIZE(pollfds), 0,
			             &listen_to_signals);
			if (-1 == nfds) {
				if (EINTR == errno) {
					continue;
				}
				status = EXIT_FAILURE;
				goto exit_mainloop;
			}

			for (ii = 0U; ii < ARRAY_SIZE(pollfds); ++ii) {
				short revents;
				struct cmd *cmd;

				revents = pollfds[ii].revents;

				if (0 == revents)
					continue;

				if ((revents & POLLNVAL) != 0)
					continue;

				cmd = pollcmds[ii];
				switch (cmd->type) {
				case LNTD_ASYNC_CMD_TYPE_WRITE: {
					struct lntd_async_cmd_write *w;
					int err = 0;
					ssize_t ret;
					size_t bytes_wrote = 0;

					w = cmd->data;

					ret = write(w->ko,
					            w->bytes + w->size -
					                w->bytes_left,
					            w->bytes_left);
					if (-1 == ret) {
						err = errno;
					} else {
						bytes_wrote = ret;
					}

					/* Just let the polling continue
					 */
					if (EINTR == err ||
					    EAGAIN == err ||
					    EWOULDBLOCK == err)
						break;

					if ((w->bytes_left -=
					     bytes_wrote) != 0)
						break;

					pollfds[ii].fd = -1;
					pollfds[ii].events = 0;
					pollcmds[ii] = 0;
					finish_cmd(cmd, ii, err);
					break;
				}

				case LNTD_ASYNC_CMD_TYPE_POLL: {
					struct lntd_async_cmd_poll *p;
					short pollflags;
					uint_fast64_t revents;

					revents = 0U;
					pollflags = pollfds[ii].revents;
					if ((pollflags & POLLIN) != 0)
						revents |=
						    LNTD_ASYNC_POLLER_IN;
					if ((pollflags & POLLOUT) != 0)
						revents |=
						    LNTD_ASYNC_POLLER_OUT;

					p = cmd->data;

					p->revents = revents;

					pollfds[ii].fd = -1;
					pollfds[ii].events = 0;
					pollcmds[ii] = 0;
					finish_cmd(cmd, ii, 0);
					break;
				}

				case LNTD_ASYNC_CMD_TYPE_TIMER: {
					struct lntd_async_cmd_timer *p;
					lntd_nonblock_pool_id overrun;
					lntd_error err = 0;

					p = cmd->data;

					if (-1 ==
					    read(p->ko, &overrun,
					         sizeof overrun)) {
						err = errno;
					}

					/* Just let the polling continue
					 */
					if (EINTR == err ||
					    EAGAIN == err ||
					    EWOULDBLOCK == err)
						break;

					pollfds[ii].fd = -1;
					pollfds[ii].events = 0;
					pollcmds[ii] = 0;
					finish_cmd(cmd, ii, 0);
					break;
				}

				case LNTD_ASYNC_CMD_TYPE_READ: {
					struct lntd_async_cmd_read *w;
					int err = 0;
					ssize_t ret;
					size_t bytes_read = 0;

					w = cmd->data;

					ret = read(w->ko,
					           w->bytes + w->size -
					               w->bytes_left,
					           w->bytes_left);
					if (-1 == ret) {
						err = errno;
					} else {
						bytes_read = ret;
					}

					/* Just let the polling continue
					 */
					if (EINTR == err ||
					    EAGAIN == err ||
					    EWOULDBLOCK == err)
						break;

					/* Short reads can happen */
					if (bytes_read != 0) {
						if ((w->bytes_left -=
						     bytes_read) != 0)
							break;
					}

					pollfds[ii].fd = -1;
					pollfds[ii].events = 0;
					pollcmds[ii] = 0;
					finish_cmd(cmd, ii, err);
					break;
				}

				default:
					LNTD_ASSERT(0);
				}

				--nfds;
				if (0 == nfds)
					break;
			}
		}

	exit_mainloop:
		sigaction(SIGINT, &old_action, 0);

		pthread_sigmask(SIG_SETMASK, &listen_to_signals, 0);

		munmap(stack, stack_size);

		return signal LntdMainLoop.shutdown(status);
	}

	void finish_cmd(struct cmd * cmd, lntd_nonblock_pool_id id,
	                lntd_error err)
	{
		cmd->in_use = false;
		cmd->id = id;
		cmd->err = err;
		cmd->data = 0;

		if (cmd->have_waiter) {
			LNTD_ASSERT(0 == waiter);
			waiter = cmd;
		} else {
			push_node(&event_queue, &cmd->node, COMMAND);
		}
		cmd->have_waiter = false;
	}

default event
	void LntdAsyncCommand.done[lntd_nonblock_pool_id id](
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

	void user_mainloop_routine(size_t argc, char const *const *argv)
	{
		signal LntdMainLoop.boot(argc, argv);

		for (;;) {
			struct node *node;

			swapcontext(&user_context, &io_context);

			node = pop_node(&event_queue);
			if (0 == node)
				continue;

			switch (node->type) {
			case TASK: {
				struct taskstate *taskstate;

				taskstate = (void *)node;
				signal LntdTask
				    .run_task[taskstate->task_id]();
				continue;
			}

			case COMMAND: {
				struct cmd *cmd;

				cmd = (void *)node;
				cmd->in_use = false;

				signal LntdAsyncCommand.done[cmd->id](
				    cmd->err);
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

	size_t get_default_stack_size(void)
	{
		pthread_attr_t attr;
		size_t stack_size;

		pthread_attr_init(&attr);
		pthread_attr_getstacksize(&attr, &stack_size);
		pthread_attr_destroy(&attr);

		return stack_size;
	}

	size_t get_page_size(void)
	{
		long size = sysconf(_SC_PAGE_SIZE);
		LNTD_ASSERT(size >= 0);
		return size;
	}

	void push_cmd(struct cmd * *stack, struct cmd * cmd)
	{
		cmd->next = *stack;
		*stack = cmd;
	}

	struct cmd *pop_cmd(struct cmd * *stack)
	{
		struct cmd *cmd;

		cmd = *stack;
		if (0 == cmd)
			return 0;

		*stack = cmd->next;

		return cmd;
	}

	bool push_node(struct node * *stack, struct node * node,
	               unsigned char type)
	{
		if (node->is_pending)
			return 1;

		node->type = type;
		node->is_pending = true;

		node->next = *stack;
		*stack = node;

		return 0;
	}

	struct node *pop_node(struct node * *stack)
	{
		struct node *node;

		node = *stack;
		if (0 == node)
			return 0;

		node->is_pending = false;

		*stack = node->next;

		return node;
	}
}
