/*
 * Copyright 2015, 2016 Steven Stewart-Gallus
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

#include "lntd/env.h"
#include "lntd/error.h"
#include "lntd/io.h"
#include "lntd/ko.h"
#include "lntd/log.h"
#include "lntd/mem.h"
#include "lntd/proc.h"
#include "lntd/util.h"
#include "lntd/window.h"
#include "lntd/xcb.h"

#include <errno.h>
#include <math.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include <xcb/xcb.h>

module LntdWindowP
{
	uses interface LntdMainLoop;
	uses interface LntdLogger;
	uses interface LntdStdio;

	uses interface LntdPoller as Poller;
	uses interface LntdWriter as GuiNotifier;
	uses interface LntdWriter as DrawerNotifier;
}
implementation
{
	lntd_ko kill_ko;
	lntd_ko window_ko;
	lntd_ko gui_notifier;
	lntd_ko drawer_notifier;
	lntd_proc root_pid;
	xcb_connection_t *connection;
	xcb_window_t window_id;

	char const dummy;

	void finish(lntd_error err);

	event void LntdMainLoop.boot(size_t argc,
	                             char const *const *argv)
	{
		lntd_error err = 0;

		char *display_string;
		char const *kill_ko_path;
		char const *window_path;
		char const *gui_notifier_path;
		char const *drawer_notifier_path;

		unsigned screen_number;

		xcb_screen_t *screen;

		xcb_void_cookie_t create_win_ck;
		xcb_intern_atom_cookie_t protocols_ck;
		xcb_intern_atom_cookie_t delete_ck;
		xcb_intern_atom_cookie_t pid_ck;
		xcb_atom_t wm_protocols_atom;
		xcb_atom_t wm_delete_window_atom;
		xcb_void_cookie_t change_ck;
		xcb_atom_t net_wm_pid_atom;

		if (argc < 3U) {
			call LntdLogger.log(
			    LNTD_LOGGER_ERROR,
			    "missing some of 2 file operands!");
			call LntdMainLoop.exit(EXIT_FAILURE);
			return;
		}

		kill_ko_path = argv[1U];
		window_path = argv[2U];
		gui_notifier_path = argv[3U];
		drawer_notifier_path = argv[4U];

		{
			char *root;
			long int the_root_pid;
			{
				char *xx;
				err = lntd_env_get("MANAGERPID", &xx);
				if (err != 0) {
					finish(err);
					return;
				}
				root = xx;
			}
			if (0 == root) {
				call LntdLogger.log(
				    LNTD_LOGGER_ERROR,
				    "MANAGERPID is a required "
				    "environment variable");
				call LntdMainLoop.exit(EXIT_FAILURE);
				return;
			}

			errno = 0;
			the_root_pid = strtol(root, 0, 10);
			err = errno;
			if (0 == err) {
				if (the_root_pid < 1) {
					err = ERANGE;
				}
				if (the_root_pid > INT_MAX) {
					err = ERANGE;
				}
			}

			lntd_mem_free(root);

			if (err != 0)
				goto destroy_window;
			root_pid = the_root_pid;
		}

		{
			lntd_ko xx;
			err = lntd_ko_open(&xx, LNTD_KO_CWD,
			                   kill_ko_path, LNTD_KO_RDWR);
			if (err != 0)
				goto destroy_window;
			kill_ko = xx;
		}

		{
			lntd_ko xx;
			err = lntd_ko_open(&xx, LNTD_KO_CWD,
			                   window_path, LNTD_KO_RDWR);
			if (err != 0)
				goto destroy_window;
			window_ko = xx;
		}

		{
			lntd_ko xx;
			err = lntd_ko_open(&xx, LNTD_KO_CWD,
			                   gui_notifier_path,
			                   LNTD_KO_RDWR);
			if (err != 0)
				goto destroy_window;
			gui_notifier = xx;
		}

		{
			lntd_ko xx;
			err = lntd_ko_open(&xx, LNTD_KO_CWD,
			                   drawer_notifier_path,
			                   LNTD_KO_RDWR);
			if (err != 0)
				goto destroy_window;
			drawer_notifier = xx;
		}

		{
			char *xx;
			err = lntd_env_get("DISPLAY", &xx);
			if (err != 0)
				goto destroy_window;
			display_string = xx;
		}

		{
			int xx;
			connection = xcb_connect(display_string, &xx);
			if (0 == connection) {
				err = LNTD_ERROR_UNIMPLEMENTED;
				goto destroy_window;
			}
			err = lntd_xcb_conn_error(connection);
			if (err != 0)
				goto destroy_window;
			screen_number = (unsigned)xx;
		}

		screen = 0;
		{
			xcb_screen_iterator_t iter;
			size_t ii;

			iter = xcb_setup_roots_iterator(
			    xcb_get_setup(connection));
			for (ii = 0U; ii < screen_number; ++ii) {
				if (0 == iter.rem)
					break;

				xcb_screen_next(&iter);
			}

			if (0 == iter.rem) {
				err = EINVAL;
				goto destroy_window;
			}

			screen = iter.data;
		}

		window_id = xcb_generate_id(connection);
		err = lntd_xcb_conn_error(connection);
		if (err != 0)
			goto destroy_window;

		{
			uint32_t value_mask =
			    XCB_CW_BACK_PIXEL | XCB_CW_BACKING_STORE;
			uint32_t const window_opts[] = {
			    screen->black_pixel,
			    XCB_BACKING_STORE_WHEN_MAPPED};

			create_win_ck = xcb_create_window_checked(
			    connection, XCB_COPY_FROM_PARENT, window_id,
			    screen->root, 0, 0, 640, 480, 0,
			    XCB_WINDOW_CLASS_INPUT_OUTPUT,
			    screen->root_visual, value_mask,
			    window_opts);
		}
		err = lntd_xcb_conn_error(connection);
		if (err != 0)
			goto destroy_window;

		protocols_ck = xcb_intern_atom(connection, 1,
		                               strlen("WM_PROTOCOLS"),
		                               "WM_PROTOCOLS");
		err = lntd_xcb_conn_error(connection);
		if (err != 0)
			goto destroy_window;

		delete_ck = xcb_intern_atom(connection, 0,
		                            strlen("WM_DELETE_WINDOW"),
		                            "WM_DELETE_WINDOW");
		err = lntd_xcb_conn_error(connection);
		if (err != 0)
			goto destroy_window;

		pid_ck = xcb_intern_atom(connection, 0,
		                         strlen("_NET_WM_PID"),
		                         "_NET_WM_PID");
		err = lntd_xcb_conn_error(connection);
		if (err != 0)
			goto destroy_window;

		{
			xcb_intern_atom_reply_t *proto_reply;
			xcb_generic_error_t *proto_err;
			{
				xcb_generic_error_t *xx = 0;
				proto_reply = xcb_intern_atom_reply(
				    connection, protocols_ck, &xx);
				proto_err = xx;
			}
			err = lntd_xcb_conn_error(connection);
			if (err != 0)
				goto destroy_window;

			if (proto_err != 0) {
				err = lntd_xcb_error(proto_err);
				lntd_mem_free(proto_err);
				goto destroy_window;
			}

			wm_protocols_atom = proto_reply->atom;
			lntd_mem_free(proto_reply);
		}

		{
			xcb_intern_atom_reply_t *delete_ck_reply;
			xcb_generic_error_t *delete_ck_err;
			{
				xcb_generic_error_t *xx = 0;
				delete_ck_reply = xcb_intern_atom_reply(
				    connection, delete_ck, &xx);
				delete_ck_err = xx;
			}
			err = lntd_xcb_conn_error(connection);
			if (err != 0)
				goto destroy_window;

			if (delete_ck_err != 0) {
				err = lntd_xcb_error(delete_ck_err);
				lntd_mem_free(delete_ck_err);
				goto destroy_window;
			}

			wm_delete_window_atom = delete_ck_reply->atom;
			lntd_mem_free(delete_ck_reply);
		}

		{
			xcb_atom_t xx = wm_delete_window_atom;
			change_ck = xcb_change_property_checked(
			    connection, XCB_PROP_MODE_REPLACE,
			    window_id, wm_protocols_atom, XCB_ATOM_ATOM,
			    32, 1U, &xx);
		}
		err = lntd_xcb_conn_error(connection);
		if (err != 0)
			goto destroy_window;

		{
			xcb_generic_error_t *change_err;

			change_err =
			    xcb_request_check(connection, change_ck);

			err = lntd_xcb_conn_error(connection);
			if (err != 0)
				goto destroy_window;
			if (change_err != 0) {
				err = lntd_xcb_error(change_err);
				lntd_mem_free(change_err);
				goto destroy_window;
			}
		}

		{
			xcb_intern_atom_reply_t *pid_ck_reply;
			xcb_generic_error_t *pid_ck_err;
			{
				xcb_generic_error_t *xx = 0;
				pid_ck_reply = xcb_intern_atom_reply(
				    connection, pid_ck, &xx);
				pid_ck_err = xx;
			}
			err = lntd_xcb_conn_error(connection);
			if (err != 0)
				goto destroy_window;

			if (pid_ck_err != 0) {
				err = lntd_xcb_error(pid_ck_err);
				lntd_mem_free(pid_ck_err);
				goto destroy_window;
			}

			net_wm_pid_atom = pid_ck_reply->atom;
			lntd_mem_free(pid_ck_reply);
		}

		xcb_change_property(
		    connection, XCB_PROP_MODE_REPLACE, window_id,
		    XCB_ATOM_WM_CLASS, XCB_ATOM_STRING, 8,
		    sizeof PACKAGE_TARNAME + sizeof PACKAGE_NAME,
		    PACKAGE_TARNAME "\0" PACKAGE_NAME);
		err = lntd_xcb_conn_error(connection);
		if (err != 0)
			goto destroy_window;

		xcb_change_property(connection, XCB_PROP_MODE_REPLACE,
		                    window_id, XCB_ATOM_WM_NAME,
		                    XCB_ATOM_STRING, 8,
		                    strlen(PACKAGE_NAME), PACKAGE_NAME);
		err = lntd_xcb_conn_error(connection);
		if (err != 0)
			goto destroy_window;

		xcb_change_property(connection, XCB_PROP_MODE_REPLACE,
		                    window_id, XCB_ATOM_WM_ICON_NAME,
		                    XCB_ATOM_STRING, 8,
		                    strlen(PACKAGE_NAME), PACKAGE_NAME);
		err = lntd_xcb_conn_error(connection);
		if (err != 0)
			goto destroy_window;

		{
			char buf[HOST_NAME_MAX + 1U];
			if (-1 == gethostname(buf, sizeof buf))
				goto get_hostname_failed;

			xcb_change_property(
			    connection, XCB_PROP_MODE_REPLACE,
			    window_id, XCB_ATOM_WM_CLIENT_MACHINE,
			    XCB_ATOM_STRING, 8, strlen(buf), buf);
			goto get_hostname_succeeded;
		}

	get_hostname_failed:
		err = errno;
		LNTD_ASSUME(err != 0);
		goto destroy_window;

	get_hostname_succeeded:
		err = lntd_xcb_conn_error(connection);
		if (err != 0)
			goto destroy_window;

		{
			uint32_t xx = root_pid;
			xcb_change_property(
			    connection, XCB_PROP_MODE_REPLACE,
			    window_id, net_wm_pid_atom,
			    XCB_ATOM_CARDINAL, 32, 1, &xx);
		}
		err = lntd_xcb_conn_error(connection);
		if (err != 0)
			goto destroy_window;

		xcb_change_property(
		    connection, XCB_PROP_MODE_REPLACE, window_id,
		    XCB_ATOM_WM_COMMAND, XCB_ATOM_STRING, 8,
		    strlen(PACKAGE_TARNAME), PACKAGE_TARNAME);
		err = lntd_xcb_conn_error(connection);
		if (err != 0)
			goto destroy_window;

		xcb_map_window(connection, window_id);
		err = lntd_xcb_conn_error(connection);
		if (err != 0)
			goto destroy_window;

		{
			xcb_generic_error_t *create_win_err;

			create_win_err = xcb_request_check(
			    connection, create_win_ck);
			if (create_win_err != 0) {
				err = lntd_xcb_error(create_win_err);
				lntd_mem_free(create_win_err);
				goto destroy_window;
			}
		}

		err = lntd_xcb_conn_error(connection);
		if (err != 0)
			goto destroy_window;

		xcb_flush(connection);

		err = lntd_window_write(window_ko, window_id);
		if (err != 0)
			goto destroy_window;

		call GuiNotifier.write_start(gui_notifier, &dummy,
		                             sizeof dummy);
		call DrawerNotifier.write_start(drawer_notifier, &dummy,
		                                sizeof dummy);
		call Poller.poll_start(
		    xcb_get_file_descriptor(connection),
		    LNTD_ASYNC_POLLER_IN);

		return;

	destroy_window:
		finish(err);
	}

	event int LntdMainLoop.shutdown(lntd_error err)
	{
		lntd_ko_close(kill_ko);
		lntd_ko_close(window_ko);
		lntd_ko_close(gui_notifier);
		lntd_ko_close(drawer_notifier);

		if (err != 0)
			return EXIT_FAILURE;

		return EXIT_SUCCESS;
	}

	event void LntdMainLoop.recv_cancel(void)
	{
		finish(0);
	}

	event void DrawerNotifier.write_done(lntd_error err)
	{
		if (err != 0)
			finish(err);
	}

	event void GuiNotifier.write_done(lntd_error err)
	{
		if (err != 0)
			finish(err);
	}

	event void Poller.poll_done(lntd_error err,
	                            uint_fast64_t revents)
	{
		if (err != 0) {
			finish(err);
			return;
		}

		for (;;) {
			xcb_generic_event_t *myevent;
			bool time_to_quit;

			myevent = xcb_poll_for_event(connection);
			if (0 == myevent)
				break;

			time_to_quit = false;
			switch (myevent->response_type & ~0x80) {
			case XCB_CLIENT_MESSAGE:
				goto quit_application;

			default:
				/* Unknown event type, ignore it */
				break;

			quit_application:
				time_to_quit = true;
				break;
			}
			lntd_mem_free(myevent);

			if (time_to_quit)
				finish(0);
		}

		call Poller.poll_start(
		    xcb_get_file_descriptor(connection),
		    LNTD_ASYNC_POLLER_IN);
	}

	void finish(lntd_error err)
	{
		if (err != 0) {
			char *errmsg;

			errno = err;
			if (-1 == asprintf(&errmsg, "%m")) {
				return;
			}

			call LntdLogger.log(LNTD_LOGGER_ERROR, errmsg);

			free(errmsg);
		}

		lntd_io_write_all(kill_ko, 0, &dummy, sizeof dummy);

		call LntdMainLoop.exit(err);
	}
}
