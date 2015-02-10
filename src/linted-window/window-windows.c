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
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#define _WIN32_WINNT 0x0600

#ifndef UNICODE
#define UNICODE
#endif

#define _UNICODE

#define WIN32_LEAN_AND_MEAN

#include "linted/start.h"

#include "linted/error.h"
#include "linted/log.h"

#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <windows.h>
#include <d3d9.h>

HRESULT WINAPI Direct3DCreate9Ex(UINT SDKVersion, IDirect3D9Ex **);

static HINSTANCE get_current_module(void)
{
	static char const dummy;

	HINSTANCE xx;
	GetModuleHandleEx(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS |
	                      GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,
	                  (void *)&dummy, &xx);
	return xx;
}

LRESULT CALLBACK window_procedure(HWND, UINT, WPARAM, LPARAM);

static unsigned char window_start(char const *process_name, size_t argc,
                                  char const *const argv[]);

struct linted_start_config const linted_start_config = {
	.canonical_process_name = PACKAGE_NAME "-window", .start = window_start
};

static unsigned char window_start(char const *process_name, size_t argc,
                                  char const *const argv[])
{
	linted_error errnum = 0;

	HGDIOBJ arrow_cursor = LoadCursor(0, IDC_ARROW);
	if (0 == arrow_cursor) {
		errnum = GetLastError();
		assert(errnum != 0);
		goto report_exit_status;
	}

	HGDIOBJ white_brush = GetStockObject(WHITE_BRUSH);
	if (0 == white_brush) {
		errnum = GetLastError();
		assert(errnum != 0);
		goto report_exit_status;
	}

	ATOM class_atom;
	{
		WNDCLASS window_class = { 0 };

		window_class.lpfnWndProc = window_procedure;
		window_class.style = CS_HREDRAW | CS_VREDRAW;
		window_class.hInstance = get_current_module();
		window_class.hCursor = arrow_cursor;
		window_class.hbrBackground = white_brush;
		window_class.lpszClassName =
		    L"" PACKAGE_NAME_SPACE ".MainWindowClass";

		class_atom = RegisterClass(&window_class);
	}
	if (0 == class_atom) {
		errnum = GetLastError();
		assert(errnum != 0);
		goto report_exit_status;
	}

	HWND main_window = CreateWindowEx(
	    WS_EX_APPWINDOW | WS_EX_COMPOSITED, (LPCTSTR)(uintptr_t)class_atom,
	    L"" PACKAGE_NAME, WS_OVERLAPPED | WS_CAPTION | WS_THICKFRAME |
	                          WS_MINIMIZEBOX | WS_MAXIMIZEBOX,
	    CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, 0, 0,
	    get_current_module(), 0);
	if (0 == main_window) {
		errnum = GetLastError();
		assert(errnum != 0);
		goto destroy_window;
	}

	switch (ShowWindow(main_window, linted_start_show_command())) {
	case -1: {
		errnum = GetLastError();
		assert(errnum != 0);
		goto destroy_window;
	}

	case 0:
	default:
		break;
	}

	IDirect3D9Ex *context;
	{
		IDirect3D9Ex *xx;
		HRESULT result = Direct3DCreate9Ex(D3D_SDK_VERSION, &xx);
		if (FAILED(result)) {
			errnum = ENOSYS;
			goto destroy_window;
		}
		context = xx;
	}

	IDirect3DDevice9 *device;
	{
		D3DPRESENT_PARAMETERS presentation_parameters = { 0 };
		presentation_parameters.BackBufferFormat = D3DFMT_UNKNOWN;
		presentation_parameters.hDeviceWindow = main_window;
		presentation_parameters.Windowed = true;

		IDirect3DDevice9 *xx;
		HRESULT result = IDirect3D9Ex_CreateDevice(
		    context, D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, main_window, 0,
		    &presentation_parameters, &xx);
		if (FAILED(result)) {
			errnum = ENOSYS;
			goto release_context;
		}
		device = xx;
	}

	IDirect3DSwapChain9 *swap_chain;
	{
		IDirect3DSwapChain9 *xx;
		IDirect3DDevice9Ex_GetSwapChain(device, 0U, &xx);
		swap_chain = xx;
	}

	if (0 == UpdateWindow(main_window)) {
		errnum = GetLastError();
		assert(errnum != 0);
		goto release_swap_chain;
	}

	for (;;) {
		for (;;) {
			MSG message;
			switch (PeekMessage(&message, 0, 0, 0, PM_REMOVE)) {
			case -1:
				return -1;

			case 0:
				goto exit_peek_loop;

			default:
				if (WM_QUIT == message.message) {
					errnum = message.wParam;
					goto release_swap_chain;
				}

				if (-1 == TranslateMessage(&message)) {
					errnum = GetLastError();
					goto release_swap_chain;
				}

				DispatchMessage(&message);
				break;
			}
		}

	exit_peek_loop:
		;
		HRESULT result =
		    IDirect3DSwapChain9_Present(swap_chain, 0, 0, 0, 0, 0);
		if (FAILED(result)) {
			errnum = ENOSYS;
			goto release_swap_chain;
		}
	}

release_swap_chain:
	IDirect3DSwapChain9_Release(swap_chain);
	IDirect3DDevice9Ex_Release(device);
release_context:
	IDirect3D9Ex_Release(context);

destroy_window:
	/* In this case the window has not already been destroyed */
	if (errnum != 0) {
		DestroyWindow(main_window);

		for (;;) {
			MSG message;
			switch (PeekMessage(&message, 0, 0, 0, PM_REMOVE)) {
			case -1:
			case 0:
				goto window_destroyed;

			default:
				DispatchMessage(&message);
				break;
			}
		}
	}

window_destroyed:
report_exit_status:
	/* I'm too lazy to bother with getting and printing the text
	 * for the error code right now. */
	return errnum;
}

static LRESULT on_paint(HWND main_window, UINT message_type, WPARAM w_param,
                        LPARAM l_param);

static LRESULT on_destroy(HWND main_window, UINT message_type, WPARAM w_param,
                          LPARAM l_param);

LRESULT CALLBACK window_procedure(HWND main_window, UINT message_type,
                                  WPARAM w_param, LPARAM l_param)
{
	switch (message_type) {
	case WM_PAINT:
		return on_paint(main_window, message_type, w_param, l_param);

	case WM_DESTROY:
		return on_destroy(main_window, message_type, w_param, l_param);

	default:
		return DefWindowProc(main_window, message_type, w_param,
		                     l_param);
	}
}

static LRESULT on_paint(HWND main_window, UINT message_type, WPARAM w_param,
                        LPARAM l_param)
{
	DWORD errnum = 0;

	PAINTSTRUCT ps;
	HDC hdc = BeginPaint(main_window, &ps);
	if (0 == hdc) {
		errnum = GetLastError();
		assert(errnum != 0);
		goto post_quit_message;
	}

	if (0 == EndPaint(main_window, &ps)) {
		errnum = GetLastError();
		assert(errnum != 0);
	}

post_quit_message:
	if (errnum != 0)
		PostQuitMessage(errnum);

	return 0;
}

static LRESULT on_destroy(HWND main_window, UINT message_type, WPARAM w_param,
                          LPARAM l_param)
{
	PostQuitMessage(0);

	return DefWindowProc(main_window, message_type, w_param, l_param);
}
