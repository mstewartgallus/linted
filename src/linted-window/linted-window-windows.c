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
#define _WIN32_WINNT 0x0600

#ifndef UNICODE
#define UNICODE
#endif

#define _UNICODE

#define WIN32_LEAN_AND_MEAN

#define COBJMACROS

#include "config.h"

#include "lntd/start.h"

#include "lntd/error.h"
#include "lntd/log.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <windows.h>
#include <d3d9.h>

typedef WINAPI IDirect3D9 *typeof_Direct3DCreate9(UINT SDKVersion);

static GUID const uuidof_IDXGIDevice = {
    0x54ec77fa,
    0x1377,
    0x44e6,
    {0x8c, 0x32, 0x88, 0xfd, 0x5f, 0x44, 0xc8, 0x4c}};
static GUID const uuidof_IDXGIFactory1 = {
    0x770aae78,
    0xf26f,
    0x4dba,
    {0xa8, 0x29, 0x25, 0x3c, 0x83, 0xd1, 0xb3, 0x87}};

static HINSTANCE get_current_module(void)
{
	static char const dummy;

	HINSTANCE xx;
	GetModuleHandleEx(
	    GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS |
	        GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,
	    (void *)&dummy, &xx);
	return xx;
}

static LRESULT CALLBACK window_procedure(HWND, UINT, WPARAM, LPARAM);

static struct lntd_start_config const lntd_start_config = {
    .canonical_process_name = PACKAGE_NAME "-window", 0};

static unsigned char lntd_start_main(char const *process_name,
                                     size_t argc,
                                     char const *const argv[])
{
	lntd_error err = 0;

	HGDIOBJ arrow_cursor = LoadCursor(0, IDC_ARROW);
	if (0 == arrow_cursor) {
		err = HRESULT_FROM_WIN32(GetLastError());
		LNTD_ASSERT(err != 0);
		goto report_exit_status;
	}

	HGDIOBJ white_brush = GetStockObject(WHITE_BRUSH);
	if (0 == white_brush) {
		err = HRESULT_FROM_WIN32(GetLastError());
		LNTD_ASSERT(err != 0);
		goto report_exit_status;
	}

	ATOM class_atom;
	{
		WNDCLASS window_class = {0};

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
		err = HRESULT_FROM_WIN32(GetLastError());
		LNTD_ASSERT(err != 0);
		goto report_exit_status;
	}

	HWND main_window = CreateWindowEx(
	    WS_EX_APPWINDOW | WS_EX_COMPOSITED,
	    (LPCTSTR)(uintptr_t)class_atom, L"" PACKAGE_NAME,
	    WS_OVERLAPPED | WS_CAPTION | WS_THICKFRAME |
	        WS_MINIMIZEBOX | WS_MAXIMIZEBOX,
	    CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
	    0, 0, get_current_module(), 0);
	if (0 == main_window) {
		err = HRESULT_FROM_WIN32(GetLastError());
		LNTD_ASSERT(err != 0);
		goto destroy_window;
	}

	switch (ShowWindow(main_window, lntd_start_show_command())) {
	case -1: {
		err = HRESULT_FROM_WIN32(GetLastError());
		LNTD_ASSERT(err != 0);
		goto destroy_window;
	}

	case 0:
	default:
		break;
	}

	HMODULE d3d9_module = LoadLibraryW(L"d3d9.dll");
	if (0 == d3d9_module) {
		lntd_log(LNTD_LOG_ERROR, "Could not find d3d9.dll");
		return EXIT_FAILURE;
	}

	SetLastError(0);
	typeof_Direct3DCreate9 *my_Direct3dCreate9 =
	    (typeof_Direct3DCreate9 *)GetProcAddress(d3d9_module,
	                                             "Direct3DCreate9");
	err = HRESULT_FROM_WIN32(GetLastError());
	if (err != 0) {
		lntd_log(LNTD_LOG_ERROR, "GetProcAddress: %s",
		         lntd_error_string(err));
		return EXIT_FAILURE;
	}

	IDirect3D9 *direct3d = my_Direct3dCreate9(D3D_SDK_VERSION);
	if (0 == direct3d) {
		lntd_log(LNTD_LOG_ERROR, "Direct3dCreate9");
		err = LNTD_ERROR_UNIMPLEMENTED;
		goto destroy_window;
	}

	IDirect3DDevice9 *device;
	{
		D3DPRESENT_PARAMETERS xx = {0};
		IDirect3DDevice9 *yy;

		xx.BackBufferFormat = D3DFMT_UNKNOWN;
		xx.MultiSampleType = D3DMULTISAMPLE_NONE;
		xx.SwapEffect = D3DSWAPEFFECT_FLIP;
		xx.hDeviceWindow = main_window;
		xx.Windowed = true;
		xx.EnableAutoDepthStencil = true;
		xx.AutoDepthStencilFormat = D3DFMT_R8G8B8;
		xx.PresentationInterval = D3DPRESENT_INTERVAL_DEFAULT;

		err = IDirect3D9_CreateDevice(
		    direct3d, D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL,
		    main_window, D3DCREATE_SOFTWARE_VERTEXPROCESSING,
		    &xx, &yy);
		if (FAILED(err))
			goto destroy_direct3d;
		device = yy;
	}

	if (0 == UpdateWindow(main_window)) {
		err = HRESULT_FROM_WIN32(GetLastError());
		LNTD_ASSERT(err != 0);
		goto destroy_device;
	}

	for (;;) {
		for (;;) {
			MSG message;
			switch (
			    PeekMessage(&message, 0, 0, 0, PM_REMOVE)) {
			case -1:
				return -1;

			case 0:
				goto exit_peek_loop;

			default:
				if (WM_QUIT == message.message) {
					err = message.wParam;
					goto destroy_device;
				}

				if (-1 == TranslateMessage(&message)) {
					err = HRESULT_FROM_WIN32(
					    GetLastError());
					goto destroy_device;
				}

				DispatchMessage(&message);
				break;
			}
		}

	exit_peek_loop:
		err = IDirect3DDevice9_Clear(
		    device, 0, 0, D3DCLEAR_TARGET | D3DCLEAR_ZBUFFER,
		    D3DCOLOR_ARGB(255, 0, 255, 255), 0, 0);
		if (FAILED(err))
			goto destroy_device;

		err = IDirect3DDevice9_Present(device, 0, 0,
		                               main_window, 0);
		if (FAILED(err))
			goto destroy_device;
	}

destroy_device:
	IDirect3DDevice9_Release(device);

destroy_direct3d:
	IDirect3D9_Release(direct3d);

destroy_window:
	/* In this case the window has not already been destroyed */
	if (err != 0) {
		DestroyWindow(main_window);

		for (;;) {
			MSG message;
			switch (
			    PeekMessage(&message, 0, 0, 0, PM_REMOVE)) {
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
	if (FAILED(err))
		lntd_log(LNTD_LOG_ERROR, "%s", lntd_error_string(err));
	return err;
}

static LRESULT on_paint(HWND main_window, UINT message_type,
                        WPARAM w_param, LPARAM l_param);

static LRESULT on_destroy(HWND main_window, UINT message_type,
                          WPARAM w_param, LPARAM l_param);

static LRESULT CALLBACK window_procedure(HWND main_window,
                                         UINT message_type,
                                         WPARAM w_param, LPARAM l_param)
{
	switch (message_type) {
	case WM_PAINT:
		return on_paint(main_window, message_type, w_param,
		                l_param);

	case WM_DESTROY:
		return on_destroy(main_window, message_type, w_param,
		                  l_param);

	default:
		return DefWindowProc(main_window, message_type, w_param,
		                     l_param);
	}
}

static LRESULT on_paint(HWND main_window, UINT message_type,
                        WPARAM w_param, LPARAM l_param)
{
	DWORD err = 0;

	PAINTSTRUCT ps;
	HDC hdc = BeginPaint(main_window, &ps);
	if (0 == hdc) {
		err = HRESULT_FROM_WIN32(GetLastError());
		LNTD_ASSERT(err != 0);
		goto post_quit_message;
	}

	if (0 == EndPaint(main_window, &ps)) {
		err = HRESULT_FROM_WIN32(GetLastError());
		LNTD_ASSERT(err != 0);
	}

post_quit_message:
	if (err != 0)
		PostQuitMessage(err);

	return 0;
}

static LRESULT on_destroy(HWND main_window, UINT message_type,
                          WPARAM w_param, LPARAM l_param)
{
	PostQuitMessage(0);

	return DefWindowProc(main_window, message_type, w_param,
	                     l_param);
}
