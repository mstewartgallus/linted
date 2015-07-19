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

#include "linted/start.h"

#include "linted/error.h"
#include "linted/log.h"

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <windows.h>
#include <d3d11.h>

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

LRESULT CALLBACK window_procedure(HWND, UINT, WPARAM, LPARAM);

static unsigned char window_start(char const *process_name, size_t argc,
                                  char const *const argv[]);

struct linted_start_config const linted_start_config = {
    .canonical_process_name = PACKAGE_NAME "-window",
    .start = window_start};

static unsigned char window_start(char const *process_name, size_t argc,
                                  char const *const argv[])
{
	linted_error err = 0;

	HGDIOBJ arrow_cursor = LoadCursor(0, IDC_ARROW);
	if (0 == arrow_cursor) {
		err = GetLastError();
		assert(err != 0);
		goto report_exit_status;
	}

	HGDIOBJ white_brush = GetStockObject(WHITE_BRUSH);
	if (0 == white_brush) {
		err = GetLastError();
		assert(err != 0);
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
		err = GetLastError();
		assert(err != 0);
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
		err = GetLastError();
		assert(err != 0);
		goto destroy_window;
	}

	switch (ShowWindow(main_window, linted_start_show_command())) {
	case -1: {
		err = GetLastError();
		assert(err != 0);
		goto destroy_window;
	}

	case 0:
	default:
		break;
	}

#if 0
	static D3D_FEATURE_LEVEL const feature_levels[] = {
	    D3D_FEATURE_LEVEL_11_0, D3D_FEATURE_LEVEL_10_1,
	    D3D_FEATURE_LEVEL_10_0,
	};

	unsigned device_flags = 0U;
	ID3D11Device *device;
	ID3D11DeviceContext *device_context;
	{
		ID3D11Device *xx;
		D3D_FEATURE_LEVEL yy;
		ID3D11DeviceContext *zz;
		HRESULT result = D3D11CreateDevice(
		    0, D3D_DRIVER_TYPE_HARDWARE, 0, device_flags,
		    feature_levels, LINTED_ARRAY_SIZE(feature_levels),
		    D3D11_SDK_VERSION, &xx, &yy, &zz);
		if (FAILED(result)) {
			err = LINTED_ERROR_UNIMPLEMENTED;
			goto destroy_window;
		}
		device = xx;
		device_context = zz;
	}

	IDXGIFactory1 *dxgi_factory;
	{
		IDXGIDevice *dxgi_device;
		{
			void *xx;
			HRESULT result = ID3D11Device_QueryInterface(
			    device, &uuidof_IDXGIDevice, &xx);
			if (FAILED(result)) {
				err = LINTED_ERROR_UNIMPLEMENTED;
				goto destroy_device;
			}
			dxgi_device = xx;
		}

		IDXGIAdapter *adapter;
		{
			IDXGIAdapter *xx;
			HRESULT result =
			    IDXGIDevice_GetAdapter(dxgi_device, &xx);
			if (FAILED(result)) {
				err = LINTED_ERROR_UNIMPLEMENTED;
				goto destroy_device;
			}
			adapter = xx;
		}

		{
			void *xx;
			HRESULT result = IDXGIAdapter_GetParent(
			    adapter, &uuidof_IDXGIFactory1, &xx);
			if (FAILED(result)) {
				err = LINTED_ERROR_UNIMPLEMENTED;
				goto destroy_device;
			}
			dxgi_factory = xx;
		}
		IDXGIAdapter_Release(adapter);
		IDXGIDevice_Release(dxgi_device);
	}

	IDXGISwapChain *swap_chain;
	{
		DXGI_SWAP_CHAIN_DESC desc = {0};
		desc.BufferCount = 1;
		desc.BufferDesc.Width = 2048U;
		desc.BufferDesc.Height = 2048U;
		desc.BufferDesc.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
		desc.BufferDesc.RefreshRate.Numerator = 60;
		desc.BufferDesc.RefreshRate.Denominator = 1;
		desc.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;
		desc.OutputWindow = main_window;
		desc.SampleDesc.Count = 1;
		desc.SampleDesc.Quality = 0;
		desc.Windowed = TRUE;

		IDXGISwapChain *xx;
		HRESULT result = IDXGIFactory1_CreateSwapChain(
		    dxgi_factory, (IUnknown *)device, &desc, &xx);
		if (FAILED(result)) {
			err = LINTED_ERROR_UNIMPLEMENTED;
			goto destroy_device;
		}
		swap_chain = xx;
	}

	IDXGIFactory1_MakeWindowAssociation(dxgi_factory, main_window,
	                                    DXGI_MWA_NO_ALT_ENTER);

	IDXGIFactory1_Release(dxgi_factory);
#endif
	if (0 == UpdateWindow(main_window)) {
		err = GetLastError();
		assert(err != 0);
		goto release_swap_chain;
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
					goto release_swap_chain;
				}

				if (-1 == TranslateMessage(&message)) {
					err = GetLastError();
					goto release_swap_chain;
				}

				DispatchMessage(&message);
				break;
			}
		}

	exit_peek_loop:
		;
#if 0
		HRESULT result =
		    IDXGISwapChain_Present(swap_chain, 0, 0);
		if (FAILED(result)) {
			err = LINTED_ERROR_UNIMPLEMENTED;
			goto release_swap_chain;
		}
#endif
	}

release_swap_chain:
#if 0
	IDXGISwapChain_Release(swap_chain);

destroy_device:
	ID3D11Device_Release(device);
	ID3D11DeviceContext_Release(device_context);
#endif

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
	/* I'm too lazy to bother with getting and printing the text
	 * for the error code right now. */
	return err;
}

static LRESULT on_paint(HWND main_window, UINT message_type,
                        WPARAM w_param, LPARAM l_param);

static LRESULT on_destroy(HWND main_window, UINT message_type,
                          WPARAM w_param, LPARAM l_param);

LRESULT CALLBACK window_procedure(HWND main_window, UINT message_type,
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
		err = GetLastError();
		assert(err != 0);
		goto post_quit_message;
	}

	if (0 == EndPaint(main_window, &ps)) {
		err = GetLastError();
		assert(err != 0);
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
