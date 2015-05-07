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
 *implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#include "config.h"

#if defined HAVE_WINDOWS_API
#include "log-windows.c"
#elif defined HAVE_EMSCRIPTEN_API
#include "log-emscripten.c"
#elif defined HAVE_POSIX_API
#include "log-posix.c"
#else
#error no logging library implementation for this platform
#endif
