# Process this file with automake to produce Makefile.in (in this,
# and all subdirectories).
#
# Copyright 2013, 2014, 2015 Steven Stewart-Gallus
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
# implied.  See the License for the specific language governing
# permissions and limitations under the License.

EXTRA_DIST+=\
  include/GLES3/gl3.h\
  include/GLES3/gl3platform.h\
  \
  include/EGL/egl.h\
  include/EGL/eglplatform.h\
  \
  include/KHR/khrplatform.h\
  \
  include/portable-stdatomic/stdatomic.h\
  \
  include/fake-stdatomic/stdatomic.h\
  \
  include/lntd/execveat.h\
  include/lntd/rpc.h\
  include/lntd/prctl.h\
  include/lntd/ptrace.h\
  include/lntd/test.h\
  include/lntd/util.h

include %C%/linted/Makefile.am
include %C%/linted-init/Makefile.am
include %C%/linted-monitor/Makefile.am

include %C%/factory/Makefile.am

include %C%/linted-window/Makefile.am
include %C%/linted-audio/Makefile.am
include %C%/linted-simulator/Makefile.am

if WINDOWS_API
else
include %C%/linted-drawer/Makefile.am
include %C%/linted-gui/Makefile.am

include %C%/linted-sandbox/Makefile.am
include %C%/linted-startup/Makefile.am
include %C%/linted-waiter/Makefile.am

include %C%/linted-enter/Makefile.am
include %C%/linted-control/Makefile.am

include %C%/ko-stack/Makefile.am
include %C%/gpu/Makefile.am
include %C%/spawn/Makefile.am
include %C%/unit/Makefile.am

include %C%/admin/Makefile.am
endif

include %C%/assets/Makefile.am
include %C%/async/Makefile.am
include %C%/channel/Makefile.am
include %C%/dir/Makefile.am
include %C%/env/Makefile.am
include %C%/error/Makefile.am
include %C%/file/Makefile.am
include %C%/fifo/Makefile.am
include %C%/io/Makefile.am
include %C%/ko/Makefile.am
include %C%/locale/Makefile.am
include %C%/log/Makefile.am
include %C%/mem/Makefile.am
include %C%/path/Makefile.am
include %C%/proc/Makefile.am
include %C%/sched/Makefile.am
include %C%/signal/Makefile.am
include %C%/stack/Makefile.am
include %C%/start/Makefile.am
include %C%/trigger/Makefile.am
include %C%/str/Makefile.am
include %C%/window/Makefile.am

include %C%/ada/Makefile.am
include %C%/ada-core/Makefile.am
include %C%/ada-libc/Makefile.am
include %C%/ada-pulse/Makefile.am
include %C%/ada-xcb/Makefile.am
include %C%/ada-xkb/Makefile.am
include %C%/nesc/Makefile.am

if WINDOWS_API
include %C%/utf/Makefile.am
endif

if X11_API
include %C%/xcb/Makefile.am
endif

EXTRA_DIST+=%C%/spark_lemmas
