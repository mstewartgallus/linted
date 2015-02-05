# Process this file with automake to produce Makefile.in (in this,
# and all subdirectories).
#
# Copyright 2014, 2015 Steven Stewart-Gallus
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
noinst_LTLIBRARIES+=%D%/libsim.la

%C%_libsim_la_SOURCES=include/linted/sim.h
%C%_libsim_la_LIBADD=$(MATH_LIBS)

TESTS+=%D%/test_sim_angle

check_PROGRAMS+=%D%/test_sim_angle
%C%_test_sim_angle_SOURCES=%D%/test_sim_angle.c
%C%_test_sim_angle_LDADD=\
  src/asynch/libasynch.la\
  src/io/libio.la\
  src/sim/libsim.la
