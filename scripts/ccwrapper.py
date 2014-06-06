#! /usr/bin/env python3.2
# Copyright 2014 Steven Stewart-Gallus
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
import subprocess
import sys

def go():
    clang = sys.argv[1]
    cppcheck = sys.argv[2]
    cc = sys.argv[3]
    options = sys.argv[4:]

    if '-c' in options:
        if clang != "":
            clang_args = [clang,
                          '-Qunused-arguments',
                          '-Wno-unknown-warning-option',
                          '--analyze']
            for checker in checkers:
                clang_args.extend(['-Xanalyzer', '-analyzer-checker=' + checker])
            clang_args.extend(options)

            cppcheck_args = [cppcheck,
                             '--quiet',
                             '--platform=unix64',
                             '--enable=all',
                             '--template=gcc',
                             '-D__linux__']
            cppcheck_args.extend(cppcheck_filter(options))

            exit_status = subprocess.call(cppcheck_args)
            if exit_status != 0:
                sys.exit(exit_status)

            exit_status = subprocess.call(cppcheck_args)
            if exit_status != 0:
                sys.exit(exit_status)

    exit_status = subprocess.call(cc.split() + options)
    if exit_status != 0:
        sys.exit(exit_status)

    sys.exit(0)

def cppcheck_filter(options):
    cppcheck_options = []
    for option in options:
        if ((not option.startswith('-') and option.endswith('.c'))
            or option.startswith('-D')
            or option.startswith('-U')
            or option.startswith('-I')
            or option.startswith('-i')
            or option.startswith('-I')):
            cppcheck_options.append(option)

    return cppcheck_options

checkers = [
    'core.CallAndMessage',
    'core.DivideZero',
    'core.DynamicTypePropagation',
    'core.NonNullParamChecker',
    'core.NullDereference',
    'core.StackAddressEscape',
    'core.UndefinedBinaryOperatorResult',
    'core.VLASize',
    'core.builtin.BuiltinFunctions',
    'core.builtin.NoReturnFunctions',
    'core.uninitialized.ArraySubscript',
    'core.uninitialized.Assign',
    'core.uninitialized.Branch',
    'core.uninitialized.CapturedBlockVariable',
    'core.uninitialized.UndefReturn',

    'deadcode.DeadStores',

    'security.FloatLoopCounter',
    'security.insecureAPI.UncheckedReturn',

    'security.insecureAPI.getpw',
    'security.insecureAPI.gets',
    'security.insecureAPI.mkstemp',
    'security.insecureAPI.mktemp',
    'security.insecureAPI.rand',
    'security.insecureAPI.strcpy',
    'security.insecureAPI.vfork',

    'unix.API',
    'unix.Malloc',
    'unix.MallocSizeof',
    'unix.MismatchedDeallocator',
    'unix.cstring.BadSizeArg',
    'unix.cstring.NullArg']

if __name__ == '__main__':
    go()
