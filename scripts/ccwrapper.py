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
import os
import subprocess
import sys

def go():
    host = sys.argv[1]
    clang = sys.argv[2].split()
    cppcheck = sys.argv[3]
    cc = sys.argv[4].split()
    options = sys.argv[5:]

    if '-c' in options:
        filtered_options = options_filter(options)
        define_flags = get_predefined(cc)
        includes = get_includes(cc)

        if clang != "":
            clang_args = []
            clang_args.extend(clang)
            clang_args.extend(['-target', host,
                               '-Qunused-arguments',
                               '-Wno-unknown-warning-option',
                               '--analyze',
                               '-Xclang', '-analyzer-output=text',
                               '-o-',
                               '-nostdinc'])
            for checker in checkers:
                clang_args.extend(['-Xanalyzer', '-analyzer-checker=' + checker])
            clang_args.extend(define_flags)
            clang_args.extend(['-isystem' + xx for xx in includes])
            clang_args.extend(filtered_options)

            exit_status = subprocess.call(clang_args)
            if exit_status != 0:
                sys.exit(exit_status)

        if cppcheck != "":
            cppcheck_args = [cppcheck,
                             '--quiet',
                             '--enable=all',

                             # GCC already does resource leak
                             # detection and besides Cppcheck doesn't
                             # properly handle tricky errno handling
                             # code.
                             '--suppress=resourceLeak',

                             # Having a widely scoped errnum variable
                             # is useful
                             '--suppress=variableScope',

                             # This warning makes working with arrays
                             # of pointers annoying
                             '--suppress=sizeofwithsilentarraypointer',

                             '--template={file}:{line}: ({id}) {message}']

            if 'D__unix__=1' in define_flags:
                cppcheck_args.append('--platform=unix64')

            if 'D__WIN64=1' in define_flags:
                cppcheck_args.append('--platform=win64')
            elif 'D__WIN32=1' in define_flags:
                cppcheck_args.append('--platform=win32W')

            cppcheck_args.extend(define_flags)
            cppcheck_args.extend(filtered_options)

            exit_status = subprocess.call(cppcheck_args)
            if exit_status != 0:
                sys.exit(exit_status)

    exit_status = subprocess.call(cc + options)
    if exit_status != 0:
        sys.exit(exit_status)

    sys.exit(0)

def get_predefined(cc):
    defines = None
    with open(os.devnull) as null:
        defines = subprocess.check_output(cc + ['-dM', '-E', '-'], stdin=null)

    defines = defines.decode('utf-8').split('\n')

    defineflags = []
    for define in defines:
        if '' == define:
            continue
        l = define.split(' ', 2)
        defineflags.append('-D' + l[1] + '=' + l[2])

    return defineflags

def get_includes(cc):
    includes = None
    with open(os.devnull) as null:
        includes = subprocess.check_output(cc + ['-v', '-E', '-'], stdin=null,
                                           stderr=subprocess.STDOUT)

    includes = includes.decode('utf-8')

    start = '#include <...> search starts here:\n'
    end = 'End of search list.'
    includes = includes[includes.find(start) + len(start):includes.find(end) - 1].split('\n')

    includes = [xx[1:] for xx in includes]
    return includes

def options_filter(options):
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
    try:
        go()
    except KeyboardInterrupt:
        sys.exit(1)
