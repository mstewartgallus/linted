#! /usr/bin/env python3.3
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
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
# implied.  See the License for the specific language governing
# permissions and limitations under the License.
import os
import subprocess
import sys
import json
from collections import namedtuple

def go():
    arguments = sys.argv

    _, cc, *argument_tail = arguments
    files, flags, makeflags, linking, output = parse(argument_tail)

    if len(makeflags) > 0:
        preprocessor_command = [cc, '-E']

        output_args = []
        if output != None:
            output_args = ['-o', output]

        link_args = []
        if linking:
            link_args = ['-c']

        preprocessor_command.extend(output_args)
        preprocessor_command.extend(makeflags)
        preprocessor_command.extend(link_args)
        preprocessor_command.extend(flags)
        preprocessor_command.extend(files)

        subprocess.check_call(preprocessor_command)
        # Fall through

    if '-dM' in flags or '-E' in makeflags:
        return

    if 0 == len(files):
        raise Exception('Some files are needed!')

    if linking:
        if None == output:
            output = 'a.out'

        files_json = []
        for filename in files:
            if filename.endswith('.c'):
                files_json.append(filename)
            elif filename.endswith('.a'):
                members = subprocess.check_output(['ar', 't', filename]).decode('utf-8').split()
                for member in members:
                    member_contents = subprocess.check_output(['ar', 'p', filename, member])
                    files_json.append(json.loads(member_contents.decode('utf-8')))
            else:
                with open(filename, 'r') as afile:
                    files_json.append(json.loads(afile.read()))

        with open(output, 'w') as outputfile:
            outputfile.write(json.JSONEncoder().encode({
                'flags': flags,
                'files': files_json
            }))

        return

    if 1 == len(files):
        input_file = files[0]

        if output == None:
            output = input_file.replace('.c', '.o')

        exit_status = subprocess.call([cc, '-std=gnu99'] + flags + [input_file, '-o/dev/null'])
        if exit_status != 0:
            sys.exit(exit_status)

        with open(output, 'w') as outputfile:
            outputfile.write(json.dumps({
                'flags': flags,
                'files': [input_file]
            }))
        return

    if output == None:
        raise Exception('cannot specify -o with -c with multiple files')

    for input_file in files:
        with open(input_file.replace('.c', '.o'), 'w') as outputfile:
            outputfile.write(json.dumps({
                'flags': flags,
                'files': [input_file]
            }))

Arguments = namedtuple('Arguments',
                       ('files', 'flags', 'makeflags', 'linking', 'output'))
def parse(arguments: list):
    ii = 0

    files = []
    output = None
    linking = True
    flags = []
    makeflags = []
    while True:
        if ii == len(arguments):
            break

        argument = arguments[ii]

        if argument == '-c':
            linking = False
            flags.append(argument)
        elif argument == '-o':
            ii += 1

            if ii == len(arguments):
                raise Exception('No argument for o given')

            output = arguments[ii]
        elif argument == '-MF' or argument == '-MT':
            makeflags.append(argument)

            ii += 1

            if ii == len(arguments):
                raise Exception('No argument for -MF or -MT given')

            makeflags.append(arguments[ii])
        elif argument.startswith('-M') or argument == '-E' or argument == '-C':
            makeflags.append(argument)
        elif (argument.startswith('-d')
              or argument.startswith('-D')
              or argument.startswith('-v')
              or argument.startswith('-U')
              or argument.startswith('-I')
              or argument.startswith('-i')
              or argument.startswith('-I')
              or argument.startswith('-std=')):
            flags.append(argument)
        elif not argument.startswith('-') and (argument.endswith('.c')
                                               or argument.endswith('.o')
                                               or argument.endswith('.a')):
            files.append(argument)
        elif argument == '-':
            files.append('-')

        ii += 1
    return Arguments(files=files,
                     flags=flags,
                     makeflags=makeflags,
                     linking=linking,
                     output=output)

if __name__ == '__main__':
    go()
