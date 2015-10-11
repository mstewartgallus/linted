#! /usr/bin/env python3.3
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
import argparse
import os
import subprocess
import json

def go():
    parser = argparse.ArgumentParser(
        description = 'Analysis a tree of sources')
    parser.add_argument(
        'input_file',
        type = str,
        help = 'The recursive file of sources')

    arguments = parser.parse_args()

    filename = arguments.input_file

    files_json = []

    if filename.endswith('.a'):
        members = subprocess.check_output(['ar', 't', filename]).decode('utf-8').split()
        for member in members:
            member_contents = subprocess.check_output(['ar', 'p', filename, member])
            files_json.append(json.loads(member_contents.decode('utf-8')))
    else:
        inputdata = None
        with open(arguments.input_file, 'r') as the_file:
            inputdata = the_file.read()

        files_json.append(json.loads(inputdata))

    def recurse(jsondata):
        for afile in jsondata['files']:
            if isinstance(afile, str):
                flags = jsondata['flags']

                arguments = ['cppcheck',
			     '-j8',
                             '-D__linux__',
                             '-D__unix__',
                             '-D__amd64__',
                             '-I/usr/include',
                             '-I/usr/include/x86_64-linux-gnu',
                             '-I/usr/lib/clang/3.6/include',
                             '--platform=unix64',
                             '--std=c99',
                             '--std=posix',
                             '--template={file}:{line}: ({id}) {message}',
                             '--quiet',
	                     '--library=' + os.path.dirname(__file__) + '/cppcheck.cfg'
                             ]

                checkers = ['--enable=warning',
                            '--enable=style',
                            '--enable=performance',
                            '--enable=portability',
                            '--enable=information',
                            '--enable=missingInclude',
                            '--suppress=nonreentrantFunctionscrypt',
                            '--suppress=nonreentrantFunctionsctermid',
                            '--suppress=nonreentrantFunctionsecvt',
                            '--suppress=nonreentrantFunctionsfcvt',
                            '--suppress=nonreentrantFunctionsgcvt',
                            '--suppress=nonreentrantFunctionsgetlogin',
                            '--suppress=nonreentrantFunctionsgmtime',
                            '--suppress=nonreentrantFunctionslocaltime',
                            '--suppress=nonreentrantFunctionsreaddir',
                            '--suppress=nonreentrantFunctionsstrtok',
                            '--suppress=nonreentrantFunctionstempnam',
                            '--suppress=nonreentrantFunctionsttyname',
                            '--suppress=duplicateExpression',
                            '--suppress=unmatchedSuppression',
                            '--suppress=obseleteFunctionsvfork']

                arguments.extend(checkers)
                arguments.extend([flag for flag in flags if flag != '-std=c99' and flag != '-c'])
                arguments.append(afile)

                subprocess.call(arguments)
            elif isinstance(afile, dict):
                recurse(afile)
            else:
                raise Exception('type error: ' + str(type(afile)))

    for data in files_json:
        recurse(data)

if __name__ == '__main__':
    go()
