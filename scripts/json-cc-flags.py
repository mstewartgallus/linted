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
import json
import subprocess
import sys

def go():
    parser = argparse.ArgumentParser(
        description = 'Unpack file contents and give them to the command')
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
        with open(filename, 'r') as the_file:
            inputdata = the_file.read()
        files_json.append(json.loads(inputdata))

    flags = []

    def recurse(jsondata):
        for afile in jsondata['files']:
            if isinstance(afile, str):
                # We care about C preprocessor flags only
                fileflags = jsondata['flags']
                for flag in fileflags:
                    if (
                            flag == '-E'
                            or flag == '-C'
                            or flag.startswith('-I')
                            or flag.startswith('-i')
                            or flag.startswith('-D')
                            or flag.startswith('-U')
                    ):
                        flags.append(flag)
            elif isinstance(afile, dict):
                recurse(afile)
            else:
                raise Exception('type error')

    for data in files_json:
        recurse(data)

    print('\n'.join([ii for ii in unique(flags)]))

def unique(l):
    newl = []
    for x in l:
        if x not in newl:
            newl.append(x)
    return newl

if __name__ == '__main__':
    go()
