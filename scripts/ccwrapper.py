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

# TODO: Add the following checkers

# cppcheck

def go():
    clang = sys.argv[1]
    cc = sys.argv[2]
    options = sys.argv[3:]

    if clang != "" and '-c' in options:
        subprocess.check_call([clang, '-Qunused-arguments', '-Wno-unknown-warning-option', '--analyze'] + options)

    subprocess.check_call([cc] + options)

    sys.exit(0)

if __name__ == '__main__':
    go()
