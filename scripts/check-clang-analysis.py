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
import subprocess
import json

def go():
    parser = argparse.ArgumentParser(
        description = 'Analysis a tree of sources')
    parser.add_argument(
        'input_files',
        type = str,
	nargs = '+',
        help = 'The recursive file of sources')

    arguments = parser.parse_args()

    filenames = arguments.input_files

    files_json = []

    for filename in filenames:
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

    files = []
    def recurse(jsondata):
        for afile in jsondata['files']:
            if isinstance(afile, str):
                flags = jsondata['flags']
                files.append({'file': afile, 'flags': flags})
            elif isinstance(afile, dict):
                recurse(afile)
            else:
                raise Exception('type error: ' + str(type(afile)))
    for data in files_json:
        recurse(data)

    files_json = None

    files = unique(files)

    for afileandflags in files:
        afile = afileandflags['file']
        flags = afileandflags['flags']

        arguments = ['clang-3.9',
                     '-Xclang', '-analyzer-output=text',
                     '-Xclang', '-o-',
                     '--analyze',
                     '-w',
                     '-Qunused-arguments',
                     '-Wno-unknown-warning-option'
                     ]

        arguments.extend(CHECKERS)
        arguments.extend(flags)
        arguments.append('--')
        arguments.append(afile)
        subprocess.call(arguments)

def unique(l):
    newl = []
    for x in l:
        if x not in newl:
            newl.append(x)
    return newl

DISABLED = [
	# Sadly we need to use vfork sometimes
	# Warn on uses of the 'vfork' function
	 'security.insecureAPI.vfork',

	# Check unreachable code
	# Too many false positives on code of the form

	# #define LINTED_ASSUME_UNREACHABLE() \
	# do {                                \
	#         extern void abort(void);    \
	#         abort();                    \
	# } while (0)

	# for the while (0)

	 'alpha.deadcode.UnreachableCode'
    ]

ENABLED = [
	# Check for cast from non-struct pointer to struct pointer
	 'alpha.core.CastToStruct',

	# Warn about assigning non-{0,1} values to Boolean variables
	 'alpha.core.BoolAssignment',

	# Check for logical errors for function calls and Objective-C
	# message expressions (e.g., uninitialized arguments, null
	# function pointers, and pointer to undefined variables)
	 'alpha.core.CallAndMessageUnInitRefArg',

	# Check when casting a malloc'ed type T, whether the size is a
	# multiple of the size of T
	 'alpha.core.CastSize',

	# Check for assignment of a fixed address to a pointer
	 'alpha.core.FixedAddr',
	# Warn about unintended use of identical expressions in operators
	 'alpha.core.IdenticalExpr',
	# Check for pointer arithmetic on locations other than array elements
	 'alpha.core.PointerArithm',
	# Check for pointer subtractions on two pointers pointing to different
	# memory chunks
	 'alpha.core.PointerSub',
	# Warn about unintended use of sizeof() on pointer expressions
	 'alpha.core.SizeofPtr',

	# Check for division by variable that is later compared against
	# 0. Either the comparison is useless or there is division by
	# zero.
	 'alpha.core.TestAfterDivZero',

	# Check virtual function calls during construction or destruction
	 'alpha.cplusplus.VirtualCall',

	# Warn about Objective-C classes that lack a correct implementation of
	# -dealloc
	# 'alpha.osx.cocoa.Dealloc',
	# Check for direct assignments to instance variables
	 'alpha.osx.cocoa.DirectIvarAssignment',
	# Check for direct assignments to instance variables in the methods
	# annotated with objc_no_direct_instance_variable_assignment
	 'alpha.osx.cocoa.DirectIvarAssignmentForAnnotatedFunctions',
	# Check that the invalidatable instance variables are invalidated in
	# the methods annotated with objc_instance_variable_invalidator
	 'alpha.osx.cocoa.InstanceVariableInvalidation',
	# Check that the invalidation methods are present in classes that
	# contain invalidatable instance variables
	 'alpha.osx.cocoa.MissingInvalidationMethod',

	# Warn about buffer overflows (older checker)
	 'alpha.security.ArrayBound',
	# Warn about buffer overflows (newer checker)
	 'alpha.security.ArrayBoundV2',
	# Check for overflows in the arguments to malloc()
	 'alpha.security.MallocOverflow',
	# Check for an out-of-bound pointer being returned to callers
	 'alpha.security.ReturnPtrRange',
	# Generate taint information used by other checkers
	 'alpha.security.taint.TaintPropagation',

	# Check improper use of chroot
	 'alpha.unix.Chroot',
	# Check for memory leaks, double free, and use-after-free
	# problems. Traces memory managed by malloc()/free(). Assumes that all
	# user-defined functions which might free a pointer are annotated.
	#'alpha.unix.MallocWithAnnotations',
	# Simple lock -> unlock checker
	 'alpha.unix.PthreadLock',
	# Check for misuses of stream APIs
	 'alpha.unix.SimpleStream',
	# Check stream handling functions
	 'alpha.unix.Stream',
	# Checks for overlap in two buffer arguments
	 'alpha.unix.cstring.BufferOverlap',
	# Check for arguments which are not null-terminating strings
	 'alpha.unix.cstring.NotNullTerminated',
	# Check for out-of-bounds access in string functions
	 'alpha.unix.cstring.OutOfBounds',

	# Check for logical errors for function calls and Objective-C message
	# expressions (e.g., uninitialized arguments, null function pointers)
	 'core.CallAndMessage',
	# Check for division by zero
	 'core.DivideZero',
	# Generate dynamic type information
	 'core.DynamicTypePropagation',
	# Check for null pointers passed as arguments to a function whose
	# arguments are references or marked with the 'nonnull' attribute
	 'core.NonNullParamChecker',
	# Check for dereferences of null pointers
	 'core.NullDereference',
	# Check that addresses to stack memory do not escape the function
	 'core.StackAddressEscape',
	# Check for undefined results of binary operators
	 'core.UndefinedBinaryOperatorResult',
	# Check for declarations of VLA of undefined or zero size
	 'core.VLASize',
	# Evaluate compiler builtin functions (e.g., alloca())
	 'core.builtin.BuiltinFunctions',
	# Evaluate panic functions that are known to not return to the caller
	 'core.builtin.NoReturnFunctions',
	# Check for uninitialized values used as array subscripts
	 'core.uninitialized.ArraySubscript',
	# Check for assigning uninitialized values
	 'core.uninitialized.Assign',
	# Check for uninitialized values used as branch conditions
	 'core.uninitialized.Branch',
	# Check for blocks that capture uninitialized values
	 'core.uninitialized.CapturedBlockVariable',
	# Check for uninitialized values being returned to the caller
	 'core.uninitialized.UndefReturn',

	# Check for double-free and use-after-free problems. Traces memory
	# managed by new/delete.
	 'cplusplus.NewDelete',

	# Check for memory leaks. Traces memory managed by new/delete.
	 'cplusplus.NewDeleteLeaks',

	# Check for values stored to variables that are never read
	# afterwards
	 'deadcode.DeadStores',

	# Check code for LLVM codebase conventions
	 'llvm.Conventions',

	 'nullability.NullPassedToNonnull',
	 'nullability.NullReturnedFromNonnull',

	 'optin.performance.Padding',

	# Check for proper uses of various Apple APIs
	 'osx.API',
	# Check for proper uses of Secure Keychain APIs
	 'osx.SecKeychainAPI',
	# Check for nil pointers used as mutexes for @synchronized
	 'osx.cocoa.AtSync',
	# Check for sending 'retain', 'release', or 'autorelease' directly to a Class
	 'osx.cocoa.ClassRelease',
	# Warn about Objective-C method signatures with type incompatibilities
	 'osx.cocoa.IncompatibleMethodTypes',
	# Improved modeling of loops using Cocoa collection types
	 'osx.cocoa.Loops',
	# Warn about Objective-C methods that lack a necessary call to super
	 'osx.cocoa.MissingSuperCall',
	# Warn for suboptimal uses of NSAutoreleasePool in Objective-C GC mode
	 'osx.cocoa.NSAutoreleasePool',
	# Check usage of NSError** parameters
	 'osx.cocoa.NSError',
	# Check for prohibited nil arguments to ObjC method calls
	 'osx.cocoa.NilArg',
	# Model the APIs that are guaranteed to return a non-nil value
	 'osx.cocoa.NonNilReturnValue',
	# Check for leaks and improper reference count managementModel the
	# APIs that are guaranteed to return a non-nil value
	 'osx.cocoa.RetainCount',
	# Check that 'self' is properly initialized inside an initializer method
	 'osx.cocoa.SelfInit',
	# Warn about private ivars that are never used
	 'osx.cocoa.UnusedIvars',
	# Check for passing non-Objective-C types to variadic collection
	# initialization methods that expect only Objective-C types
	 'osx.cocoa.VariadicMethodTypes',
	# Check usage of CFErrorRef* parameters
	 'osx.coreFoundation.CFError',
	# Check for proper uses of CFNumberCreate
	 'osx.coreFoundation.CFNumber',
	# Check for null arguments to CFRetain/CFRelease/CFMakeCollectable
	 'osx.coreFoundation.CFRetainRelease',
	# Checks for index out-of-bounds when using 'CFArray' API
	 'osx.coreFoundation.containers.OutOfBounds',
	# Warns if 'CFArray', 'CFDictionary', 'CFSet' are created with
	# non-pointer-size values
	 'osx.coreFoundation.containers.PointerSizedValues',

	# Warn on using a floating point value as a loop counter (CERT:
	# FLP30-C, FLP30-CPP)
	 'security.FloatLoopCounter',

	# Warn on uses of functions whose return values must be always checked
	 'security.insecureAPI.UncheckedReturn',
	# Warn on uses of the 'getpw' function
	 'security.insecureAPI.getpw',
	# Warn on uses of the 'gets' function
	 'security.insecureAPI.gets',
	# Warn when 'mkstemp' is passed fewer than 6 X's in the format string
	 'security.insecureAPI.mkstemp',
	# Warn on uses of the 'mktemp' function
	 'security.insecureAPI.mktemp',
	# Warn on uses of the 'rand', 'random', and related functions
	 'security.insecureAPI.rand',
	# Warn on uses of the 'strcpy' and 'strcat' functions
	 'security.insecureAPI.strcpy',


	# Check calls to various UNIX/Posix functions
	 'unix.API',
	# Check for memory leaks, double free, and use-after-free
	# problems. Traces memory managed by malloc()/free().
	 'unix.Malloc',
	# Check for dubious malloc arguments involving sizeof
	 'unix.MallocSizeof',
	# Check for mismatched deallocators.
	 'unix.MismatchedDeallocator',
	# Check the size argument passed into C string functions for common
	# erroneous patterns
	 'unix.cstring.BadSizeArg',
	# Check for null pointers being passed as arguments to C string functions
	 'unix.cstring.NullArg',
]

CHECKERS = []
for ii in ENABLED:
    CHECKERS.append('-Xanalyzer')
    CHECKERS.append('-analyzer-checker=' + ii)
for ii in DISABLED:
    CHECKERS.append('-Xanalyzer')
    CHECKERS.append('-analyzer-disable-checker=' + ii)

if __name__ == '__main__':
    go()
