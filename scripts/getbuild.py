#! /usr/bin/env python3
# Copyright (C) 2015 Steven Stewart-Gallus
#
# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.
import argparse
import os
import subprocess
import sys

def go(arguments):
	os.environ['POSIXLY_CORRECT'] = '1'
	os.environ['LC_ALL'] = 'C.UTF-8'

	program_name = os.path.basename(arguments[0])

	parser = argparse.ArgumentParser(
	        prog = program_name,
	        description = 'Build a final binary for binary distributions of Linted')
	parser.add_argument(
	        'COMMAND',
	        type = str,
	        help = 'command to do')
	parser.add_argument(
	        'COMMANDARG',
	        type = str,
		nargs = '+',
	        help = 'command arguments')

	parsed = parser.parse_args(arguments[1:])
	parser = None

	task_name = parsed.COMMAND
	task_args = parsed.COMMANDARG
	parsed = None

	task = TASKS.get(task_name)
	if None == task:
		print(task_name)
		return 1

	return task(program_name, task_args)

def source(program_name, arguments):
	parser = argparse.ArgumentParser(
	        prog = program_name + ' fetch-source',
	        description = 'Build a final binary for binary distributions of Linted')
	parser.add_argument(
	        'SRCDIR',
	        type = str,
	        help = 'the source directory')

	parsed = parser.parse_args(arguments)
	parser = None

	srcdir = parsed.SRCDIR
	parsed = None

	lintedsrcdir = os.path.join(srcdir, 'linted')

	subprocess.check_call(['git', 'clone',
			       '--quiet',
			       '--depth=1',
			       '--branch=master',
			       '--',
			       'git@gitlab.com:linted/linted.git', lintedsrcdir])
	subprocess.check_call(['autoreconf', '-ivf', '-Wall', lintedsrcdir])
	return 0

def deps(program_name, arguments):
	parser = argparse.ArgumentParser(
	        prog = program_name + ' fetch-dependencies',
	        description = 'Build a final binary for binary distributions of Linted')
	parser.add_argument(
	        'CONFIG',
	        type = str,
	        help = 'command to do')
	parser.add_argument(
	        'DEPDIR',
	        type = str,
	        help = 'command arguments')

	parsed = parser.parse_args(arguments)
	parser = None

	config = parsed.CONFIG
	depdir = parsed.DEPDIR
	parsed = None

	deplist = getdeps(config)

	path = ''

	for folder in [depdir, 'var', 'cache', 'builddeps']:
		path = os.path.join(path, folder)
		try:
			os.mkdir(path)
		except FileExistsError:
			pass

	subprocess.check_call(['apt-get', 'download', '--quiet', '--'] + deplist,
			cwd = os.path.join(depdir, 'var', 'cache', 'builddeps'))

	for entry in os.listdir(os.path.join(depdir, 'var', 'cache', 'builddeps')):
		if not (entry.endswith('.deb')):
			continue
		fullentry = os.path.join(depdir, 'var', 'cache', 'builddeps', entry)
		subprocess.check_call(['dpkg-deb', '-x', '--', fullentry, depdir])

	return 0

def getdeps(config):
	arch, os, abi = {
		'x86_64-w64-mingw32': ('amd64', 'w64', 'mingw32')
	}[config]
	triplet = arch + '-' + os + '-' + abi

	deplist = [
		# This is an ugly hack to get autoconf working
		'libc6',
		'libc6-dev',

		'x11proto-core-dev',
		'x11proto-damage-dev',
		'x11proto-fixes-dev',
		'x11proto-kb-dev',
		'x11proto-xext-dev',
		'x11proto-xf86vidmode-dev'
		]

	if 'dietlibc' == abi:
		deplist.append('dietlibc-dev')

	if triplet in ['aarch64-linux-gnu',
		'arm-linux-androideabi',
		'arm-linux-gnueabi',
		'arm-linux-gnueabihf',
		'i686-linux-android',
		'powerpc64le-linux-gnu',
		'powerpc-linux-gnu']:
		deplist.append('gcc-' + triplet)

	deplist.extend({
		'arm-linux-gnueabihf': lambda: ['libc6-dev-armhf-cross'],
		'powerpc-linux-gnu': lambda: ['libc6-dev-powerpc-cross'],
		'aarch64-linux-gnu': lambda: ['libc6-dev-arm64-cross'],
		'arm-linux-gnueabi': lambda: ['libc6-dev-armel-cross'],
		'powerpc64le-linux-gnu': lambda: ['libc6-dev-ppc64el-cross'],
		'amd64-w64-mingw': lambda: ['mingw-w64-x86-64-dev'],
		'i686-w64-mingw': lambda: ['mingw-w64-i686-dev'],
		'i686-linux-musl': lambda: ['musl-dev:' + arch],
		'amd64-linux-musl': lambda: ['musl-dev:' + arch]
	 }.get(triplet, lambda: [])())

	if arch == 'i386' or arch == 'amd64':
		if os == 'linux':
			deplist.append('linux-libc-dev:' + arch)

		if triplet in ['i386-linux-gnu', 'amd64-linux-gnu']:
			deplist.extend([
				'libc6:' + arch,
				'libc6-dev:' + arch,
				'libcap-dev:' + arch,
				'libegl1-mesa:' + arch,
				'libegl1-mesa-dev:' + arch,
				'libgl1-mesa-glx:' + arch,
				'libgles2-mesa:' + arch,
				'libgles2-mesa-dev:' + arch,
				'libglib2.0-0:' + arch,
				'libpulse0:' + arch,
				'libpulse-dev:' + arch,
				'libseccomp2:' + arch,
				'libseccomp-dev:' + arch,
				'libxcb1:' + arch,
				'libxcb1-dev:' + arch,
				'libxcb-xkb1:' + arch,
				'libxcb-xkb-dev:' + arch,
				'libxdmcp-dev:' + arch,
				'libxkbcommon0:' + arch,
				'libxkbcommon-dev:' + arch,
				'libxkbcommon-x11-0:' + arch,
				'libxkbcommon-x11-dev:' + arch,

				'libasyncns0:' + arch,
				'libcap2:' + arch,
				'libdbus-1-3:' + arch,
				'libdrm2:' + arch,
				'libdrm-dev:' + arch,
				'libffi6:' + arch,
				'libflac8:' + arch,
				'libgbm1:' + arch,
				'libglapi-mesa:' + arch,
				'libjson-c2:' + arch,
				'libogg0:' + arch,
				'libpthread-stubs0-dev:' + arch,
				'libsndfile1:' + arch,
				'libvorbis0a:' + arch,
				'libvorbisenc2:' + arch,
				'libwayland-client0:' + arch,
				'libwayland-server0:' + arch,
				'libwrap0:' + arch,
				'libx11-6:' + arch,
				'libx11-dev:' + arch,
				'libx11-xcb1:' + arch,
				'libx11-xcb-dev:' + arch,
				'libxau6:' + arch,
				'libxau-dev:' + arch,
				'libxcb-dri2-0:' + arch,
				'libxcb-dri2-0-dev:' + arch,
				'libxcb-dri3-dev:' + arch,
				'libxcb-glx0-dev:' + arch,
				'libxcb-present-dev:' + arch,
				'libxcb-sync-dev:' + arch,
				'libxcb-xfixes0:' + arch,
				'libxcb-xfixes0-dev:' + arch,
				'libxdamage-dev:' + arch,
				'libxdmcp6:' + arch,
				'libxext-dev:' + arch,
				'libxfixes-dev:' + arch,
				'libxkbcommon-x11-dev:' + arch,
				'libxshmfence-dev:' + arch,
				'libxxf86vm-dev:' + arch])

	return deplist

def configure(program_name, arguments):
	parser = argparse.ArgumentParser(
	        prog = program_name + ' configure',
	        description = 'Build a final binary for binary distributions of Linted')
	parser.add_argument(
	        'CONFIG',
	        type = str,
	        help = 'configuration')
	parser.add_argument(
	        'SRCDIR',
	        type = str,
	        help = 'source directory')
	parser.add_argument(
	        'DEPDIR',
	        type = str,
	        help = 'dependencies directory')
	parser.add_argument(
	        'BUILDDIR',
	        type = str,
	        help = 'build directory')

	parsed = parser.parse_args(arguments)
	parser = None

	config = parsed.CONFIG
	srcdir = parsed.SRCDIR
	depdir = parsed.DEPDIR
	builddir = parsed.BUILDDIR
	parsed = None

	path = os.path.join(builddir, 'linted')
	try:
		os.mkdir(path)
	except FileExistsError:
		pass

	configure_script = os.path.realpath(os.path.join(srcdir, 'linted', 'configure-defaults'))
	depdir = os.path.realpath(depdir)

	CC='x86_64-w64-mingw32-gcc-4.8'
	HOST='x86_64-w64-mingw32'

	subprocess.check_call([configure_script, '--quiet', '--with-sysroot=' + depdir, 'CC=' + CC, '--prefix=/opt/linted', '--host=' + HOST,
			       'CFLAGS=-fno-lto -fno-stack-protector'], cwd = path)

	return 0

def build(program_name, arguments):
	parser = argparse.ArgumentParser(
	        prog = program_name + ' build',
	        description = 'Build a final binary for binary distributions of Linted')
	parser.add_argument(
	        'SRCDIR',
	        type = str,
	        help = 'source directory')
	parser.add_argument(
	        'BUILDDIR',
	        type = str,
	        help = 'build directory')

	parsed = parser.parse_args(arguments)
	parser = None

	srcdir = parsed.SRCDIR
	builddir = parsed.BUILDDIR
	parsed = None

	subprocess.check_call(['make', '--silent', '-C', os.path.join(builddir, 'linted')])
	subprocess.check_call(['make', '--silent', '-C', os.path.join(builddir, 'linted'), 'install',
			       'DESTDIR=' + os.path.realpath(os.path.join(builddir, 'tarball'))])
	subprocess.check_call(['cp', '--', os.path.join(srcdir, 'linted', 'README'), os.path.join(builddir, 'tarball')])

	dirs = []
	for dirpath, dirnames, filenames in os.walk(os.path.join(builddir, 'tarball')):
		dirs.extend([os.path.join(dirpath, path) for path in os.listdir(dirpath)])

	dirs = [os.path.relpath(path, start=os.path.join(builddir, 'tarball')) for path in dirs]

	dirs.sort()

	subprocess.check_call(['tar', '--null', '--no-recursion', '--mtime=0', '--numeric-owner', '--group=root', '--owner=root', '-c', '-f', os.path.join(builddir, 'tarball.tar'),
			       '-C', os.path.join(builddir, 'tarball'), '--'] + dirs)
	subprocess.check_call(['gzip', '-n', '-9', '-f', '--', os.path.join(builddir, 'tarball.tar')])

	return 0

TASKS = {
	'fetch-source': source,
	'fetch-dependencies': deps,
	'configure': configure,
	'build': build
}

if __name__ == '__main__':
	sys.exit(go(sys.argv))
