Copyright (C) 2013, 2014 Steven Stewart-Gallus

Copying and distribution of this file, with or without modification,
are permitted in any medium without royalty provided the copyright
notice and this notice are preserved.

Linted -- TODO

* Integrate this documentation with Doxygen
* Port to --host=i686-w64-mingw32

    * As part of this process environment sanitization code will need
      to be reworked. Windows maintains two copies of environment
      variables: one copy in process space and the other copy in
      kernel space.
* Move away from C legacy practises
  - move away from errno
  - move away from null terminated strings

* Port to SDL 2.0
* Manage child process error streams and syslog logs
* Use more secure method for communicating with the init

    The current method is insecure because the init requires PTRACE
    capabilities. As well, the sigqueue system call is wrapped by
    GLibc with the rt_sigqueue system call that uses a user provided
    pid. An attacker could write to another process by sending a
    command to init with a fake pid.

* Make a hang check timer

    This timer will periodically make sure tasks are reaching their
    deadlines by sending a message to every message queue and seeing
    if the owner of the queue responds within appropriate
    deadlines. It will then kill and restart the process if it is not
    responding.

* Monitor vulnerabilities in dependencies and supported platforms
* Think about whether to use -Wa,--noexecstack and -Wl,-z,noexecstack

    I think I read somewhere that these force a nonexecutable stack
    but that if things are done correctly that the shouldn't be
    happening anyways. I believe what I really want is a
    -Wl,--warn-execstack option.

* Don't do cargo cult security

    This is pretty much impossible.

    I'm just a young hacker playing around with technologies like
    sandboxing. I have no real knowledge of security. I hope this
    project is useful as a personal learning experience though.

* Don't do security theater

    Security theater is when people do big and impressive things with
    sandboxing but then conveniently forget a back door that lets
    attackers in.

    See Don't do cargo cult security for why this is impossible.
