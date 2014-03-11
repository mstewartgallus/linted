Copyright (C) 2013, 2014 Steven Stewart-Gallus

Copying and distribution of this file, with or without modification,
are permitted in any medium without royalty provided the copyright
notice and this notice are preserved.

Linted -- TODO

* Integrate this documentation with Doxygen
* Port to --host=i686-w64-mingw32
* Ban assert in favour of LINTED_IMPOSSIBLE_ERROR
assert has problems with not evaluating arguments.
* Port to SDL 2.0
* Manage child process error streams and syslog logs
* Make a hang check timer
This timer will periodically make sure tasks are reaching their
deadlines by sending a message to every message queue and seeing if
the owner of the queue responds within appropriate deadlines. It will
then kill and restart the process if it is not responding.
* Monitor vulnerabilities in dependencies and supported platforms
* Think about whether to use -Wa,--noexecstack and -Wl,-z,noexecstack
I think I read somewhere that these force a nonexecutable stack but
that if things are done correctly that the shouldn't be happening
anyways. I believe what I really want is a -Wl,--warn-execstack
option.
* Make spawner forks check passed in sockets
Because the spawner forks off processes the current protocol isn't so
bad but there should be a little error checking.
* Don't do cargo cult security
This is pretty much impossible.

I'm just a young hacker playing around with technologies like
sandboxing. I have no real knowledge of security. I hope this project
is useful as a personal learning experience though.
* Don't do security theater
Security theater is when people do big and impressive things with
sandboxing but then conveniently forget a back door that lets
attackers in.

See cargo cult security for why this is impossible.
