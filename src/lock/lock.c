/*
 * Copyright 2013, 2014 Steven Stewart-Gallus
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#define _GNU_SOURCE

#include "config.h"

#include "linted/lock.h"

#include "linted/file.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <sys/mman.h>
#include <sys/prctl.h>
#include <sys/wait.h>
#include <unistd.h>

static void exit_with_error(volatile linted_error *spawn_error,
                            linted_error errnum);
static size_t align_to_page_size(size_t size);

linted_error linted_lock_file_create(linted_ko *kop, linted_ko dirko,
                                     char const *pathname, int flags,
                                     mode_t mode)
{
    if ((flags & ~LINTED_LOCK_RDONLY & ~LINTED_LOCK_WRONLY & ~LINTED_LOCK_RDWR
         & ~LINTED_LOCK_EXCL) != 0) {
        return EINVAL;
    }

    bool lock_rdonly = (flags & LINTED_LOCK_RDONLY) != 0u;
    bool lock_wronly = (flags & LINTED_LOCK_WRONLY) != 0u;
    bool lock_rdwr = (flags & LINTED_LOCK_RDWR) != 0u;

    bool lock_excl = (flags & LINTED_LOCK_EXCL) != 0;

    int file_flags = 0;

    if (lock_rdonly != 0) {
        file_flags |= LINTED_FILE_RDONLY;
    }
    if (lock_wronly != 0) {
        file_flags |= LINTED_FILE_WRONLY;
    }
    if (lock_rdwr != 0) {
        file_flags |= LINTED_FILE_RDWR;
    }

    if (lock_excl != 0) {
        file_flags |= LINTED_FILE_EXCL;
    }

    /* Lock does not exist try to create it */
    return linted_file_create(kop, dirko, pathname, file_flags, mode);
}

linted_error linted_lock_acquire(linted_lock *lockp, linted_ko lock_file)
{
    linted_error errnum = 0;

    size_t spawn_error_length = align_to_page_size(sizeof(linted_error));

    linted_error *spawn_error
        = mmap(NULL, spawn_error_length, PROT_READ | PROT_WRITE,
               MAP_SHARED | MAP_ANONYMOUS, -1, 0);
    if (MAP_FAILED == spawn_error) {
        errnum = errno;
        assert(errnum != 0);
        assert(errnum != EINVAL);
        return errnum;
    }

    /*
     * We have to fork because locks are associated with (pid, inode)
     * pairs. Now any other thread that tries to lock the db will
     * simply wait for this process to exit.
     */
    pid_t child = fork();
    if (-1 == child) {
        errnum = errno;
        assert(errnum != 0);
        return errnum;
    }

    if (0 == child) {
        /* Don't leak the process in case of an unexpected parent death */
        prctl(PR_SET_PDEATHSIG, SIGKILL, 0, 0);

        /* Make a copy so that the parent can't unlock the lock accidentally */
        {
            linted_lock_file xx = lock_file;
            if ((errnum = linted_ko_reopen(&xx, LINTED_KO_RDWR)) != 0) {
                exit_with_error(spawn_error, errnum);
            }
            lock_file = xx;
        }

        {
            struct flock flock = {
                .l_type = F_WRLCK,
                .l_whence = SEEK_SET,
                .l_start = 0,
                .l_len = 0
            };
            if (-1 == fcntl(lock_file, F_SETLK, &flock)) {
                exit_with_error(spawn_error, errno);
            }
        }

        /* Got the lock! */

        /* The lock is killed and never dies by itself */
        for (;;) {
            raise(SIGSTOP);
        }
    }

    int code;
    int status;
    {
        siginfo_t info;
        do {
            if (-1 == waitid(P_PID, child, &info, WEXITED | WSTOPPED)) {
                errnum = errno;
                assert(errnum != 0);
            } else {
                errnum = 0;
            }
        } while (EINTR == errnum);
        assert(errnum != ECHILD);
        assert(errnum != EINVAL);
        assert(0 == errnum);

        status = info.si_status;
        code = info.si_code;
    }

    switch (code) {
    case CLD_EXITED: {
        linted_error exit_status = status;
        switch (exit_status) {
        case 0:
            errnum = EINVAL;
            goto unmap_spawn_error;

        default:
            errnum = ENOSYS;
            goto unmap_spawn_error;
        }
    }

    case CLD_DUMPED:
    case CLD_KILLED: {
#if defined __linux__
        linted_error signo = status;
        switch (signo) {
        case SIGKILL:
            errnum = ENOMEM;
            break;

        case SIGSEGV:
            /* Corrupted elf file */
            errnum = ENOEXEC;
            break;

        case SIGTERM:
            /* Exited with error and passed an error code */
            errnum = *spawn_error;
            assert(errnum != 0);
            break;

        default:
            errnum = ENOSYS;
            break;
        }
        goto unmap_spawn_error;
#else
#error The exit status of an aborted execve is very OS specific
#endif
    }

    case CLD_STOPPED:
        /*
         * Don't continue the process. Let it be stopped and holding
         * the lock
         */
        break;

    default:
        assert(false);
    }

unmap_spawn_error:
    if (-1 == munmap(spawn_error, spawn_error_length)) {
        linted_error munmap_errnum = errno;
        assert(munmap_errnum != 0);

        if (0 == errnum) {
            errnum = munmap_errnum;
        }
    }

    *lockp = child;

    return errnum;
}

linted_error linted_lock_release(linted_lock lock)
{
    linted_error errnum;

    if (-1 == kill(lock, SIGKILL)) {
        errnum = errno;

        if (ESRCH == errnum) {
            return EINVAL;
        }

        assert(errnum != 0);
        assert(errnum != EINVAL);
        assert(errnum != EPERM);
        assert(false);
    }

    /* Unfortunately, one cannot pass NULL into the info parameter here */
    siginfo_t info;
    do {
        if (-1 == waitid(P_PID, lock, &info, WEXITED)) {
            errnum = errno;
            assert(errnum != 0);
        } else {
            errnum = 0;
        }
    } while (EINTR == errnum);
    assert(errnum != EINVAL);
    assert(errnum != ECHILD);
    assert(0 == errnum);

    return 0;
}

static size_t align_to_page_size(size_t size)
{
    size_t page_size = sysconf(_SC_PAGESIZE);

    /* This should always be true */
    assert(page_size > 0u);

    /* Round up to a multiple of page size */
    size_t remainder = size % page_size;
    if (0u == remainder)
        return size;

    return size - remainder + page_size;
}

static void exit_with_error(volatile linted_error *spawn_error,
                            linted_error errnum)
{
    volatile linted_error *vol_spawn_error = spawn_error;
    *vol_spawn_error = errnum;
    raise(SIGTERM);
}
