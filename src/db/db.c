/*
 * Copyright 2014 Steven Stewart-Gallus
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

#include "linted/db.h"

#include "linted/error.h"
#include "linted/io.h"
#include "linted/ko.h"

#include <assert.h>
#include <fcntl.h>
#include <semaphore.h>
#include <signal.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/prctl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#define CURRENT_VERSION "0.0.0"

#define GLOBAL_LOCK "global.lock"

#define FIELD_DIR "fields"
#define TEMP_DIR "temp"

static linted_error lock_db(linted_db *dbp, pid_t * lock);
static void unlock_db(linted_db *dbp, pid_t lock);

static void exit_with_error(volatile linted_error *spawn_error,
                            linted_error errnum);

static size_t align_to_page_size(size_t size);

static linted_error prepend(char **result, char const *base, char const *end);

static linted_error fname_alloc(int fd, char **buf);
static linted_error fname(int fd, char *buf, size_t *sizep);

linted_error linted_db_open(linted_db *dbp, linted_ko cwd, char const *pathname,
                            int flags)
{
    linted_error errnum;

    if ((flags & ~LINTED_DB_CREAT) != 0) {
        return EINVAL;
    }

    if (0 == flags) {
        return EINVAL;
    }

    bool db_creat = (flags & LINTED_DB_CREAT) != 0;

    if (db_creat) {
        if (-1 == mkdirat(cwd, pathname, S_IRWXU)) {
            errnum = errno;
            if (errnum != EEXIST) {
                return errnum;
            }
        }
    }

    int o_flags = O_DIRECTORY | O_CLOEXEC;

    linted_db the_db = openat(cwd, pathname, o_flags);
    if (-1 == the_db) {
        return errno;
    }

/*
 * This happens at startup and has to be consistent across every
 * version of the database.
 */

try_to_open_lock_again:
    ;
    pid_t lock;
    linted_error lock_errnum = lock_db(&the_db, &lock);
    if (lock_errnum != 0) {
        if (lock_errnum != ENOENT) {
            errnum = lock_errnum;
            goto close_db;
        }

        /* Lock does not exist try to create it */
        int lock_file = openat(the_db, GLOBAL_LOCK,
                              O_RDWR
                              | O_CLOEXEC | O_NONBLOCK
                              | O_CREAT | O_EXCL,
                              S_IRUSR | S_IWUSR);
        if (-1 == lock_file) {
            errnum = errno;

            if (EEXIST == errnum) {
                /* File already exists try to lock it */
                goto try_to_open_lock_again;
            }
            goto close_db;
        }

        if (-1 == linted_ko_close(lock_file)) {
            errnum = errno;
            goto close_db;
        }

        /* File created try to lock it */
        goto try_to_open_lock_again;
    }

    /* Sole user of the database now */
    int version_file;
    switch (errnum = linted_ko_open(&version_file, the_db, "version",
                                    LINTED_KO_RDONLY)) {
    case 0: {
        /* Opening a created database */
        off_t version_file_size;
        {
            struct stat stats;
            if (-1 == fstat(version_file, &stats)) {
                errnum = errno;
                goto close_version_file;
            }

            version_file_size = stats.st_size;
        }

        /* Maybe we're a 32 bit binary and opening up a 64 bit file */
        if ((uintmax_t)version_file_size > (uintmax_t)SIZE_MAX) {
            errnum = ENOMEM;
            goto close_version_file;
        }

        char *const version_text = malloc(version_file_size);
        if (NULL == version_text) {
            errnum = errno;
            goto close_version_file;
        }

        if ((errnum = linted_io_read_all(version_file, NULL, version_text,
                                         version_file_size)) != 0) {
            goto free_version_text;
        }

        if (version_file_size != sizeof CURRENT_VERSION - 1) {
            errnum = EINVAL;
            goto free_version_text;
        }

        if (memcmp(version_text, CURRENT_VERSION, sizeof CURRENT_VERSION - 1) !=
            0) {
            errnum = EINVAL;
            goto free_version_text;
        }

    free_version_text:
        free(version_text);

    close_version_file : {
        linted_error close_errnum = linted_ko_close(version_file);
        if (0 == errnum) {
            errnum = close_errnum;
        }
    } break;
    }

    case ENOENT: {
        /* Creating the initial database */
        if (!db_creat) {
            errnum = EINVAL;
            goto unlock_db;
        }

        int version_file_write = openat(
            the_db, "version", O_RDWR | O_SYNC | O_CLOEXEC | O_CREAT | O_EXCL,
            S_IRUSR | S_IWUSR);
        if (-1 == version_file_write) {
            errnum = errno;
            goto unlock_db;
        }

        errnum = linted_io_write_all(version_file_write, NULL, CURRENT_VERSION,
                                     sizeof CURRENT_VERSION - 1);

        {
            linted_error close_errnum = linted_ko_close(version_file_write);
            if (0 == errnum) {
                errnum = close_errnum;
            }
        }

        if (errnum != 0) {
            goto unlock_db;
        }

        if (-1 == mkdirat(the_db, TEMP_DIR, S_IRWXU)) {
            errnum = errno;
            goto unlock_db;
        }

        if (-1 == mkdirat(the_db, FIELD_DIR, S_IRWXU)) {
            errnum = errno;
            goto unlock_db;
        }
        break;
    }
    }

unlock_db:
    unlock_db(&the_db, lock);

    if (errnum != 0) {
        goto close_db;
    }

    *dbp = the_db;
    return 0;

close_db:
    linted_db_close(&the_db);
    return errnum;
}

linted_error linted_db_close(linted_db *dbp)
{
    return linted_ko_close(*dbp);
}

linted_error linted_db_temp_file(linted_db *dbp, int *fildesp)
{
    linted_error errnum = 0;

    static char const field_name[] = "field-XXXXXX.tmp";

    char *temp_path;
    if ((errnum = prepend(&temp_path, TEMP_DIR "/", field_name)) != 0) {
        return errnum;
    }

    char *xxxxxx =
        temp_path + strlen(temp_path) + sizeof "field-" - sizeof field_name;

try_again:
    for (size_t ii = 0; ii < 6; ++ii) {
        for (;;) {
            char value = ((int)(unsigned char)rand()) - 255 / 2 - 1;
            if ((value > 'a' && value < 'z') || (value > 'A' && value < 'Z') ||
                (value > '0' && value < '9')) {
                xxxxxx[ii] = value;
                break;
            }
        }
    }

    int temp_field =
        openat(*dbp, temp_path,
               O_RDWR | O_SYNC
               | O_CLOEXEC | O_NONBLOCK
               | O_CREAT | O_EXCL,
               S_IRUSR | S_IWUSR);
    if (-1 == temp_field) {
        linted_error open_errnum = errno;
        if (EEXIST == open_errnum) {
            goto try_again;
        } else {
            errnum = open_errnum;
        }
    }

    free(temp_path);

    if (errnum != 0) {
        return errnum;
    }

    *fildesp = temp_field;
    return 0;
}

linted_error linted_db_temp_send(linted_db *dbp, char const *name, int tmp)
{
    linted_error errnum;

    char *temp_path;
    if ((errnum = fname_alloc(tmp, &temp_path)) != 0) {
        return errnum;
    }

    char *field_path;
    if ((errnum = prepend(&field_path, FIELD_DIR "/", name)) != 0) {
        goto free_temp_path;
    }

    if (-1 == renameat(*dbp, temp_path, *dbp, field_path)) {
        errnum = errno;
    }

    free(field_path);

free_temp_path:
    free(temp_path);

    return errnum;
}

static linted_error lock_db(linted_db *dbp, pid_t * lock)
{
    linted_error errnum = 0;

    size_t spawn_error_length = align_to_page_size(sizeof(linted_error));

    volatile linted_error *spawn_error =
        mmap(NULL, spawn_error_length, PROT_READ | PROT_WRITE,
             MAP_SHARED | MAP_ANONYMOUS, -1, 0);
    if (MAP_FAILED == spawn_error) {
        return errno;
    }

    /*
     * We have to fork because locks are associated with (pid, inode)
     * pairs. Now any other thread that tries to lock the db will
     * simply wait for this process to exit.
     */
    pid_t child = fork();
    if (-1 == child) {
        return errno;
    }

    if (0 == child) {
        /* Don't leak the process in case of an unexpected parent death */
        prctl(PR_SET_PDEATHSIG, SIGKILL, 0, 0);

        int lock_file;
        if ((errnum = linted_ko_open(&lock_file, *dbp, GLOBAL_LOCK,
                                     LINTED_KO_RDWR)) != 0) {
            exit_with_error(spawn_error, errno);
        }

        struct flock flock = {
            .l_type = F_WRLCK,
            .l_whence = SEEK_SET,
            .l_start = 0,
            .l_len = 0
        };
        if (-1 == fcntl(lock_file, F_SETLK, &flock)) {
            exit_with_error(spawn_error, errno);
        }

        /* Got the lock! */

        /* The lock is killed and never dies by itself */
        for (;;) {
            raise(SIGSTOP);
        }
    }

    siginfo_t info;
    do {
        int wait_status = waitid(P_PID, child, &info, WEXITED | WSTOPPED);
        errnum = -1 == wait_status ? errno : 0;
    } while (EINTR == errnum);
    if (errnum != 0) {
        goto unmap_spawn_error;
    }

    switch (info.si_code) {
    case CLD_EXITED: {
        linted_error exit_status = info.si_status;
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
        linted_error signo = info.si_status;
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
    if (-1 == munmap((void *)spawn_error, spawn_error_length)) {
        if (0 == errnum) {
            errnum = errno;
        }
    }

    *lock = child;

    return errnum;
}

static void unlock_db(linted_db *dbp, pid_t lock)
{
    linted_error errnum;

    if (-1 == kill(lock, SIGKILL)) {
        errnum = errno;
        assert(errnum != EINVAL);
        assert(errnum != EPERM);
        assert(errnum != ESRCH);
        assert(false);
    }

    /* Unfortunately, one cannot pass NULL into the info parameter here */
    siginfo_t info;
    do {
        int wait_status = waitid(P_PID, lock, &info, WEXITED | WSTOPPED);
        errnum = -1 == wait_status ? errno : 0;
    } while (EINTR == errnum);

    assert(errnum != EINVAL);
    assert(errnum != ECHILD);
    assert(0 == errnum);
}

static size_t align_to_page_size(size_t size)
{
    size_t page_size = sysconf(_SC_PAGESIZE);

    /* This should always be true */
    assert(page_size > 0);

    /* Round up to a multiple of page size */
    size_t remainder = size % page_size;
    if (0 == remainder)
        return size;

    return size - remainder + page_size;
}

static void exit_with_error(volatile linted_error *spawn_error,
                            linted_error errnum)
{
    *spawn_error = errnum;
    raise(SIGTERM);
}

static linted_error prepend(char **result, char const *base,
                            char const *pathname)
{
    size_t base_size = strlen(base);
    size_t pathname_size = strlen(pathname);

    size_t new_path_size = base_size + pathname_size + 1;

    char *new_path = malloc(new_path_size);
    if (NULL == new_path) {
        return errno;
    }

    new_path[new_path_size - 1] = '\0';
    memcpy(new_path, base, base_size);
    memcpy(new_path + base_size, pathname, pathname_size);

    *result = new_path;
    return 0;
}

static int fname_alloc(int fd, char **bufp)
{
    int errnum;
    size_t bytes_wrote;

    size_t buf_size = 40;

    char *buf = malloc(buf_size);
    if (NULL == buf) {
        return errno;
    }

    for (;;) {
        bytes_wrote = buf_size;
        if ((errnum = fname(fd, buf, &bytes_wrote)) != 0) {
            goto free_buf;
        }

        if (bytes_wrote < buf_size) {
            break;
        }

        if (SIZE_MAX / 3 < buf_size) {
            errnum = ENOMEM;
            goto free_buf;
        }
        buf_size = (buf_size * 3u) / 2u;
        char *newbuf = realloc(buf, buf_size);
        if (NULL == newbuf) {
            errnum = errno;
            goto free_buf;
        }
        buf = newbuf;
    }

    /* Save on excess memory, also give debugging allocators more
     * information.
     */
    char *newbuf = realloc(buf, bytes_wrote + 1);
    if (NULL == newbuf) {
        errnum = errno;
        goto free_buf;
    }
    buf = newbuf;

    buf[bytes_wrote] = '\0';

    *bufp = buf;
    return 0;

free_buf:
    free(buf);

    return errnum;
}

#define PROC_SELF_FD "/proc/self/fd/"
#define INT_STR_SIZE 10

static int fname(int fd, char *buf, size_t *sizep)
{
    {
        struct stat statistics;
        if (-1 == fstat(fd, &statistics)) {
            return errno;
        }

        if (0 == statistics.st_nlink) {
            return ENOENT;
        }
    }

    static char const proc_self_fd[sizeof PROC_SELF_FD - 1] = PROC_SELF_FD;

    char fd_path[sizeof proc_self_fd + INT_STR_SIZE + 1];

    memcpy(fd_path, proc_self_fd, sizeof proc_self_fd);

    char *int_end = fd_path + sizeof proc_self_fd;

    size_t bytes_written = (size_t)snprintf(int_end, INT_STR_SIZE, "%i", fd);
    int_end[bytes_written] = '\0';

    size_t size = *sizep;

    ssize_t bytes_wrote = readlink(fd_path, buf, size);
    if (-1 == bytes_wrote) {
        return errno;
    }

    *sizep = bytes_wrote;
    return 0;
}
