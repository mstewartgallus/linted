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
#include "linted/file.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/lock.h"
#include "linted/mem.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#if defined _WIN32 || defined _WIN64
#define snprintf sprintf_s
#endif

#define CURRENT_VERSION "0.0.0"

#define GLOBAL_LOCK "global.lock"

#define FIELD_DIR "fields"
#define TEMP_DIR "temp"

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
            assert(errnum != 0);
            if (errnum != EEXIST) {
                return errnum;
            }
        }
    }

    int o_flags = O_DIRECTORY | O_CLOEXEC;

    linted_db the_db = openat(cwd, pathname, o_flags);
    if (-1 == the_db) {
        errnum = errno;
        assert(errnum != 0);
        return errnum;
    }

    /*
     * This happens at startup and has to be consistent across every
     * version of the database.
     */
    linted_ko lock_file;

    if ((errnum = linted_lock_file_create(&lock_file, the_db, GLOBAL_LOCK,
                                          0)) != 0) {
        goto close_db;
    }

    linted_lock lock;
    errnum = linted_lock_acquire(&lock, lock_file);

    {
        linted_error close_errnum = linted_ko_close(lock_file);
        if (0 == errnum) {
            errnum = close_errnum;
        }
    }

    if (errnum != 0) {
        goto close_db;
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
                assert(errnum != 0);
                goto close_version_file;
            }

            version_file_size = stats.st_size;
        }

        /* Maybe we're a 32 bit binary and opening up a 64 bit file */
        if ((uintmax_t)version_file_size > (uintmax_t)SIZE_MAX) {
            errnum = ENOMEM;
            goto close_version_file;
        }

        char *const version_text = linted_mem_alloc(&errnum, version_file_size);
        if (errnum != 0) {
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
        linted_mem_free(version_text);

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

        int version_file_write;
        if ((errnum = linted_file_create(&version_file_write, the_db, "version",
                                         LINTED_FILE_RDWR | LINTED_FILE_SYNC,
                                         S_IRUSR | S_IWUSR)) != 0) {
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
            assert(errnum != 0);
            goto unlock_db;
        }

        if (-1 == mkdirat(the_db, FIELD_DIR, S_IRWXU)) {
            errnum = errno;
            assert(errnum != 0);
            goto unlock_db;
        }
        break;
    }
    }

unlock_db:
    linted_lock_release(lock);

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

linted_error linted_db_temp_file(linted_db *dbp, linted_ko *kop)
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
    for (size_t ii = 0u; ii < 6; ++ii) {
        for (;;) {
            char value = ((int)(unsigned char)rand()) - 255 / 2 - 1;
            if ((value > 'a' && value < 'z') || (value > 'A' && value < 'Z') ||
                (value > '0' && value < '9')) {
                xxxxxx[ii] = value;
                break;
            }
        }
    }

    int temp_field;
    linted_error open_errnum = linted_file_create(&temp_field, *dbp, temp_path,
                                                  LINTED_FILE_RDWR | LINTED_FILE_SYNC | LINTED_FILE_EXCL,
                                                  S_IRUSR | S_IWUSR);
    if (open_errnum != 0) {
        if (EEXIST == open_errnum) {
            goto try_again;
        } else {
            errnum = open_errnum;
        }
    }

    linted_mem_free(temp_path);

    if (errnum != 0) {
        return errnum;
    }

    *kop = temp_field;
    return 0;
}

linted_error linted_db_temp_send(linted_db *dbp, char const *name,
                                 linted_ko tmp)
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
        assert(errnum != 0);
    }

    linted_mem_free(field_path);

free_temp_path:
    linted_mem_free(temp_path);

    return errnum;
}

static linted_error prepend(char **result, char const *base,
                            char const *pathname)
{
    linted_error errnum;
    size_t base_size = strlen(base);
    size_t pathname_size = strlen(pathname);

    size_t new_path_size = base_size + pathname_size + 1;

    char *new_path = linted_mem_alloc(&errnum, new_path_size);
    if (errnum != 0) {
        return errnum;
    }

    new_path[new_path_size - 1u] = '\0';
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

    char *buf = linted_mem_alloc(&errnum, buf_size);
    if (errnum != 0) {
        return errnum;
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
        char *newbuf = linted_mem_realloc(&errnum, buf, buf_size);
        if (errnum != 0) {
            goto free_buf;
        }
        buf = newbuf;
    }

    /* Save on excess memory, also give debugging allocators more
     * information.
     */
    char *newbuf = linted_mem_realloc(&errnum, buf, bytes_wrote + 1);
    if (errnum != 0) {
        goto free_buf;
    }
    buf = newbuf;

    buf[bytes_wrote] = '\0';

    *bufp = buf;
    return 0;

free_buf:
    linted_mem_free(buf);

    return errnum;
}

#define PROC_SELF_FD "/proc/self/fd/"
#define INT_STR_SIZE 10

static int fname(int fd, char *buf, size_t *sizep)
{
    linted_error errnum;

    {
        struct stat statistics;
        if (-1 == fstat(fd, &statistics)) {
            errnum = errno;
            assert(errnum != 0);
            return errnum;
        }

        if (0 == statistics.st_nlink) {
            return ENOENT;
        }
    }

    static char const proc_self_fd[sizeof PROC_SELF_FD - 1u] = PROC_SELF_FD;

    char fd_path[sizeof proc_self_fd + INT_STR_SIZE + 1u];

    memcpy(fd_path, proc_self_fd, sizeof proc_self_fd);

    char *int_end = fd_path + sizeof proc_self_fd;

    size_t bytes_written = (size_t)snprintf(int_end, INT_STR_SIZE, "%i", fd);
    int_end[bytes_written] = '\0';

    size_t size = *sizep;

    ssize_t bytes_wrote = readlink(fd_path, buf, size);
    if (-1 == bytes_wrote) {
        errnum = errno;
        assert(errnum != 0);
        return errnum;
    }

    *sizep = bytes_wrote;
    return 0;
}
