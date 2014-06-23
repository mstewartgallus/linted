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
#include "linted/random.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#define CURRENT_VERSION "0.0.0"

#define GLOBAL_LOCK "global.lock"

#define FIELD_DIR "fields"
#define TEMP_DIR "temp"

static linted_error prepend(char **result, char const *base, char const *end);

static linted_error fname_alloc(linted_ko ko, char **buf);
static linted_error fname(linted_ko ko, char *buf, size_t *sizep);

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

    {
        linted_ko xx;
        if ((errnum = linted_lock_file_create(
                 &xx, the_db, GLOBAL_LOCK, LINTED_LOCK_RDWR, S_IRUSR | S_IWUSR))
            != 0) {
            goto close_db;
        }
        lock_file = xx;
    }

    linted_lock lock;
    {
        linted_lock xx;
        errnum = linted_lock_acquire(&xx, lock_file);
        lock = xx;
    }

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
    linted_ko version_file;
    {
        linted_ko xx;
        errnum = linted_ko_open(&xx, the_db, "version", LINTED_KO_RDONLY);
        version_file = xx;
    }
    switch (errnum) {
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

        char *version_text;
        {
            linted_error xx;
            version_text = linted_mem_alloc(&xx, version_file_size);
            if ((errnum = xx) != 0) {
                goto close_version_file;
            }
        }

        if ((errnum = linted_io_read_all(version_file, NULL, version_text,
                                         version_file_size)) != 0) {
            goto free_version_text;
        }

        if (version_file_size != sizeof CURRENT_VERSION - 1) {
            errnum = EINVAL;
            goto free_version_text;
        }

        if (memcmp(version_text, CURRENT_VERSION, sizeof CURRENT_VERSION - 1)
            != 0) {
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

        linted_ko version_file_write;
        {
            linted_ko xx;
            if ((errnum
                 = linted_file_create(&xx, the_db, "version",
                                      LINTED_FILE_RDWR | LINTED_FILE_SYNC,
                                      S_IRUSR | S_IWUSR)) != 0) {
                goto unlock_db;
            }
            version_file_write = xx;
        }

        errnum = linted_io_write_all(version_file_write, NULL, CURRENT_VERSION,
                                     sizeof CURRENT_VERSION - 1u);

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
    linted_db_close(the_db);
    return errnum;
}

linted_error linted_db_close(linted_db db)
{
    return linted_ko_close(db);
}

linted_error linted_db_temp_file(linted_db db, linted_ko *kop)
{
    linted_error errnum = 0;

    static char const field_name[] = "field-XXXXXX.tmp";

    char *temp_path;
    if ((errnum = prepend(&temp_path, TEMP_DIR "/", field_name)) != 0) {
        return errnum;
    }

    char *xxxxxx = temp_path + strlen(temp_path) + sizeof "field-"
                   - sizeof field_name;

try_again:
    for (size_t ii = 0u; ii < 6u; ++ii) {
        char random_char;
        for (;;) {
            /* Normally using the modulus would give a bad
             * distribution but CHAR_MAX + 1u is a power of two
             */
            random_char = linted_random_fast() % (CHAR_MAX + 1u);

            /* Throw out results and retry for an even
             * distribution
             */
            if ((random_char >= 'a' && random_char <= 'z')
                || (random_char >= 'A' && random_char <= 'Z')
                || (random_char >= '0' && random_char <= '9')) {
                break;
            }
        }

        xxxxxx[ii] = random_char;
    }

    linted_ko temp_field;
    errnum = linted_file_create(&temp_field, db, temp_path,
                                LINTED_FILE_RDWR | LINTED_FILE_SYNC
                                | LINTED_FILE_EXCL,
                                S_IRUSR | S_IWUSR);
    if (EEXIST == errnum) {
        goto try_again;
    }

    linted_mem_free(temp_path);

    if (errnum != 0) {
        return errnum;
    }

    *kop = temp_field;
    return 0;
}

linted_error linted_db_temp_send(linted_db db, char const *name,
                                 linted_ko tmp)
{
    linted_error errnum;

    char *temp_path;
    {
        char *xx;
        if ((errnum = fname_alloc(tmp, &xx)) != 0) {
            return errnum;
        }
        temp_path = xx;
    }

    char *field_path;
    {
        char *xx;
        if ((errnum = prepend(&xx, FIELD_DIR "/", name)) != 0) {
            goto free_temp_path;
        }
        field_path = xx;
    }

    if (-1 == renameat(db, temp_path, db, field_path)) {
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

    size_t new_path_size = base_size + pathname_size + 1u;

    char *new_path;
    {
        linted_error xx;
        new_path = linted_mem_alloc(&xx, new_path_size);
        errnum = xx;
    }
    if (errnum != 0) {
        return errnum;
    }

    new_path[new_path_size - 1u] = '\0';
    memcpy(new_path, base, base_size);
    memcpy(new_path + base_size, pathname, pathname_size);

    *result = new_path;
    return 0;
}

static linted_error fname_alloc(linted_ko fd, char **bufp)
{
    linted_error errnum;
    size_t bytes_wrote;

    size_t buf_size = 40u;

    char *buf;
    {
        linted_error xx;
        buf = linted_mem_alloc(&xx, buf_size);
        errnum = xx;
    }
    if (errnum != 0) {
        return errnum;
    }

    for (;;) {
        bytes_wrote = buf_size;
        {
            size_t xx = bytes_wrote;
            if ((errnum = fname(fd, buf, &xx)) != 0) {
                goto free_buf;
            }
            bytes_wrote = xx;
        }

        if (bytes_wrote < buf_size) {
            break;
        }

        if (SIZE_MAX / 3 < buf_size) {
            errnum = ENOMEM;
            goto free_buf;
        }
        buf_size = (buf_size * 3u) / 2u;
        char *newbuf;
        {
            linted_error xx;
            newbuf = linted_mem_realloc(&xx, buf, buf_size);
            errnum = xx;
        }
        if (errnum != 0) {
            goto free_buf;
        }
        buf = newbuf;
    }

    /* Save on excess memory, also give debugging allocators more
     * information.
     */
    char *newbuf;
    {
        linted_error xx;
        newbuf = linted_mem_realloc(&xx, buf, bytes_wrote + 1u);
        errnum = xx;
    }
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
#define INT_STR_SIZE 10u

static linted_error fname(linted_ko fd, char *buf, size_t *sizep)
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
