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

#include "linted/dir.h"
#include "linted/error.h"
#include "linted/file.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/random.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#define CURRENT_VERSION "0.0.0"

#define GLOBAL_LOCK "global.lock"

#define FIELD_DIR "fields"
#define TEMP_DIR "temp"

static linted_error mutex_lock(linted_ko mutex);
static linted_error mutex_unlock(linted_ko mutex_file);

static size_t align_to_page_size(size_t size);

static linted_error prepend(char **result, char const *base, char const *end);

linted_error linted_db_open(linted_db * restrict dbp, linted_ko cwd,
                            char const * pathname, int flags)
{
    linted_error errnum;

    if ((flags & ~LINTED_DB_CREAT) != 0) {
        return EINVAL;
    }

    if (0 == flags) {
        return EINVAL;
    }

    bool db_creat = (flags & LINTED_DB_CREAT) != 0;

    linted_dir the_db;
    if (db_creat) {
        linted_dir xx;
        if ((errnum = linted_dir_create(&xx, cwd, pathname, 0, S_IRWXU)) != 0) {
            return errnum;
        }
        the_db = xx;
    } else {
        linted_dir xx;
        if ((errnum = linted_ko_open(&xx, cwd, pathname, 0)) != 0) {
            return errnum;
        }
        the_db = xx;
    }

    /*
     * This happens at startup and has to be consistent across every
     * version of the database.
     */
    linted_ko lock_file;

try_to_open_lock_file : {
    linted_ko xx;
    if ((errnum = linted_ko_open(&xx, the_db, GLOBAL_LOCK, LINTED_KO_RDWR))
        != 0) {
        if (ENOENT == errnum) {
            goto try_to_create_lock_file;
        }
        goto close_db;
    }
    lock_file = xx;
    goto opened_lock_file;
}

try_to_create_lock_file : {
    char prototype_lock_file[] = "%i.lock";
    if (sprintf(prototype_lock_file, "%i.lock", getpid()) < 0) {
        errnum = errno;
        LINTED_ASSUME(errnum != 0);
        assert(errnum != EINVAL);
        goto close_db;
    }

    {
        linted_ko xx;
        if ((errnum = linted_file_create(&xx, the_db, prototype_lock_file,
                                         LINTED_FILE_RDWR, S_IRUSR | S_IWUSR))
            != 0) {
            goto close_db;
        }
        lock_file = xx;
    }

    if (-1 == ftruncate(lock_file, sizeof(pthread_mutex_t))) {
        errnum = errno;
        LINTED_ASSUME(errnum != 0);
        assert(errnum != EINVAL);
        goto unlink_prototype_lock_file;
    }

    size_t mutex_length = align_to_page_size(sizeof(pthread_mutex_t));

    pthread_mutex_t *mutex = mmap(NULL, mutex_length, PROT_READ | PROT_WRITE,
                                  MAP_SHARED, lock_file, 0);
    if (MAP_FAILED == mutex) {
        errnum = errno;
        LINTED_ASSUME(errnum != 0);
        assert(errnum != EINVAL);
        goto unlink_prototype_lock_file;
    }

    {
        pthread_mutexattr_t attr;

        if ((errnum = pthread_mutexattr_init(&attr)) != 0) {
            goto unmap_mutexattr;
        }

        if ((errnum = pthread_mutexattr_setpshared(
                 &attr, PTHREAD_PROCESS_SHARED)) != 0) {
            assert(errnum != EINVAL);
            assert(false);
        }

        if ((errnum = pthread_mutexattr_setrobust(&attr, PTHREAD_MUTEX_ROBUST))
            != 0) {
            assert(errnum != EINVAL);
            assert(false);
        }

        if ((errnum = pthread_mutex_init(mutex, &attr)) != 0) {
            assert(errnum != EINVAL);
        }

        {
            linted_error destroy_errnum = pthread_mutexattr_destroy(&attr);
            assert(0 == destroy_errnum);
        }
    }

unmap_mutexattr:
    if (-1 == munmap(mutex, mutex_length)) {
        linted_error munmap_errnum = errno;
        assert(munmap_errnum != EINVAL);
        assert(false);
    }

    if (errnum != 0) {
        goto unlink_prototype_lock_file;
    }

    {
        char lock_file_path[] = "/proc/self/fd/XXXXXXXXXXXXXXX";
        sprintf(lock_file_path, "/proc/self/fd/%i", lock_file);
        if (-1 == linkat(-1, lock_file_path, the_db, GLOBAL_LOCK,
                         AT_SYMLINK_FOLLOW)) {
            errnum = errno;
            LINTED_ASSUME(errnum != 0);
            assert(errnum != EINVAL);
        }
    }

unlink_prototype_lock_file:
    if (-1 == unlinkat(the_db, prototype_lock_file, 0)) {
        if (0 == errnum) {
            errnum = errno;
            LINTED_ASSUME(errnum != 0);
            assert(errnum != EINVAL);
        }
    }

    if (errnum != 0) {
        {
            linted_error close_errnum = linted_ko_close(lock_file);
            assert(close_errnum != EBADF);
        }

        if (EEXIST == errnum) {
            goto try_to_open_lock_file;
        }

        goto close_db;
    }

    goto opened_lock_file;
}

opened_lock_file:

    if ((errnum = mutex_lock(lock_file)) != 0) {
        goto close_lock_file;
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
                LINTED_ASSUME(errnum != 0);
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
            void *xx;
            if ((errnum = linted_mem_alloc(&xx, version_file_size)) != 0) {
                goto close_version_file;
            }
            version_text = xx;
        }

        if ((errnum = linted_io_read_all(version_file, NULL, version_text,
                                         version_file_size)) != 0) {
            goto free_version_text;
        }

        if (version_file_size != sizeof CURRENT_VERSION - 1U) {
            errnum = EINVAL;
            goto free_version_text;
        }

        if (memcmp(version_text, CURRENT_VERSION, sizeof CURRENT_VERSION - 1U)
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
        break;
    }
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
                                     sizeof CURRENT_VERSION - 1U);

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
            LINTED_ASSUME(errnum != 0);
            goto unlock_db;
        }

        if (-1 == mkdirat(the_db, FIELD_DIR, S_IRWXU)) {
            errnum = errno;
            LINTED_ASSUME(errnum != 0);
            goto unlock_db;
        }
        break;
    }
    }

unlock_db:
    mutex_unlock(lock_file);

close_lock_file:
    linted_ko_close(lock_file);

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

linted_error linted_db_temp_file(linted_db db, linted_ko * restrict kop,
                                 char ** restrict pathp)
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
    for (size_t ii = 0U; ii < 6U; ++ii) {
        char random_char;
        for (;;) {
            /* Normally using the modulus would give a bad
             * distribution but CHAR_MAX + 1U is a power of two
             */
            random_char = linted_random_fast() % (CHAR_MAX + 1U);

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

    if (errnum != 0) {
        linted_mem_free(temp_path);
        return errnum;
    }

    *pathp = temp_path;
    *kop = temp_field;
    return 0;
}

linted_error linted_db_temp_send(linted_db db, char const * path,
                                 char const * name)
{
    linted_error errnum;

    char *field_path;
    {
        char *xx;
        if ((errnum = prepend(&xx, FIELD_DIR "/", name)) != 0) {
            return errnum;
        }
        field_path = xx;
    }

    if (-1 == renameat(db, path, db, field_path)) {
        errnum = errno;
        LINTED_ASSUME(errnum != 0);
    }

    linted_mem_free(field_path);

    return errnum;
}

static linted_error prepend(char **result, char const *base,
                            char const *pathname)
{
    linted_error errnum;
    size_t base_size = strlen(base);
    size_t pathname_size = strlen(pathname);

    size_t new_path_size = base_size + pathname_size + 1U;

    char *new_path;
    {
        void *xx;
        if ((errnum = linted_mem_alloc(&xx, new_path_size)) != 0) {
            return errnum;
        }
        new_path = xx;
    }

    new_path[new_path_size - 1U] = '\0';
    memcpy(new_path, base, base_size);
    memcpy(new_path + base_size, pathname, pathname_size);

    *result = new_path;
    return 0;
}

static linted_error mutex_lock(linted_ko mutex_file)
{
    linted_error errnum = 0;
    size_t mutex_length = align_to_page_size(sizeof(pthread_mutex_t));

    pthread_mutex_t *mutex = mmap(NULL, mutex_length, PROT_READ | PROT_WRITE,
                                  MAP_SHARED, mutex_file, 0);
    if (MAP_FAILED == mutex) {
        errnum = errno;
        LINTED_ASSUME(errnum != 0);
        assert(errnum != EINVAL);
        return errnum;
    }

    if ((errnum = pthread_mutex_lock(mutex)) != 0) {
        if (EOWNERDEAD == errnum) {
            linted_error cons_error = pthread_mutex_consistent(mutex);
            assert(cons_error != EINVAL);
            assert(0 == cons_error);
        }
    }

    if (-1 == munmap(mutex, mutex_length)) {
        linted_error munmap_errnum = errno;
        assert(munmap_errnum != EINVAL);
        assert(false);
    }

    return errnum;
}

static linted_error mutex_unlock(linted_ko mutex_file)
{
    linted_error errnum = 0;
    size_t mutex_length = align_to_page_size(sizeof(pthread_mutex_t));

    pthread_mutex_t *mutex = mmap(NULL, mutex_length, PROT_READ | PROT_WRITE,
                                  MAP_SHARED, mutex_file, 0);
    if (MAP_FAILED == mutex) {
        errnum = errno;
        LINTED_ASSUME(errnum != 0);
        assert(errnum != EINVAL);
        return errnum;
    }

    if ((errnum = pthread_mutex_unlock(mutex)) != 0) {
        assert(errnum != EPERM);
        assert(errnum != EINVAL);
        assert(false);
    }

    if (-1 == munmap(mutex, mutex_length)) {
        linted_error munmap_errnum = errno;
        assert(munmap_errnum != EINVAL);
        assert(false);
    }

    return 0;
}

static size_t align_to_page_size(size_t size)
{
    size_t page_size = sysconf(_SC_PAGESIZE);

    /* This should always be true */
    assert(page_size > 0U);

    /* Round up to a multiple of page size */
    size_t remainder = size % page_size;
    if (0U == remainder)
        return size;

    return size - remainder + page_size;
}
