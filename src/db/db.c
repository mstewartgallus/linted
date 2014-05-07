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
#include "config.h"

#include "linted/db.h"

#include "linted/error.h"
#include "linted/io.h"
#include "linted/ko.h"

#include <fcntl.h>
#include <semaphore.h>
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
#define GLOBAL_LOCK_TEMPORARY "global.lock.tmp"

#define FIELD_DIR "fields"
#define TEMP_DIR "temp"

static linted_error lock_db(linted_db* dbp);
static linted_error unlock_db(linted_db* dbp);

static linted_error prepend(char** result, char const* base, char const* end);

static linted_error fname_alloc(int fd, char** buf);
static linted_error fname(int fd, char* buf, size_t* sizep);

linted_error linted_db_open(linted_db* dbp, char const* pathname, int flags)
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
        if (-1 == mkdir(pathname, S_IRWXU)) {
            errnum = errno;
            if (errnum != EEXIST) {
                return errnum;
            }
        }
    }

    int o_flags = O_DIRECTORY | O_CLOEXEC;

    linted_db the_db = open(pathname, o_flags);
    if (-1 == the_db) {
        return errno;
    }

/*
 * This happens at startup and has to be consistent across every
 * version of the database.
 */

try_to_open_lock_again:
    ;
    linted_error lock_errnum = lock_db(&the_db);
    if (lock_errnum != 0) {
        if (lock_errnum != ENOENT) {
            errnum = lock_errnum;
            goto close_db;
        }

        /* Lock does not exist try to create it */
        /*
         * Note that using a constant temporary filename is not
         * incorrect just slow.
         */
        int temp_file
            = openat(the_db, GLOBAL_LOCK_TEMPORARY,
                     O_RDWR | O_SYNC | O_CREAT | O_EXCL, S_IRUSR | S_IWUSR);
        if (-1 == temp_file) {
            linted_error open_errnum = errno;
            if (EEXIST == open_errnum) {
                goto try_to_open_lock_again;
            } else {
                errnum = open_errnum;
                goto close_db;
            }
        }

        if (-1 == ftruncate(temp_file, sizeof(sem_t))) {
            errnum = errno;
            linted_ko_close(temp_file);
            goto unlink_temp;
        }

        size_t page_size = sysconf(_SC_PAGESIZE);
        size_t map_size = ((sizeof(sem_t) + page_size - 1) / page_size)
                          * page_size;

        sem_t* semaphore = mmap(NULL, map_size, PROT_READ | PROT_WRITE,
                                MAP_SHARED, temp_file, 0);
        if (MAP_FAILED == semaphore) {
            errnum = errno;
            linted_ko_close(temp_file);
            goto unlink_temp;
        }

        if ((errnum = linted_ko_close(temp_file)) != 0) {
            munmap(semaphore, map_size);
            goto unlink_temp;
        }

        if (-1 == sem_init(semaphore, true, 1)) {
            errnum = errno;
            goto unlink_temp;
        }

        if (-1 == munmap(semaphore, map_size)) {
            errnum = errno;
            goto unlink_temp;
        }

        if (-1
            == linkat(the_db, GLOBAL_LOCK_TEMPORARY, the_db, GLOBAL_LOCK, 0)) {
            errnum = errno;
        }

    unlink_temp:
        if (-1 == unlinkat(the_db, GLOBAL_LOCK_TEMPORARY, 0)) {
            if (0 == errnum) {
                errnum = errno;
            }
        }

        if (EEXIST == errnum) {
            goto try_to_open_lock_again;
        }

        if (errnum != 0) {
            goto close_db;
        }

        /* File created try to lock it */
        goto try_to_open_lock_again;
    }

    /* Sole user of the database now */
    int version_file = openat(the_db, "version", O_RDONLY | O_CLOEXEC);
    if (version_file != -1) {
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

        char* const version_text = malloc(version_file_size);
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

        if (memcmp(version_text, CURRENT_VERSION, sizeof CURRENT_VERSION - 1)
            != 0) {
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
    }
    } else {
        linted_error open_errnum = errno;
        if (open_errnum != ENOENT) {
            errnum = open_errnum;
            goto unlock_semaphore;
        }

        /* Creating the initial database */
        if (!db_creat) {
            errnum = EINVAL;
            goto unlock_semaphore;
        }

        int version_file_write = openat(
            the_db, "version", O_RDWR | O_SYNC | O_CLOEXEC | O_CREAT | O_EXCL,
            S_IRUSR | S_IWUSR);
        if (-1 == version_file_write) {
            errnum = errno;
            goto unlock_semaphore;
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
            goto unlock_semaphore;
        }

        if (-1 == mkdirat(the_db, TEMP_DIR, S_IRWXU)) {
            errnum = errno;
            goto unlock_semaphore;
        }

        if (-1 == mkdirat(the_db, FIELD_DIR, S_IRWXU)) {
            errnum = errno;
            goto unlock_semaphore;
        }
    }

unlock_semaphore:
    unlock_db(&the_db);

    if (errnum != 0) {
        goto close_db;
    }

    *dbp = the_db;
    return 0;

close_db:
    linted_db_close(&the_db);
    return errnum;
}

linted_error linted_db_close(linted_db* dbp)
{
    return linted_ko_close(*dbp);
}

linted_error linted_db_temp_file(linted_db* dbp, int* fildesp)
{
    linted_error errnum = 0;

    static char const field_name[] = "field-XXXXXX.tmp";

    char* temp_path;
    if ((errnum = prepend(&temp_path, TEMP_DIR "/", field_name)) != 0) {
        return errnum;
    }

    char* xxxxxx = temp_path + strlen(temp_path) + sizeof "field-"
                   - sizeof field_name;

try_again:
    for (size_t ii = 0; ii < 6; ++ii) {
        for (;;) {
            char value = ((int)(unsigned char)rand()) - 255 / 2 - 1;
            if ((value > 'a' && value < 'z') || (value > 'A' && value < 'Z')
                || (value > '0' && value < '9')) {
                xxxxxx[ii] = value;
                break;
            }
        }
    }

    int temp_field = openat(*dbp, temp_path,
                            O_RDWR | O_SYNC | O_CLOEXEC | O_CREAT | O_EXCL,
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

linted_error linted_db_temp_send(linted_db* dbp, char const* name, int tmp)
{
    linted_error errnum;

    char* temp_path;
    if ((errnum = fname_alloc(tmp, &temp_path)) != 0) {
        return errnum;
    }

    char* field_path;
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

static linted_error lock_db(linted_db* dbp)
{
    linted_error errnum;

    int lock_file = openat(*dbp, GLOBAL_LOCK, O_RDWR | O_SYNC | O_CLOEXEC);
    if (-1 == lock_file) {
        return errno;
    }

    size_t page_size = sysconf(_SC_PAGESIZE);
    size_t map_size = ((sizeof(sem_t) + page_size - 1) / page_size) * page_size;

    sem_t* semaphore = mmap(NULL, map_size, PROT_READ | PROT_WRITE, MAP_SHARED,
                            lock_file, 0);
    if (MAP_FAILED == semaphore) {
        errnum = errno;
        linted_ko_close(lock_file);
        return errnum;
    }

    if ((errnum = linted_ko_close(lock_file)) != 0) {
        munmap(semaphore, map_size);
        return errnum;
    }

    /* Lock */
    linted_error wait_errnum;
    do {
        int wait_status = sem_wait(semaphore);
        wait_errnum = -1 == wait_status ? errno : 0;
    } while (EINTR == wait_errnum);

    munmap(semaphore, map_size);

    return 0;
}

static linted_error unlock_db(linted_db* dbp)
{
    linted_error errnum;

    int lock_file = openat(*dbp, GLOBAL_LOCK, O_RDWR | O_SYNC | O_CLOEXEC);
    if (-1 == lock_file) {
        return errno;
    }

    size_t page_size = sysconf(_SC_PAGESIZE);
    size_t map_size = ((sizeof(sem_t) + page_size - 1) / page_size) * page_size;

    sem_t* semaphore = mmap(NULL, map_size, PROT_READ | PROT_WRITE, MAP_SHARED,
                            lock_file, 0);
    if (MAP_FAILED == semaphore) {
        errnum = errno;
        linted_ko_close(lock_file);
        return errnum;
    }

    if ((errnum = linted_ko_close(lock_file)) != 0) {
        munmap(semaphore, map_size);
        return errnum;
    }

    /* Unlock */
    sem_post(semaphore);

    munmap(semaphore, map_size);

    return 0;
}

static linted_error prepend(char** result, char const* base,
                            char const* pathname)
{
    size_t base_size = strlen(base);
    size_t pathname_size = strlen(pathname);

    size_t new_path_size = base_size + pathname_size + 1;

    char* new_path = malloc(new_path_size);
    if (NULL == new_path) {
        return errno;
    }

    new_path[new_path_size - 1] = '\0';
    memcpy(new_path, base, base_size);
    memcpy(new_path + base_size, pathname, pathname_size);

    *result = new_path;
    return 0;
}

static int fname_alloc(int fd, char** bufp)
{
    int errnum;
    size_t bytes_wrote;

    size_t buf_size = 40;

    char* buf = malloc(buf_size);
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
        char* newbuf = realloc(buf, buf_size);
        if (NULL == newbuf) {
            errnum = errno;
            goto free_buf;
        }
        buf = newbuf;
    }

    /* Save on excess memory, also give debugging allocators more
     * information.
     */
    char* newbuf = realloc(buf, bytes_wrote + 1);
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

static int fname(int fd, char* buf, size_t* sizep)
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

    char* int_end = fd_path + sizeof proc_self_fd;

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
