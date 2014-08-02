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

#include "linted/spawn.h"

#include "linted/error.h"
#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <grp.h>
#include <sched.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <sys/mount.h>
#include <sys/mman.h>
#include <sys/prctl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include <linux/capability.h>

#define INT_STRING_PADDING "XXXXXXXXXXXXXX"

enum file_action_type {
    FILE_ACTION_ADDDUP2
};

struct adddup2
{
    enum file_action_type type;
    int oldfildes;
    int newfildes;
};

union file_action
{
    enum file_action_type type;
    struct adddup2 adddup2;
};

struct linted_spawn_file_actions
{
    size_t action_count;
    union file_action actions[];
};

struct mount_args
{
    char *source;
    char *target;
    char *filesystemtype;
    unsigned long mountflags;
    char *data;
};

struct linted_spawn_attr
{
    char const *chrootdir;
    size_t mount_args_size;
    struct mount_args *mount_args;
    pid_t pgroup;
    bool setpgroup : 1U;
    bool drop_caps : 1U;
};

struct spawn_error
{
    linted_error errnum;
};

static unsigned long const capabilities[]
    = { CAP_CHOWN,           CAP_DAC_OVERRIDE,     CAP_DAC_READ_SEARCH,
        CAP_FOWNER,          CAP_FSETID,           CAP_KILL,
        CAP_SETGID,          CAP_SETUID,           CAP_SETPCAP,
        CAP_LINUX_IMMUTABLE, CAP_NET_BIND_SERVICE, CAP_NET_BROADCAST,
        CAP_NET_ADMIN,       CAP_NET_RAW,          CAP_IPC_LOCK,
        CAP_IPC_OWNER,       CAP_SYS_MODULE,       CAP_SYS_RAWIO,
        CAP_SYS_CHROOT,      CAP_SYS_PTRACE,       CAP_SYS_PACCT,
        CAP_SYS_ADMIN,       CAP_SYS_BOOT,         CAP_SYS_NICE,
        CAP_SYS_RESOURCE,    CAP_SYS_TIME,         CAP_SYS_TTY_CONFIG,
        CAP_MKNOD,           CAP_LEASE,            CAP_AUDIT_WRITE,
        CAP_AUDIT_CONTROL,   CAP_SETFCAP,          CAP_MAC_OVERRIDE,
        CAP_MAC_ADMIN,       CAP_SYSLOG,           CAP_WAKE_ALARM };

static int execveat(int dirfd, const char *filename, char *const argv[],
                    char *const envp[]);
static size_t align_to_page_size(size_t size);
static void exit_with_error(volatile struct spawn_error *spawn_error,
                            linted_error errnum);

linted_error linted_spawn_attr_init(struct linted_spawn_attr **attrp)
{
    linted_error errnum;
    struct linted_spawn_attr *attr;

    {
        void *xx;
        if ((errnum = linted_mem_alloc_zeroed(&xx, sizeof *attr)) != 0) {
            return errnum;
        }
        attr = xx;
    }

    *attrp = attr;
    return 0;
}

void linted_spawn_attr_drop_caps(struct linted_spawn_attr *attr)
{
    attr->drop_caps = true;
}

void linted_spawn_attr_destroy(struct linted_spawn_attr *attr)
{
    for (size_t ii = 0U; ii < attr->mount_args_size; ++ii) {
        struct mount_args *mount_arg = &attr->mount_args[ii];
        linted_mem_free(mount_arg->source);
        linted_mem_free(mount_arg->target);
        linted_mem_free(mount_arg->filesystemtype);
        linted_mem_free(mount_arg->data);
    }
    linted_mem_free(attr->mount_args);

    linted_mem_free(attr);
}

void linted_spawn_attr_setpgroup(struct linted_spawn_attr *attr, pid_t pgroup)
{
    attr->setpgroup = true;
    attr->pgroup = pgroup;
}

void linted_spawn_attr_setchrootdir(struct linted_spawn_attr *attr,
                                    char const *chrootdir)
{
    attr->chrootdir = chrootdir;
}

linted_error linted_spawn_attr_setmount(struct linted_spawn_attr *attr,
                                        char const *source, char const *target,
                                        char const *filesystemtype,
                                        unsigned long mountflags,
                                        char const *data)
{
    linted_error errnum = 0;
    struct mount_args *mount_args = attr->mount_args;
    size_t size = attr->mount_args_size;

    size_t new_size = size + 1U;
    if (new_size < size) {
        return ENOMEM;
    }
    size = new_size;

    char *source_copy;
    char *target_copy;
    char *filesystemtype_copy;
    char *data_copy;

    if (NULL == source) {
        source_copy = NULL;
    } else {
        source_copy = strdup(source);
        if (NULL == source_copy) {
            return errno;
        }
    }

    if (NULL == target) {
        target_copy = NULL;
    } else {
        target_copy = strdup(target);
        if (NULL == target_copy) {
            goto free_source;
        }
    }

    if (NULL == filesystemtype) {
        filesystemtype_copy = NULL;
    } else {
        filesystemtype_copy = strdup(filesystemtype);
        if (NULL == filesystemtype) {
            goto free_target;
        }
    }

    if (NULL == data) {
        data_copy = NULL;
    } else {
        data_copy = strdup(data);
        if (NULL == data_copy) {
            goto free_filesystemtype;
        }
    }

    {
        void *xx;
        if ((errnum = linted_mem_realloc_array(&xx, mount_args, size,
                                               sizeof mount_args[0U])) != 0) {
            goto free_data;
        }
        mount_args = xx;
    }

    if (errnum != 0) {
    free_data:
        linted_mem_free(data_copy);

    free_filesystemtype:
        linted_mem_free(filesystemtype_copy);

    free_target:
        linted_mem_free(target_copy);

    free_source:
        linted_mem_free(source_copy);
    } else {
        mount_args[size - 1U].source = source_copy;
        mount_args[size - 1U].target = target_copy;
        mount_args[size - 1U].filesystemtype = filesystemtype_copy;
        mount_args[size - 1U].mountflags = mountflags;
        mount_args[size - 1U].data = data_copy;

        attr->mount_args = mount_args;
        attr->mount_args_size = size;
    }
    return errnum;
}

linted_error linted_spawn_file_actions_init(struct linted_spawn_file_actions
                                            **file_actionsp)
{
    linted_error errnum;
    struct linted_spawn_file_actions *file_actions;

    {
        void *xx;
        if ((errnum = linted_mem_alloc_zeroed(&xx, sizeof *file_actions))
            != 0) {
            return errnum;
        }
        file_actions = xx;
    }

    *file_actionsp = file_actions;
    return 0;
}

linted_error linted_spawn_file_actions_adddup2(struct linted_spawn_file_actions
                                               **file_actionsp,
                                               int oldfildes, int newfildes)
{
    linted_error errnum;
    struct linted_spawn_file_actions *file_actions;
    struct linted_spawn_file_actions *new_file_actions;
    union file_action *new_action;
    size_t old_count;
    size_t new_count;

    file_actions = *file_actionsp;

    old_count = file_actions->action_count;
    new_count = old_count + 1U;
    {
        void *xx;
        if ((errnum = linted_mem_realloc(
                 &xx, file_actions,
                 sizeof *file_actions
                 + new_count * sizeof file_actions->actions[0U])) != 0) {
            return errnum;
        }
        new_file_actions = xx;
    }

    new_file_actions->action_count = new_count;

    new_action = &new_file_actions->actions[old_count];

    new_action->type = FILE_ACTION_ADDDUP2;
    new_action->adddup2.oldfildes = oldfildes;
    new_action->adddup2.newfildes = newfildes;

    *file_actionsp = new_file_actions;

    return 0;
}

void linted_spawn_file_actions_destroy(struct linted_spawn_file_actions
                                       *file_actions)
{
    linted_mem_free(file_actions);
}

linted_error linted_spawn(pid_t *childp, int dirfd, char const *filename,
                          struct linted_spawn_file_actions const *file_actions,
                          struct linted_spawn_attr const *attr,
                          char *const argv[], char *const envp[])
{
    linted_error errnum = 0;
    bool is_relative_path = filename[0U] != '/';
    bool at_fdcwd = AT_FDCWD == dirfd;
    if (is_relative_path && !at_fdcwd && dirfd < 0) {
        return EBADF;
    }

    /*
     * So adddup2 works use memory mapping instead of a pipe to
     * communicate an error.
     */
    size_t spawn_error_length = align_to_page_size(sizeof(struct spawn_error));

    volatile struct spawn_error *spawn_error
        = mmap(NULL, spawn_error_length, PROT_READ | PROT_WRITE,
               MAP_SHARED | MAP_ANONYMOUS, -1, 0);
    if (MAP_FAILED == spawn_error) {
        errnum = errno;
        assert(errnum != 0);
        return errnum;
    }

    pid_t child;
    {
        /*
         * To save stack space reuse the same sigset for the full set and
         * the old set.
         */
        sigset_t sigset;
        sigfillset(&sigset);

        pthread_sigmask(SIG_BLOCK, &sigset, &sigset);

        child = fork();

        if (-1 == child) {
            errnum = errno;
            assert(errnum != 0);
        }

        if (0 == child) {
            /*
             * Get rid of signal handlers so that they can't be called before
             * execve.
             */
            for (int ii = 1; ii < NSIG; ++ii) {
                /* Uncatchable, avoid Valgrind warnings */
                if (SIGSTOP == ii || SIGKILL == ii) {
                    continue;
                }

                struct sigaction action;
                sigaction(ii, NULL, &action);

                if (action.sa_handler != SIG_IGN) {
                    action.sa_handler = SIG_DFL;

                    sigaction(ii, &action, NULL);
                }
            }
        }

        pthread_sigmask(SIG_SETMASK, &sigset, NULL);
    }

    if (child != 0) {
        if (errnum != 0) {
            goto unmap_spawn_error;
        }

        if (attr != NULL) {
            if (attr->setpgroup) {
                if (-1 == setpgid(child, attr->pgroup)) {
                    errnum = errno;
                    assert(errnum != 0);

                    assert(errnum != EINVAL);
                    assert(EACCES == errnum || EPERM == errnum || ESRCH
                                                                  == errnum);
                    errnum = 0;
                }
            }
        }

    unmap_spawn_error:
        if (-1 == munmap((void *)spawn_error, spawn_error_length)) {
            if (0 == errnum) {
                errnum = errno;
                assert(errnum != 0);
            }
        }

        *childp = child;
        return errnum;
    }

    if (attr != NULL) {

        if (attr->setpgroup) {
            if (-1 == setpgid(0, attr->pgroup)) {
                errnum = errno;
                assert(errnum != 0);
                if (errnum != EACCES) {
                    exit_with_error(spawn_error, errnum);
                }
            }
        }

        if (attr->drop_caps) {
            /* Used ways to sandbox:
             *
             * - CLONE_NEWNS - Coupled with chrooting is good for
             *                 sandboxing files.
             *
             * - CLONE_NEWIPC - Nobody really uses System V IPC
             *                  objects anymore but maybe a few
             *                  applications on the system have some
             *                  for legacy communication purposes.
             *
             * - CLONE_NEWNET - Prevents processes from connecting to
             *                  open abstract sockets.
             *
             * Unused
             *
             * - CLONE_NEWUTS - Clones the hostname namespace. Pretty
             *                  useless.
             *
             * - CLONE_NEWUSER - Allows to create one's own users and
             *                   enable some more
             *                   sandboxes. Otherwise, it is pretty
             *                   useless. Not permitted to use under
             *                   the existing sandbox.
             *
             * - CLONE_NEWPID - Prevents processes from ptracing and
             *                  signalling other
             *                  processes. Unfortunately, PID 1 can't
             *                  send itself signals so this is
             *                  unusable for many applications.
             *
             */
            if (-1 == unshare(CLONE_NEWIPC | CLONE_NEWNET | CLONE_NEWNS)) {
                exit_with_error(spawn_error, errno);
            }

            if (-1 == mount(NULL, attr->chrootdir, "tmpfs", 0, NULL)) {
                exit_with_error(spawn_error, errno);
            }

            if (-1 == chdir(attr->chrootdir)) {
                exit_with_error(spawn_error, errno);
            }

            for (size_t ii = 0U; ii < attr->mount_args_size; ++ii) {
                struct mount_args *mount_arg = &attr->mount_args[ii];

                if (-1 == mkdir(mount_arg->target, S_IRWXU)) {
                    errnum = errno;
                    if (errnum != EEXIST) {
                        exit_with_error(spawn_error, errno);
                    }
                }

                if (-1 == mount(mount_arg->source, mount_arg->target,
                                mount_arg->filesystemtype,
                                mount_arg->mountflags, mount_arg->data)) {
                    exit_with_error(spawn_error, errno);
                }
            }

            if (-1 == mount(".", "/", NULL, MS_MOVE, NULL)) {
                exit_with_error(spawn_error, errno);
            }

            if (-1 == chroot(".")) {
                exit_with_error(spawn_error, errno);
            }

            if (-1 == chdir("/")) {
                exit_with_error(spawn_error, errno);
            }

            /* Favor other processes over this process hierarchy. Only
             * superuser may lower priorities so this is not stoppable. This
             * also makes the process hierarchy nicer for the OOM killer.
             */
            errno = 0;
            int priority = getpriority(PRIO_PROCESS, 0);
            if (-1 == priority) {
                errnum = errno;
                if (errnum != 0) {
                    exit_with_error(spawn_error, errnum);
                }
            }

            if (-1 == setpriority(PRIO_PROCESS, 0, priority + 1)) {
                exit_with_error(spawn_error, errno);
            }

            /* Drop all privileges I might possibly have. I'm not sure
             * I need to do this and I probably can do this in a
             * better way. Note that currently we do not use
             * PR_SET_KEEPCAPS and do not map our sandboxed user to
             * root but if we did in the future we would need this.
             */
            for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(capabilities); ++ii) {
                if (-1 == prctl(PR_CAPBSET_DROP, capabilities[ii], 0, 0, 0)) {
                    exit_with_error(spawn_error, errno);
                }
            }

            if (-1 == setgroups(0U, NULL)) {
                exit_with_error(spawn_error, errno);
            }
        }
    }

    /* Copy it in case it is overwritten */

    int dirfd_copy;

    if (at_fdcwd) {
        dirfd_copy = AT_FDCWD;
    } else if (is_relative_path && at_fdcwd) {
        if (-1 == (dirfd_copy = fcntl(dirfd, F_DUPFD_CLOEXEC, (long)0))) {
            exit_with_error(spawn_error, errno);
        }
    } else {
        dirfd_copy = -1;
    }

    if (file_actions != NULL) {
        for (size_t ii = 0U; ii < file_actions->action_count; ++ii) {
            union file_action const *action = &file_actions->actions[ii];
            switch (action->type) {
            case FILE_ACTION_ADDDUP2: {
                int newfildes = action->adddup2.newfildes;

                if (dirfd_copy >= 0 && dirfd_copy == newfildes) {
                    /* We don't need to close the old dirfd copy
                     * because it is closed by the following dup2.
                     */
                    dirfd_copy = fcntl(dirfd_copy, F_DUPFD_CLOEXEC, 0L);
                    if (-1 == dirfd_copy) {
                        exit_with_error(spawn_error, errno);
                    }
                }

                if (-1 == dup2(action->adddup2.oldfildes, newfildes)) {
                    exit_with_error(spawn_error, errno);
                }
                break;
            }

            default:
                exit_with_error(spawn_error, EINVAL);
            }
        }
    }

    execveat(dirfd_copy, filename, argv, envp);

    exit_with_error(spawn_error, errno);
    return 0;
}

static int execveat(int dirfd, const char *filename, char *const argv[],
                    char *const envp[])
{
    linted_error errnum;
    bool is_relative_path = filename[0U] != '/';
    bool at_fdcwd = AT_FDCWD == dirfd;
    char *new_path = NULL;

    if (is_relative_path && !at_fdcwd) {
        {
            void *xx;
            if ((errnum = linted_mem_alloc(&xx, strlen("/proc/self/fd/") + 10U
                                                + strlen(filename) + 1U))
                != 0) {
                errno = errnum;
                return -1;
            }
            new_path = xx;
        }
        sprintf(new_path, "/proc/self/fd/%i/%s", dirfd, filename);
        filename = new_path;
    }

    execve(filename, argv, envp);

    {
        errnum = errno;
        linted_mem_free(new_path);
        errno = errnum;
    }

    return -1;
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

static void exit_with_error(volatile struct spawn_error *spawn_error,
                            linted_error errnum)
{
    spawn_error->errnum = errnum;

    /* Stop the SIG_IGN handler from catching SIGTERM */
    {
        struct sigaction action;
        memset(&action, 0, sizeof action);
        action.sa_handler = SIG_DFL;

        sigaction(SIGTERM, &action, NULL);
    }

    /* Unlock SIGTERM */
    {
        sigset_t termset;
        sigemptyset(&termset);
        sigaddset(&termset, SIGTERM);

        pthread_sigmask(SIG_UNBLOCK, &termset, NULL);
    }

    raise(SIGTERM);

    assert(false);
}
