/*
 * Copyright 2013, 2014, 2015 Steven Stewart-Gallus
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 * implied.  See the License for the specific language governing
 * permissions and limitations under the License.
 */
#define _POSIX_C_SOURCE 200809L

#include "config.h"

#include "lntd/window.h"

#include "lntd/error.h"
#include "lntd/io.h"
#include "lntd/ko.h"
#include "lntd/mem.h"
#include "lntd/rpc.h"
#include "lntd/util.h"

#include <errno.h>
#include <signal.h>
#include <stdint.h>
#include <unistd.h>

lntd_error lntd_window_write(lntd_window window, uint_fast32_t in)
{
	lntd_error err;

	char buf[LNTD_RPC_UINT32_SIZE];
	lntd_rpc_pack_uint32(in, buf);

	/*
	 * From POSIX
	 *
	 * If write() is interrupted by a signal after it successfully
	 * writes some data, it shall return the number of bytes
	 * written.
	 *
	 * So, make writes atomic by preventing signals during the
	 * write.
	 */

	sigset_t oldset;
	sigfillset(&oldset);

	err = pthread_sigmask(SIG_BLOCK, &oldset, &oldset);
	if (err != 0)
		return err;

	if (-1 == pwrite(window, buf, sizeof buf, 0U)) {
		err = errno;
		LNTD_ASSUME(err != 0);
	}

	lntd_error restore_err =
	    pthread_sigmask(SIG_SETMASK, &oldset, 0);
	if (0 == err)
		err = restore_err;

	/*
	 * From POSIX
	 *
	 * Writes can be serialized with respect to other reads and
	 * writes. If a read() of file data can be proven (by any
	 * means) to occur after a write() of the data, it must
	 * reflect that write(), even if the calls are made by
	 * different processes. A similar requirement applies to
	 * multiple write operations to the same file position. This
	 * is needed to guarantee the propagation of data from write()
	 * calls to subsequent read() calls. This requirement is
	 * particularly significant for networked file systems, where
	 * some caching schemes violate these semantics.
	 *
	 * So, no fdatasync call needs to be made.
	 */

	return err;
}

lntd_error lntd_window_read(lntd_window window, uint_fast32_t *outp)
{
	lntd_error err;

	char buf[LNTD_RPC_UINT32_SIZE];

	sigset_t oldset;
	sigfillset(&oldset);

	err = pthread_sigmask(SIG_BLOCK, &oldset, &oldset);
	if (err != 0)
		return err;

	ssize_t bytes = pread(window, buf, sizeof buf, 0U);
	if (-1 == bytes) {
		err = errno;
		LNTD_ASSUME(err != 0);
	}

	lntd_error restore_err =
	    pthread_sigmask(SIG_SETMASK, &oldset, 0);
	if (0 == err)
		err = restore_err;

	if (err != 0)
		return err;

	if (bytes != sizeof buf)
		return EPROTO;

	*outp = lntd_rpc_unpack_uint32(buf);
	return 0;
}
