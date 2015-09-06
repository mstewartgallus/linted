/*
 * Copyright 2015 Steven Stewart-Gallus
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
#include "config.h"

#include "linted/error.h"
#include "linted/log.h"
#include "linted/mem.h"
#include "linted/start.h"

#include <math.h>
#include <stdlib.h>

#include <pulse/context.h>
#include <pulse/error.h>
#include <pulse/mainloop.h>
#include <pulse/proplist.h>
#include <pulse/stream.h>

#define A_TONE 440
#define SAMPLE_RATE 44100U

static short *sampledata;
static size_t sampledata_size;

static double square_wave(size_t ii, double freq, double amplitude,
                          double sample_rate)
{
	double frequency = freq / (double)sample_rate;
	size_t period = 1 / frequency;

	return amplitude * ((ii % period + (period / 2U)) / period);
}

static double triangle_wave(size_t ii, double freq, double amplitude,
                            double sample_rate)
{
	double frequency = freq / (double)sample_rate;
	size_t period = 1 / frequency;

	return amplitude * (ii % period) / (double)period;
}

static double sin_wave(size_t ii, double freq, double amplitude,
                       double sample_rate)
{
	double frequency = freq / (double)sample_rate;
	double tau = 6.28318530718;

	return amplitude * sin(tau * frequency * ii);
}

static pa_sample_spec test_sample_spec = {
    .format = PA_SAMPLE_S16LE, .rate = SAMPLE_RATE, .channels = 1U};

static void on_notify(pa_context *c, void *userdata);

static void on_ok_to_write(pa_stream *s, size_t nbytes, void *userdata);

static struct linted_start_config const linted_start_config = {
    .canonical_process_name = PACKAGE_NAME "-audio", 0};

static unsigned char linted_start_main(char const *const process_name,
                                       size_t argc,
                                       char const *const argv[])
{
	size_t greatest_period = 1 / ((A_TONE / 1000.0) / SAMPLE_RATE);

	{
		void *xx;
		linted_error err = linted_mem_alloc_array_zeroed(
		    &xx, greatest_period, sizeof sampledata[0U]);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_mem_alloc_array: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		sampledata = xx;
	}
	sampledata_size = greatest_period;

	for (size_t ii = 0U; ii < sampledata_size; ++ii)
		sampledata[ii] =
		    triangle_wave(ii, A_TONE, 8000, SAMPLE_RATE) *
		    square_wave(ii, A_TONE / 100.0, 1, SAMPLE_RATE) *
		    sin_wave(ii, A_TONE / 1000.0, 1, SAMPLE_RATE);
	linted_log(LINTED_LOG_INFO, "array filled");

	pa_mainloop *mainloop = pa_mainloop_new();
	if (0 == mainloop) {
		linted_log(LINTED_LOG_ERROR, "pa_mainloop_new");
		return EXIT_FAILURE;
	}

	pa_proplist *proplist = pa_proplist_new();
	if (0 == proplist) {
		linted_log(LINTED_LOG_ERROR, "pa_proplist_new");
		return EXIT_FAILURE;
	}

	pa_proplist_sets(proplist, "PULSE_PROP_media.role", "game");

	pa_context *context = pa_context_new_with_proplist(
	    pa_mainloop_get_api(mainloop), PACKAGE_NAME, proplist);
	if (0 == context) {
		linted_log(LINTED_LOG_ERROR, "pa_context_new");
		return EXIT_FAILURE;
	}

	pa_context_set_state_callback(context, on_notify, 0);

	int err = pa_context_connect(context, 0, PA_CONTEXT_NOAUTOSPAWN,
	                             NULL);
	if (err < 0) {
		linted_log(LINTED_LOG_ERROR, "pa_context_connect: %s",
		           pa_strerror(-err));
		return EXIT_FAILURE;
	}

	int retval;
	{
		int xx;
		int pulse_err = pa_mainloop_run(mainloop, &xx);
		if (pulse_err < 0) {
			linted_log(LINTED_LOG_ERROR,
			           "pa_mainloop_run: %s",
			           pa_strerror(-pulse_err));
			return EXIT_FAILURE;
		}
		retval = xx;
	}

	pa_context_unref(context);
	pa_proplist_free(proplist);
	pa_mainloop_free(mainloop);

	return retval;
}

static void on_notify(pa_context *c, void *userdata)
{
	switch (pa_context_get_state(c)) {
	case PA_CONTEXT_UNCONNECTED:
		linted_log(LINTED_LOG_INFO, "unconnected\n");
		break;

	case PA_CONTEXT_CONNECTING:
		linted_log(LINTED_LOG_INFO, "connecting\n");
		break;

	case PA_CONTEXT_AUTHORIZING:
		linted_log(LINTED_LOG_INFO, "authorizing\n");
		break;

	case PA_CONTEXT_SETTING_NAME:
		linted_log(LINTED_LOG_INFO, "setting_name\n");
		break;

	case PA_CONTEXT_READY: {
		linted_log(LINTED_LOG_INFO, "ready\n");

		pa_stream *stream = pa_stream_new(c, "Background Music",
		                                  &test_sample_spec, 0);
		if (0 == stream) {
			linted_log(LINTED_LOG_ERROR,
			           "pa_context_connect: %s",
			           pa_strerror(pa_context_errno(c)));
			exit(EXIT_FAILURE);
		}

		pa_stream_set_write_callback(stream, on_ok_to_write, 0);

		int err;

		{
			pa_buffer_attr buffer_attr = {0};

			unsigned latency = 20000U;

			buffer_attr.maxlength = pa_usec_to_bytes(
			    latency, &test_sample_spec);
			buffer_attr.minreq =
			    pa_usec_to_bytes(0, &test_sample_spec);
			buffer_attr.prebuf = -1;
			buffer_attr.tlength = pa_usec_to_bytes(
			    latency, &test_sample_spec);

			err = pa_stream_connect_playback(
			    stream, 0, &buffer_attr,
			    PA_STREAM_INTERPOLATE_TIMING |
			        PA_STREAM_ADJUST_LATENCY |
			        PA_STREAM_AUTO_TIMING_UPDATE,
			    0, 0);
		}
		if (err < 0) {
			linted_log(LINTED_LOG_ERROR,
			           "pa_context_connect: %s",
			           pa_strerror(-err));
			exit(EXIT_FAILURE);
		}

		break;
	}

	case PA_CONTEXT_FAILED:
		linted_log(LINTED_LOG_INFO, "failed\n");
		break;

	case PA_CONTEXT_TERMINATED:
		linted_log(LINTED_LOG_WARNING, "terminated\n");
		break;
	}
}

static void on_ok_to_write(pa_stream *s, size_t nbytes, void *userdata)
{
	static size_t sampleoffs = 0U;

	if (sampleoffs * sizeof sampledata[0U] + nbytes >
	    sampledata_size * sizeof sampledata[0U])
		sampleoffs = 0U;

	if (nbytes > sampledata_size * sizeof sampledata[0U])
		nbytes = sampledata_size * sizeof sampledata[0U];

	int err = pa_stream_write(s, &sampledata[sampleoffs], nbytes,
	                          NULL, 0LL, PA_SEEK_RELATIVE);
	if (err < 0) {
		linted_log(LINTED_LOG_ERROR, "pa_stream_write: %s",
		           pa_strerror(-err));
		exit(EXIT_FAILURE);
	}

	sampleoffs += nbytes / sizeof sampledata[0U];
}
