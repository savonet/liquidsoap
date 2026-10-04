/*
 * Copyright 2003-2026 Savonet team
 *
 * This file is part of Liquidsoap.
 *
 * Liquidsoap is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Liquidsoap is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details, fully stated in the COPYING
 * file at the root of the liquidsoap distribution.
 *
 * You should have received a copy of the GNU General Public License
 * along with Liquidsoap; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#ifndef DUPPY_CORE_H
#define DUPPY_CORE_H

/* The core of SPEC.md: neither this header nor duppy_core.c may include a
   header of the OCaml runtime. */

#include <stddef.h>
#include <stdint.h>

#ifdef _WIN32
#include <winsock2.h>
typedef SOCKET duppy_fd;
#else
typedef int duppy_fd;
#endif

#define DUPPY_READ 1
#define DUPPY_WRITE 2

#define DUPPY_RANKS 64
#define DUPPY_MAX_FDS 64
#define DUPPY_ANY_WORKER (-1)
#define DUPPY_EVERY_WORKER UINT64_MAX

typedef enum { DUPPY_IMMEDIATE, DUPPY_DIRECT, DUPPY_THREADED } duppy_class;

typedef enum {
  DUPPY_NONE,
  DUPPY_BATCH,
  DUPPY_ONE_DIRECT,
  DUPPY_ONE_THREADED,
  DUPPY_STOPPED,
  DUPPY_FAILED
} duppy_work;

typedef struct duppy_core duppy_core;

typedef struct {
  intptr_t handle;
  int rank;
  duppy_class task_class;
  /* The one worker that may take the task, or DUPPY_ANY_WORKER. */
  int pin;
  /* One bit per worker accepting the task, for the first 64 workers. */
  uint64_t accepted_by;
  /* Seconds from submission; negative for no deadline. */
  double delay;
  size_t fd_count;
  const duppy_fd *fds;
  /* A mask of DUPPY_READ and DUPPY_WRITE for each of fds. */
  const int *interests;
} duppy_task;

/* Monotonic seconds, the clock of every delay. */
double duppy_core_now(void);

/* The wake descriptors are a connected pair owned by the caller, whose write
   end must not block. Returns NULL with errno set on failure. */
duppy_core *duppy_core_create(duppy_fd wake_read, duppy_fd wake_write,
                              int force_fallback);

/* The core must be stopped, or never started. */
void duppy_core_free(duppy_core *core);

const char *duppy_core_backend(const duppy_core *core);

/* max_blocking is how many threaded tasks the pool may have taken and not
   reported done. Returns 0, or -1 with errno set. */
int duppy_core_start(duppy_core *core, int worker_count, int max_blocking);

/* Drops every task not yet taken, wakes every worker and joins the event
   thread. */
void duppy_core_stop(duppy_core *core);

/* Adds delta to the blocking budget and returns it. */
int duppy_core_reserve(duppy_core *core, int delta);

int duppy_core_slots(duppy_core *core);

/* Returns 0, or -1 with errno set: EINVAL for a task outside the limits above
   or pinned to a worker that is unknown or does not accept it. */
int duppy_core_submit(duppy_core *core, const duppy_task *task);

/* Each task taken is written to out as its handle, 1 if its delay elapsed or
   0, then the mask of events that occurred on each of its descriptors, so
   capacity must be at least DUPPY_MAX_FDS + 2.

   After DUPPY_NONE the worker must call duppy_core_wait before taking again;
   DUPPY_FAILED is returned once, to one worker, after the event thread died. */
duppy_work duppy_core_take(duppy_core *core, int worker, intptr_t *out,
                           size_t capacity, size_t *written);

void duppy_core_wait(duppy_core *core, int worker);

/* To be called when a task taken as DUPPY_ONE_THREADED returns. */
void duppy_core_blocking_done(duppy_core *core, int worker);

int duppy_core_error(duppy_core *core);

#endif
