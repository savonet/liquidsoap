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

#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>

#include <errno.h>
#include <string.h>

#include "duppy_core.h"

#ifdef _WIN32
#define Descriptor_val(_descriptor) Socket_val(_descriptor)
#else
#define Descriptor_val(_descriptor) Int_val(_descriptor)
#endif

#define Core_val(_core) (*(duppy_core **)Data_custom_val(_core))

/* One take is copied through the stack, so this bounds the worker's buffer. */
#define TAKE_CAPACITY 1024

static void finalize_core(value _core) {
  duppy_core *core = Core_val(_core);
  duppy_core_stop(core);
  duppy_core_free(core);
}

static struct custom_operations core_operations = {
    "liquidsoap.duppy.core",    finalize_core,
    custom_compare_default,     custom_hash_default,
    custom_serialize_default,   custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

CAMLprim value duppy_stub_now(value _unit) {
  (void)_unit;
  return caml_copy_double(duppy_core_now());
}

CAMLprim value duppy_stub_create(value _wake_read, value _wake_write,
                                 value _force_fallback) {
  CAMLparam0();
  CAMLlocal1(_core);
  duppy_core *core =
      duppy_core_create(Descriptor_val(_wake_read), Descriptor_val(_wake_write),
                        Bool_val(_force_fallback));
  if (core == NULL)
    caml_uerror("duppy_core_create", Nothing);
  _core = caml_alloc_custom(&core_operations, sizeof(duppy_core *), 0, 1);
  Core_val(_core) = core;
  CAMLreturn(_core);
}

CAMLprim value duppy_stub_backend(value _core) {
  return caml_copy_string(duppy_core_backend(Core_val(_core)));
}

CAMLprim value duppy_stub_start(value _core, value _worker_count,
                                value _max_blocking) {
  if (duppy_core_start(Core_val(_core), Int_val(_worker_count),
                       Int_val(_max_blocking)) != 0)
    caml_uerror("duppy_core_start", Nothing);
  return Val_unit;
}

/* The event thread never takes the runtime lock, but joining it is still a
   wait. */
CAMLprim value duppy_stub_stop(value _core) {
  duppy_core *core = Core_val(_core);
  caml_enter_blocking_section();
  duppy_core_stop(core);
  caml_leave_blocking_section();
  return Val_unit;
}

CAMLprim value duppy_stub_reserve(value _core, value _delta) {
  return Val_int(duppy_core_reserve(Core_val(_core), Int_val(_delta)));
}

CAMLprim value duppy_stub_slots(value _core) {
  return Val_int(duppy_core_slots(Core_val(_core)));
}

CAMLprim value duppy_stub_error(value _core) {
  return caml_copy_string(strerror(duppy_core_error(Core_val(_core))));
}

CAMLprim value duppy_stub_submit(value _core, value _handle, value _rank,
                                 value _task_class, value _pin,
                                 value _accepted_by, value _delay, value _fds,
                                 value _interests) {
  duppy_fd fds[DUPPY_MAX_FDS];
  int interests[DUPPY_MAX_FDS];
  size_t fd_count = Wosize_val(_fds);
  if (fd_count > DUPPY_MAX_FDS)
    caml_invalid_argument("Duppy: too many descriptors in one task");
  for (size_t i = 0; i < fd_count; i++) {
    fds[i] = Descriptor_val(Field(_fds, i));
    interests[i] = Int_val(Field(_interests, i));
  }
  duppy_task task = {Long_val(_handle),
                     Int_val(_rank),
                     Int_val(_task_class),
                     Int_val(_pin),
                     (uint64_t)Long_val(_accepted_by),
                     Double_val(_delay),
                     fd_count,
                     fds,
                     interests};
  if (duppy_core_submit(Core_val(_core), &task) != 0) {
    if (errno == ENOMEM)
      caml_raise_out_of_memory();
    caml_invalid_argument("Duppy: invalid task");
  }
  return Val_unit;
}

CAMLprim value duppy_stub_submit_bytecode(value *arguments, int count) {
  (void)count;
  return duppy_stub_submit(arguments[0], arguments[1], arguments[2],
                           arguments[3], arguments[4], arguments[5],
                           arguments[6], arguments[7], arguments[8]);
}

/* No allocation and no blocking section: the runtime cannot suspend the
   caller between the core's lock and unlock. */
CAMLprim value duppy_stub_take(value _core, value _worker, value _taken) {
  intptr_t taken[TAKE_CAPACITY];
  size_t capacity = Wosize_val(_taken);
  size_t written;
  if (capacity > TAKE_CAPACITY)
    capacity = TAKE_CAPACITY;
  duppy_work work = duppy_core_take(Core_val(_core), Int_val(_worker), taken,
                                    capacity, &written);
  for (size_t i = 0; i < written; i++)
    Field(_taken, i) = Val_long(taken[i]);
  return Val_long((written << 3) | work);
}

CAMLprim value duppy_stub_wait(value _core, value _worker) {
  duppy_core *core = Core_val(_core);
  int worker = Int_val(_worker);
  caml_enter_blocking_section();
  duppy_core_wait(core, worker);
  caml_leave_blocking_section();
  return Val_unit;
}

CAMLprim value duppy_stub_blocking_done(value _core, value _worker) {
  duppy_core_blocking_done(Core_val(_core), Int_val(_worker));
  return Val_unit;
}
