/*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************/

#include <caml/version.h>

#if OCAML_VERSION_MAJOR >= 5
#define CAML_INTERNALS
#endif

#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/weak.h>

#if OCAML_VERSION_MAJOR >= 5
#include <caml/major_gc.h>

/* In OCaml 5, marking is active in all phases except Phase_sweep_ephe. */
Caml_inline int marking_started(void)
{
  return caml_gc_phase != Phase_sweep_ephe;
}
#endif

/* Read key i (0-based) from weak array w without allocating.
   Returns 1 and sets *out to the live value, or 0 if the slot is dead.
   On OCaml 5: uses Ephe_key (atomic read, handles caml_ephe_locked), then
   darkens the value during marking to preserve the tri-color invariant. */
static int weak_get(value w, mlsize_t i, value *out)
{
#if OCAML_VERSION_MAJOR >= 5
  value v = Ephe_key(w, CAML_EPHE_FIRST_KEY + i);
  if (v == caml_ephe_none)
    return 0;
  if (marking_started())
    caml_darken(Caml_state, v, 0);
  *out = v;
  return 1;
#else
  return caml_ephemeron_get_key(w, i, out);
#endif
}

static mlsize_t weak_num_keys(value w)
{
#if OCAML_VERSION_MAJOR >= 5
  return Wosize_val(w) - CAML_EPHE_FIRST_KEY;
#else
  return caml_ephemeron_num_keys(w);
#endif
}

CAMLprim value caml_weak_utils_iter(value w, value f)
{
  CAMLparam2(w, f);
  CAMLlocal1(el);
  mlsize_t n = weak_num_keys(w);
  for (mlsize_t i = 0; i < n; i++) {
    if (weak_get(w, i, &el))
      caml_callback(f, el);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value caml_weak_utils_fold_left(value f, value init, value w)
{
  CAMLparam3(f, init, w);
  CAMLlocal2(acc, el);
  acc = init;
  mlsize_t n = weak_num_keys(w);
  for (mlsize_t i = 0; i < n; i++) {
    if (weak_get(w, i, &el))
      acc = caml_callback2(f, acc, el);
  }
  CAMLreturn(acc);
}
