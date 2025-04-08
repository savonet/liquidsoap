/*
 * Copyright 2011 The Savonet Team
 *
 * This file is part of ocaml-mm.
 *
 * ocaml-mm is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-mm is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ocaml-mm; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * As a special exception to the GNU Library General Public License, you may
 * link, statically or dynamically, a "work that uses the Library" with a
 * publicly distributed version of the Library to produce an executable file
 * containing portions of the Library, and distribute that executable file under
 * terms of your choice, without any of the additional requirements listed in
 * clause 6 of the GNU Library General Public License. By "a publicly
 * distributed version of the Library", we mean either the unmodified Library as
 * distributed by The Savonet Team, or a modified version of the Library that is
 * distributed under the conditions defined in clause 3 of the GNU Library
 * General Public License. This exception does not however invalidate any other
 * reasons why the executable file might be covered by the GNU Library General
 * Public License.
 *
 */

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>

#include <arpa/inet.h>
#include <assert.h>

#include "image_rgb.h"

CAMLprim value caml_sdl_rgb_to32(value _rgb, value _surf, value shift) {
  CAMLparam3(_rgb, _surf, shift);
  /*
  int sr = Int_val(Field(shift, 0));
  int sg = Int_val(Field(shift, 1));
  int sb = Int_val(Field(shift, 2));
  */
  frame rgb;
  frame_of_value(_rgb, &rgb);
  uint32_t *surf = Caml_ba_data_val(_surf);
  int i, j;
  int w = rgb.width;
  int h = rgb.height;

  for (j = 0; j < h; j++)
    for (i = 0; i < w; i++)
      surf[j * w + i] = htonl(Int_pixel(&rgb, i, j)) >> 8;

  CAMLreturn(Val_unit);
}
