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
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>

#include <assert.h>
#include <sys/ioctl.h>
#include <sys/soundcard.h>

CAMLprim value caml_oss_dsp_setfmt(value fd, value fmt) {
  int f = Int_val(fmt);

  /* TODO: raise errors */
  /* TODO: use format constants */
  assert(ioctl(Int_val(fd), SNDCTL_DSP_SETFMT, &f) != -1);

  return Val_int(f);
}

CAMLprim value caml_oss_dsp_channels(value fd, value chans) {
  int c = Int_val(chans);

  assert(ioctl(Int_val(fd), SNDCTL_DSP_CHANNELS, &c) != -1);

  return Val_int(c);
}

CAMLprim value caml_oss_dsp_speed(value fd, value speed) {
  int s = Int_val(speed);

  assert(ioctl(Int_val(fd), SNDCTL_DSP_SPEED, &s) != -1);

  return Val_int(s);
}
