(*****************************************************************************

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

 *****************************************************************************)

include
  Content_base.Content
    with type kind = [ `Pcm_f32 ]
     and type params = Content_audio.params
     and type data =
      (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t array

val kind : Content_base.kind
val clear : data -> int -> int -> unit
val from_audio : Content_audio.data -> data
val to_audio : data -> Content_audio.data
val blit_audio : Content_audio.data -> int -> data -> int -> int -> unit
val channels_of_format : Content_base.format -> int
val amplify : data -> float -> unit
