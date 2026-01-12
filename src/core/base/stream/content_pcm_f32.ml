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

open Content_base

module Specs = struct
  include Content_pcm_base

  type kind = [ `Pcm_f32 ]

  let kind = `Pcm_f32
  let kind_of_string = function "pcm_f32" -> Some `Pcm_f32 | _ -> None

  type data =
    (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t array

  let name = "pcm_f32"
  let string_of_kind = function `Pcm_f32 -> "pcm_f32"
  let copy = copy ~fmt:Bigarray.float32
  let make = make ~fmt:Bigarray.float32
end

include MkContentBase (Specs)

let kind = lift_kind `Pcm_f32
let clear = Content_pcm_base.clear_content ~v:0.
let from_audio c = Mm.Audio.to_ba c 0 (Mm.Audio.length c)
let to_audio = Mm.Audio.of_ba

let blit_audio src src_ofs dst dst_ofs len =
  Mm.Audio.copy_to_ba src src_ofs len
    (Array.map (fun dst -> Bigarray.Array1.sub dst dst_ofs len) dst)

let channels_of_format = Content_pcm_base.channels_of_format ~get_params

external amplify :
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  float ->
  unit = "liquidsoap_amplify_f32_ba"

let amplify c v = Array.iter (fun c -> amplify c v) c
