(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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

  let string_of_kind = function `Pcm_f32 -> "pcm_f32"
  let copy = copy ~fmt:Bigarray.float32
  let make = make ~fmt:Bigarray.float32
end

include MkContentBase (Specs)

let kind = lift_kind `Pcm_f32
let clear = Content_pcm_base.clear_content ~v:0.
let to_value (v : float) = v [@@inline always]
let of_value (v : float) = v [@@inline always]
let from_audio = Content_pcm_base.from_audio ~to_value ~fmt:Bigarray.float32
let to_audio = Content_pcm_base.to_audio ~of_value
let blit_audio = Content_pcm_base.blit_audio ~to_value
let channels_of_format = Content_pcm_base.channels_of_format ~get_params

external amplify :
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  float ->
  unit = "liquidsoap_amplify_f32_ba"

let amplify c v = Array.iter (fun c -> amplify c v) c
