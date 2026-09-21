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

module Data = struct
  type params = Pcm_format.Shared.params

  type data =
    (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t array

  let params = Content_pcm_base.params
  let length = Content_pcm_base.length
  let blit = Content_pcm_base.blit
  let copy = Content_pcm_base.copy ~fmt:Bigarray.float32
  let make = Content_pcm_base.make ~fmt:Bigarray.float32

  let checksum d =
    let len =
      Array.fold_left (fun acc c -> acc + (Bigarray.Array1.dim c * 4)) 0 d
    in
    let buf = Bytes.create len in
    let pos = ref 0 in
    Array.iter
      (fun c ->
        for i = 0 to Bigarray.Array1.dim c - 1 do
          let bits = Int32.bits_of_float c.{i} in
          Bytes.set_int32_le buf !pos bits;
          pos := !pos + 4
        done)
      d;
    Digest.bytes buf |> Digest.to_hex
end

include MkDataBase (Pcm_format.F32.Format) (Data)
include Pcm_format.F32

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
