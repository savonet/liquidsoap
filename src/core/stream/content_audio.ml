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

open Mm
open Content_base

(* The settings are core's, and this is where the two meet. *)
let () =
  Audio_format.channels :=
    fun () -> Lazy.Mutexed.force Frame_settings.audio_channels

module Data = struct
  open Frame_settings

  type params = Audio_format.Specs.params
  type data = Audio.Mono.buffer array

  let blit src src_pos dst dst_pos len =
    (* For some reason we're not getting a proper stack trace from
       this unless we re-raise. *)
    try
      let ( ! ) = audio_of_main in
      Array.iter2
        (fun src dst -> Audio.Mono.blit src !src_pos dst !dst_pos !len)
        src dst
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      Printexc.raise_with_backtrace exn bt

  let copy d = Audio.copy d 0 (Audio.length d)
  let params d = Audio_format.Specs.param_of_channels (Array.length d)

  let make ?(length = 0) { Audio_format.Specs.channel_layout } =
    let channels = Audio_format.Specs.channels_of_param !!channel_layout in
    Array.init channels (fun _ -> Audio.Mono.create (audio_of_main length))

  let length d = main_of_audio (Audio.length d)

  let checksum d =
    let len =
      Array.fold_left (fun acc c -> acc + (Audio.Mono.length c * 8)) 0 d
    in
    let buf = Bytes.create len in
    let pos = ref 0 in
    Array.iter
      (fun c ->
        for i = 0 to Audio.Mono.length c - 1 do
          let bits = Int64.bits_of_float c.(i) in
          Bytes.set_int64_le buf !pos bits;
          pos := !pos + 8
        done)
      d;
    Digest.bytes buf |> Digest.to_hex
end

include MkDataBase (Audio_format.Format) (Data)
include Audio_format
