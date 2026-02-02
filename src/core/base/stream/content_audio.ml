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

module Specs = struct
  open Frame_settings

  type kind = [ `Pcm ]

  type params = {
    channel_layout : [ `Mono | `Stereo | `Five_point_one ] Lazy.t;
  }

  type data = Audio.Mono.buffer array

  let name = "pcm"
  let string_of_kind = function `Pcm -> "pcm"

  let string_of_params { channel_layout } =
    match !!channel_layout with
      | `Mono -> "mono"
      | `Stereo -> "stereo"
      | `Five_point_one -> "5.1"

  let merge p p' =
    assert (!!(p.channel_layout) = !!(p'.channel_layout));
    p

  let compatible p p' = !!(p.channel_layout) = !!(p'.channel_layout)

  let blit src src_pos dst dst_pos len =
    (* For some reason we're not getting a proper stack trace from
       this unless we re-raise. *)
    try
      let ( ! ) = audio_of_main in
      Audio.blit src !src_pos dst !dst_pos !len
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      Printexc.raise_with_backtrace exn bt

  let copy d = Audio.copy d 0 (Audio.length d)

  let param_of_channels = function
    | 1 -> { channel_layout = Lazy.from_val `Mono }
    | 2 -> { channel_layout = Lazy.from_val `Stereo }
    | 6 -> { channel_layout = Lazy.from_val `Five_point_one }
    | _ -> raise Invalid

  let channels_of_param = function
    | `Mono -> 1
    | `Stereo -> 2
    | `Five_point_one -> 6

  let parse_param label value =
    match (label, value) with
      | "", "mono" -> Some { channel_layout = Lazy.from_val `Mono }
      | "", "stereo" -> Some { channel_layout = Lazy.from_val `Stereo }
      | "", "5.1" -> Some { channel_layout = Lazy.from_val `Five_point_one }
      | _ -> None

  let params d = param_of_channels (Array.length d)
  let kind = `Pcm

  let default_params _ =
    param_of_channels (Lazy.force Frame_settings.audio_channels)

  let make ?(length = 0) { channel_layout } =
    let channels =
      match !!channel_layout with
        | `Mono -> 1
        | `Stereo -> 2
        | `Five_point_one -> 6
    in
    Array.init channels (fun _ -> Audio.Mono.create (audio_of_main length))

  let length d = main_of_audio (Audio.length d)
  let kind_of_string = function "audio" | "pcm" -> Some `Pcm | _ -> None

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

include MkContentBase (Specs)

let kind = lift_kind `Pcm

let format_of_channels = function
  | 1 -> lift_params { channel_layout = Lazy.from_val `Mono }
  | 2 -> lift_params { channel_layout = Lazy.from_val `Stereo }
  | 6 -> lift_params { channel_layout = Lazy.from_val `Five_point_one }
  | _ -> raise Invalid

let channels_of_format p =
  Specs.(channels_of_param (Lazy.force (get_params p).channel_layout))
