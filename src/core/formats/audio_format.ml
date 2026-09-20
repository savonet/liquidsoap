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

let ( !! ) = Lazy.Mutexed.force

(* How many channels a format with no layout of its own gets: what the setting
   defaults to, and what a process that has no settings answers. *)
let default_channels = 2
let channels = ref (fun () -> default_channels)

module Specs = struct
  type kind = [ `Pcm ]

  type params = {
    channel_layout : [ `Mono | `Stereo | `Five_point_one ] Lazy.Mutexed.t;
  }

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

  let param_of_channels = function
    | 1 -> { channel_layout = Lazy.Mutexed.from_val `Mono }
    | 2 -> { channel_layout = Lazy.Mutexed.from_val `Stereo }
    | 6 -> { channel_layout = Lazy.Mutexed.from_val `Five_point_one }
    | _ -> raise Invalid

  let channels_of_param = function
    | `Mono -> 1
    | `Stereo -> 2
    | `Five_point_one -> 6

  let parse_param label value =
    match (label, value) with
      | "", "mono" -> Some { channel_layout = Lazy.Mutexed.from_val `Mono }
      | "", "stereo" -> Some { channel_layout = Lazy.Mutexed.from_val `Stereo }
      | "", "5.1" ->
          Some { channel_layout = Lazy.Mutexed.from_val `Five_point_one }
      | _ -> None

  let serialize_params = string_of_params
  let parse_params s = parse_param "" s
  let kind = `Pcm
  let default_params _ = param_of_channels (!channels ())
  let kind_of_string = function "audio" | "pcm" -> Some `Pcm | _ -> None

  let content_lang_typ =
    let open Liquidsoap_lang in
    Lang_core.record_t
      [
        ("channels", Type.make Type.Int);
        ("channel_layout", Type.make Type.String);
      ]

  let params_to_value ({ channel_layout } as p) =
    let open Liquidsoap_lang in
    let channels = channels_of_param (Lazy.Mutexed.force channel_layout) in
    let layout = string_of_params p in
    Lang_core.record
      [
        ("channels", Lang_core.mk (`Int channels));
        ("channel_layout", Lang_core.mk (`String layout));
      ]
end

module Format = MkFormatBase (Specs)
include Format

let kind = lift_kind `Pcm
let format_of_channels channels = lift_params (Specs.param_of_channels channels)

let channels_of_format p =
  Specs.(channels_of_param (Lazy.Mutexed.force (get_params p).channel_layout))
