(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

type application = [`Voip | `Audio | `Restricted_lowdelay]

type bitrate = [`Auto | `Bitrate_max | `Bitrate of int]

type mode = VBR of bool (* Variable bitrate, constrained or not. *) | CBR

(* Constant bitrate. *)

type max_bandwidth =
  [`Narrow_band | `Medium_band | `Wide_band | `Super_wide_band | `Full_band]

type signal = [`Auto | `Voice | `Music]

type t = {
  application: application option;
  bitrate: bitrate;
  complexity: int option;
  channels: int;
  frame_size: float;
  max_bandwidth: max_bandwidth option;
  mode: mode;
  samplerate: int;
  signal: signal option;
  fill: int option;
  dtx: bool;
  phase_inversion: bool;
}

let string_of_bitrate = function
  | `Auto ->
      "birate=\"auto\","
  | `Bitrate_max ->
      "birate=\"max\","
  | `Bitrate b ->
      Printf.sprintf "bitrate=%d," b

let string_of_mode = function
  | CBR ->
      "vbr=\"none\""
  | VBR b ->
      Printf.sprintf "vbr=%S" (if b then "constrained" else "unconstrained")

let string_of_application = function
  | None ->
      ""
  | Some `Voip ->
      "application=\"voip\","
  | Some `Audio ->
      "application=\"audio\","
  | Some `Restricted_lowdelay ->
      "application=\"restricted_lowdelay\","

let string_of_bandwidth = function
  | None ->
      ""
  | Some `Narrow_band ->
      "max_bandwidth=\"narrow_band\","
  | Some `Medium_band ->
      "max_bandwidth=\"medium_band\","
  | Some `Wide_band ->
      "max_bandwidth=\"wide_band\","
  | Some `Super_wide_band ->
      "max_bandwidth=\"super_wide_band\","
  | Some `Full_band ->
      "max_bandwidth=\"full_band\","

let string_of_signal = function
  | None ->
      ""
  | Some `Auto ->
      "signal=\"auto\","
  | Some `Voice ->
      "signal=\"voice\","
  | Some `Music ->
      "signal=\"music\","

let to_string v =
  Printf.sprintf
    "%%opus(%s,%schannels=%d,%s%s%s%ssamplerate=%d,frame_size=%.02f,dtx=%B,phase_inversion=%b)"
    (string_of_mode v.mode)
    (string_of_bitrate v.bitrate)
    v.channels
    (string_of_application v.application)
    ( match v.complexity with
      | None ->
          ""
      | Some i ->
          Printf.sprintf "complexity=\"%d\"," i )
    (string_of_bandwidth v.max_bandwidth)
    (string_of_signal v.signal)
    v.samplerate v.frame_size v.dtx v.phase_inversion
