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

type t = [ `Unspecified | `In_band | `Stored ]

let default = `Unspecified

let merge a b =
  match (a, b) with
    | `Stored, _ | _, `Stored -> `Stored
    | `In_band, _ | _, `In_band -> `In_band
    | `Unspecified, `Unspecified -> `Unspecified

let headers = function
  | `In_band -> `Ignore
  | `Unspecified | `Stored -> `Compare

let name = function `In_band -> "in_band" | `Stored -> "stored"

let to_string = function
  | `Unspecified -> ""
  | (`In_band | `Stored) as h -> "header=" ^ name h

let parse_param label value =
  match (label, value) with
    | "header", "in_band" -> Some `In_band
    | "header", "stored" -> Some `Stored
    | _ -> None

let parse_serialized = function
  | "" -> Some `Unspecified
  | "header=in_band" -> Some `In_band
  | "header=stored" -> Some `Stored
  | _ -> None

let carries_header_in_band muxer =
  List.mem muxer ["mpegts"; "adts"; "h264"; "hevc"; "mp3"]

let of_container = function
  | Some muxer when carries_header_in_band muxer || muxer = "ogg" -> `In_band
  | Some _ | None -> `Stored
