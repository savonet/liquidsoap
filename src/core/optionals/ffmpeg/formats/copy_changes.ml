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

type property =
  [ `Media
  | `Codec
  | `Bitstream_form
  | `Sample_rate
  | `Channel_layout
  | `Sample_format
  | `Width
  | `Height
  | `Pixel_format
  | `Extradata
  | `Profile
  | `Level
  | `Block_align ]

type t = string list

let default = []
let merge a b = List.sort_uniq compare (a @ b)
let of_container container = [Option.value ~default:"" container]
let show = function "" -> "?" | c -> c
let read = function "?" -> "" | c -> c
let value t = String.concat "+" (List.map show t)
let to_string = function [] -> "" | t -> "containers=" ^ value t

let parse_param label v =
  match label with
    | "containers" when v <> "" ->
        Some
          (List.sort_uniq compare (List.map read (String.split_on_char '+' v)))
    | _ -> None

let parse_serialized s =
  match String.index_opt s '=' with
    | None when s = "" -> Some []
    | Some i ->
        parse_param (String.sub s 0 i)
          (String.sub s (i + 1) (String.length s - i - 1))
    | None -> None

let in_band_video =
  [`Extradata; `Profile; `Level; `Width; `Height; `Pixel_format]

let in_band_adts = [`Profile; `Sample_rate; `Channel_layout]

let rule ~container ~codec ~form property =
  match (container, codec, form) with
    | ("mpegts" | "h264"), "h264", "annexb" when List.mem property in_band_video
      ->
        `In_band
    | ("mpegts" | "adts"), "aac", "in_band" when List.mem property in_band_adts
      ->
        `In_band
    | "mp3", "mp3", _ when List.mem property [`Sample_rate; `Channel_layout] ->
        `In_band
    | "ogg", _, _ when not (List.mem property [`Media; `Codec; `Bitstream_form])
      ->
        `Restart
    | _ -> `No

let allowed t ~codec ~form property =
  t <> []
  && List.for_all
       (fun container -> rule ~container ~codec ~form property <> `No)
       t
