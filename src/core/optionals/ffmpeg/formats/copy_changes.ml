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

type form = [ `Annex_b | `Length_prefixed | `Adts | `Raw_aac | `Other ]

let form_name = function
  | `Annex_b -> "annexb"
  | `Length_prefixed -> "length_prefixed"
  | `Adts -> "in_band"
  | `Raw_aac -> "raw"
  | `Other -> "none"

type container = [ `Named of string | `Unknown ]
type t = container list

let default = []
let merge a b = List.sort_uniq compare (a @ b)
let of_container = function Some name -> [`Named name] | None -> [`Unknown]
let show = function `Named name -> name | `Unknown -> "?"
let read = function "?" -> `Unknown | name -> `Named name
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
    | _ when property = `Block_align -> `In_band
    | `Named "mpegts", ("h264" | "hevc"), `Annex_b
    | `Named "h264", "h264", `Annex_b
    | `Named "hevc", "hevc", `Annex_b
      when List.mem property in_band_video ->
        `In_band
    | (`Named "mpegts", "aac", `Adts | `Named "adts", "aac", `Adts)
      when List.mem property in_band_adts ->
        `In_band
    | `Named "mp3", "mp3", _
      when List.mem property [`Sample_rate; `Channel_layout] ->
        `In_band
    | `Named "ogg", _, _
      when not (List.mem property [`Media; `Codec; `Bitstream_form]) ->
        `Restart
    | _ -> `No

let allowed t ~codec ~form property =
  t <> []
  && List.for_all
       (fun container -> rule ~container ~codec ~form property <> `No)
       t
