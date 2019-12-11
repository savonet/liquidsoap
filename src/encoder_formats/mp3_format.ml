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

type stereo_mode = Default | Stereo | Joint_stereo

type abr = {
  min_bitrate: int option;
  mean_bitrate: int;
  max_bitrate: int option;
  hard_min: bool;
}

let string_of_abr x =
  let f v x =
    match x with Some x -> Printf.sprintf "%s=%i," v x | None -> ""
  in
  Printf.sprintf "bitrate=%d,%s%shard_min=%b" x.mean_bitrate
    (f "min_bitrate" x.min_bitrate)
    (f "max_bitrate" x.max_bitrate)
    x.hard_min

type bitrate_control = ABR of abr | VBR of int | CBR of int

let string_of_bitrate_control = function
  | ABR abr ->
      string_of_abr abr
  | VBR q ->
      Printf.sprintf "quality=%d" q
  | CBR br ->
      Printf.sprintf "bitrate=%d" br

type id3v2_export = Meta_format.export_metadata -> string

type t = {
  stereo: bool;
  stereo_mode: stereo_mode;
  bitrate_control: bitrate_control;
  internal_quality: int;
  samplerate: int Lazy.t;
  id3v2: id3v2_export option;
  msg_interval: float;
  msg: string;
}

let id3v2_export : id3v2_export option ref = ref None

let to_string m =
  let name =
    match m.bitrate_control with
      | VBR _ ->
          "%mp3.vbr"
      | ABR _ ->
          "%mp3.abr"
      | CBR _ ->
          "%mp3"
  in
  Printf.sprintf "%s(%s,%s,samplerate=%d,id3v2=%b)" name
    (Encoder_formats.string_of_stereo m.stereo)
    (string_of_bitrate_control m.bitrate_control)
    (Lazy.force m.samplerate) (m.id3v2 <> None)

let bitrate m =
  match m.bitrate_control with
    | VBR _ ->
        raise Not_found
    | CBR n ->
        n * 1000
    | ABR abr ->
        abr.mean_bitrate * 1000
