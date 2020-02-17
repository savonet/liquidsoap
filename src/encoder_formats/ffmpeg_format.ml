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

type opt_val = [ `String of string | `Int of int | `Float of float ]
type output = [ `Stream | `Url of string ]
type opts = (string, opt_val) Hashtbl.t

type t = {
  format : string option;
  output : output;
  channels : int;
  samplerate : int Lazy.t;
  framerate : int Lazy.t;
  width : int Lazy.t;
  height : int Lazy.t;
  audio_codec : string option;
  audio_opts : opts;
  video_codec : string option;
  video_opts : opts;
  other_opts : opts;
}

let string_of_options options =
  let _v = function
    | `String s -> Printf.sprintf "%S" s
    | `Int i -> string_of_int i
    | `Float f -> string_of_float f
  in
  String.concat ","
    (Hashtbl.fold
       (fun k v c ->
         let v = Printf.sprintf "%s=%s" k (_v v) in
         v :: c)
       options [])

let to_string m =
  let audio_codec =
    match m.audio_codec with
      | None -> ""
      | Some c -> Printf.sprintf ",audio_codec=%S" c
  in
  let video_codec =
    match m.video_codec with
      | None -> ""
      | Some c -> Printf.sprintf ",video_codec=%S" c
  in
  let opts = string_of_options m.other_opts in
  let audio_opts =
    if Hashtbl.length m.audio_opts > 0 then
      Printf.sprintf ",%%audio(%s)" (string_of_options m.audio_opts)
    else ""
  in
  let video_opts =
    if Hashtbl.length m.video_opts > 0 then
      Printf.sprintf ",%%video(%s)" (string_of_options m.video_opts)
    else ""
  in
  let format =
    match m.format with Some f -> Printf.sprintf ",format=%S" f | None -> ""
  in
  let output =
    match m.output with
      | `Stream -> ""
      | `Url path -> Printf.sprintf ",url=%S" path
  in
  Printf.sprintf
    "%%fmpeg(channels=%d,samplerate=%d,framerate=%d,width=%d,height=%d%s%s%s%s%s%s%s)"
    m.channels (Lazy.force m.samplerate) (Lazy.force m.framerate)
    (Lazy.force m.width) (Lazy.force m.height) audio_codec audio_opts
    video_codec video_opts
    (if opts = "" then "" else Printf.sprintf ",%s" opts)
    format output
