(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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

type opt_val =
  [ `String of string | `Int of int | `Int64 of int64 | `Float of float ]

type output = [ `Stream | `Url of string ]
type opts = (string, opt_val) Hashtbl.t
type codec = [ `Copy | `Raw of string option | `Internal of string option ]
type hwaccel = [ `None | `Auto ]

type t = {
  format : string option;
  output : output;
  channels : int;
  samplerate : int Lazy.t;
  framerate : int Lazy.t;
  width : int Lazy.t;
  height : int Lazy.t;
  pixel_format : string option;
  hwaccel : hwaccel;
  hwaccel_device : string option;
  audio_codec : codec option;
  sample_format : string option;
  audio_opts : opts;
  video_codec : codec option;
  video_opts : opts;
  other_opts : opts;
}

let string_of_options
    (options : (string, [< `Var of string | opt_val ]) Hashtbl.t) =
  let _v = function
    | `Var v -> v
    | `String s -> Printf.sprintf "%S" s
    | `Int i -> string_of_int i
    | `Int64 i -> Int64.to_string i
    | `Float f -> string_of_float f
  in
  String.concat ","
    (Hashtbl.fold
       (fun k v c ->
         let v = Printf.sprintf "%s=%s" k (_v v) in
         v :: c)
       options [])

let to_string m =
  let opts = [] in
  let opts =
    if Hashtbl.length m.other_opts > 0 then
      string_of_options m.other_opts :: opts
    else opts
  in
  let opts =
    match m.video_codec with
      | None -> opts
      | Some `Copy -> "%video.copy" :: opts
      | Some (`Raw (Some c)) | Some (`Internal (Some c)) ->
          let video_opts =
            Hashtbl.fold
              (fun lbl v h ->
                Hashtbl.add h lbl (v :> [ `Var of string | opt_val ]);
                h)
              m.video_opts (Hashtbl.create 10)
          in
          Hashtbl.add video_opts "codec" (`String c);
          Hashtbl.add video_opts "framerate" (`Int (Lazy.force m.framerate));
          Hashtbl.add video_opts "width" (`Int (Lazy.force m.width));
          Hashtbl.add video_opts "height" (`Int (Lazy.force m.height));
          Hashtbl.add video_opts "hwaccel"
            (`Var (match m.hwaccel with `None -> "none" | `Auto -> "auto"));
          Hashtbl.add video_opts "hwaccel_device"
            (match m.hwaccel_device with
              | None -> `Var "none"
              | Some d -> `String d);
          let name =
            match m.video_codec with
              | Some (`Raw _) -> "video.raw"
              | _ -> "video"
          in
          Printf.sprintf "%%%s(%s)" name (string_of_options video_opts) :: opts
      | Some (`Raw None) -> "%video.raw" :: opts
      | Some (`Internal None) -> "%video" :: opts
  in
  let opts =
    match m.audio_codec with
      | None -> opts
      | Some `Copy -> "%audio.copy" :: opts
      | Some (`Raw (Some c)) | Some (`Internal (Some c)) ->
          let audio_opts = Hashtbl.copy m.audio_opts in
          Hashtbl.add audio_opts "codec" (`String c);
          Hashtbl.add audio_opts "channels" (`Int m.channels);
          Hashtbl.add audio_opts "samplerate" (`Int (Lazy.force m.samplerate));
          let name =
            match m.audio_codec with
              | Some (`Raw _) -> "audio.raw"
              | _ -> "audio"
          in
          Printf.sprintf "%%%s(%s)" name (string_of_options audio_opts) :: opts
      | Some (`Raw None) -> "%audio.raw" :: opts
      | Some (`Internal None) -> "%audio" :: opts
  in
  let opts =
    match m.format with
      | Some f -> Printf.sprintf "format=%S" f :: opts
      | None -> opts
  in
  Printf.sprintf "%%ffmpeg(%s)" (String.concat "," opts)
