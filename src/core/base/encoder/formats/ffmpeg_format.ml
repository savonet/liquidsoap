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

type opt_val =
  [ `String of string | `Int of int | `Int64 of int64 | `Float of float ]

type copy_opt = [ `Wait_for_keyframe | `Ignore_keyframe ]
type output = [ `Stream | `Url of string ]
type opts = (string, opt_val) Hashtbl.t
type hwaccel = [ `None | `Auto | `Internal | `Device | `Frame ]

let string_of_hwaccel = function
  | `None -> "none"
  | `Auto -> "auto"
  | `Internal -> "internal"
  | `Device -> "device"
  | `Frame -> "frame"

type audio_options = {
  pcm_kind : Content.kind;
  channels : int;
  samplerate : int Lazy.t;
  sample_format : string option;
}

type video_options = {
  framerate : int Lazy.t;
  width : int Lazy.t;
  height : int Lazy.t;
  pixel_format : string option;
  hwaccel : hwaccel;
  hwaccel_device : string option;
  hwaccel_pixel_format : string option;
}

type subtitle_options = { text_to_ass : int -> string -> string }

type options =
  [ `Audio of audio_options
  | `Video of video_options
  | `Subtitle of subtitle_options ]

type encoded_stream = {
  mode : [ `Raw | `Internal ];
  codec : string option;
  options : options;
  opts : opts;
}

type stream = [ `Copy of copy_opt | `Encode of encoded_stream | `Drop ]

type t = {
  format : string option;
  output : output;
  streams : (Frame.field * stream) list;
  interleaved : [ `Default | `True | `False ];
  metadata : Frame.Metadata.t;
  opts : opts;
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

let string_of_copy_opt = function
  | `Wait_for_keyframe -> "wait_for_keyframe"
  | `Ignore_keyframe -> "ignore_keyframe"

let to_string m =
  let opts = [] in
  let opts =
    if Hashtbl.length m.opts > 0 then string_of_options m.opts :: opts else opts
  in
  let opts =
    List.fold_left
      (fun opts (field, stream) ->
        let name = Frame.Fields.string_of_field field in
        let name =
          match stream with
            | `Drop -> "%" ^ name ^ ".drop"
            | `Copy _ -> "%" ^ name ^ ".copy"
            | `Encode { mode = `Raw } -> "%" ^ name ^ ".raw"
            | _ -> "%" ^ name
        in
        match stream with
          | `Drop -> name :: opts
          | `Copy opt ->
              Printf.sprintf "%s(%s)" name (string_of_copy_opt opt) :: opts
          | `Encode { codec; options = `Video options; opts = stream_opts } ->
              let stream_opts =
                Hashtbl.fold
                  (fun lbl v h ->
                    Hashtbl.replace h lbl (v :> [ `Var of string | opt_val ]);
                    h)
                  stream_opts (Hashtbl.create 10)
              in
              ignore
                (Option.map
                   (fun codec ->
                     Hashtbl.replace stream_opts "codec" (`String codec))
                   codec);
              Hashtbl.replace stream_opts "framerate"
                (`Int (Lazy.force options.framerate));
              Hashtbl.replace stream_opts "width"
                (`Int (Lazy.force options.width));
              Hashtbl.replace stream_opts "height"
                (`Int (Lazy.force options.height));
              Hashtbl.replace stream_opts "hwaccel"
                (`Var (string_of_hwaccel options.hwaccel));
              Hashtbl.replace stream_opts "hwaccel_device"
                (match options.hwaccel_device with
                  | None -> `Var "none"
                  | Some d -> `String d);
              Printf.sprintf "%%%s(%s%s)" name
                (if Re.Pcre.pmatch ~rex:(Re.Pcre.regexp "video") name then ""
                 else "video_content,")
                (string_of_options stream_opts)
              :: opts
          | `Encode { codec; options = `Audio options; opts = stream_opts } ->
              let stream_opts = Hashtbl.copy stream_opts in
              ignore
                (Option.map
                   (fun codec ->
                     Hashtbl.replace stream_opts "codec" (`String codec))
                   codec);
              Hashtbl.replace stream_opts "channels" (`Int options.channels);
              Hashtbl.replace stream_opts "samplerate"
                (`Int (Lazy.force options.samplerate));
              Printf.sprintf "%s(%s%s)" name
                (if Re.Pcre.pmatch ~rex:(Re.Pcre.regexp "audio") name then ""
                 else "audio_content,")
                (string_of_options stream_opts)
              :: opts
          | `Encode { codec; options = `Subtitle _; opts = stream_opts } ->
              let stream_opts =
                Hashtbl.fold
                  (fun lbl v h ->
                    Hashtbl.replace h lbl (v :> [ `Var of string | opt_val ]);
                    h)
                  stream_opts (Hashtbl.create 10)
              in
              ignore
                (Option.map
                   (fun codec ->
                     Hashtbl.replace stream_opts "codec" (`String codec))
                   codec);
              Printf.sprintf "%s(%s%s)" name
                (if Re.Pcre.pmatch ~rex:(Re.Pcre.regexp "subtitle") name then ""
                 else "subtitle_content,")
                (string_of_options stream_opts)
              :: opts)
      opts m.streams
  in
  let opts =
    Printf.sprintf "interleaved=%s"
      (match m.interleaved with
        | `Default -> "\"default\""
        | `True -> "true"
        | `False -> "false")
    :: opts
  in
  let opts =
    match Metadata_base.to_list m.metadata with
      | [] -> opts
      | l ->
          Printf.sprintf "metadata=[%s]"
            (String.concat ","
               (List.map
                  (fun (k, v) ->
                    Printf.sprintf "%s=%s"
                      (Lang_string.quote_string k)
                      (Lang_string.quote_string v))
                  l))
          :: opts
  in
  let opts =
    match m.format with
      | Some f -> Printf.sprintf "format=%S" f :: opts
      | None -> opts
  in
  Printf.sprintf "%%ffmpeg(%s)" (String.concat "," opts)
