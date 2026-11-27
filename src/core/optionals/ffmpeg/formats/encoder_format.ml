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

type value =
  [ `String of string | `Int of int | `Int64 of Int64.t | `Float of float ]

type opts = (string * value) list
type media = [ `Audio | `Video | `Subtitle ]
type pcm_kind = [ `Pcm | `Pcm_s16 | `Pcm_f32 ]

type audio = {
  pcm_kind : pcm_kind;
  channels : int;
  channel_layout : string option;
  samplerate : Deferred.t;
  sample_format : string option;
  written : [ `Channels | `Samplerate ] list;
}

type hwaccel = [ `Auto | `None | `Internal | `Device | `Frame ]

type video = {
  framerate : (int * int) option;
  raw_key_frame_interval : float option;
  width : Deferred.t;
  height : Deferred.t;
  pixel_format : string option;
  alpha : bool;
  hwaccel : hwaccel;
  hwaccel_device : string option;
  hwaccel_pixel_format : string option;
}

type subtitle = { text_to_ass : Value.t option }
type options = [ `Audio of audio | `Video of video | `Subtitle of subtitle ]

type encoded = {
  mode : [ `Internal | `Raw ];
  codec : string option;
  options : options;
  opts : opts;
}

type keyframe = [ `Wait_for_keyframe | `Ignore_keyframe ]
type copy = { keyframe : keyframe; codec_tag : string option }
type track = [ `Drop | `Copy of copy | `Encode of encoded ]

let copy = { keyframe = `Wait_for_keyframe; codec_tag = None }

let fourcc tag =
  if String.length tag <> 4 then
    failwith (Printf.sprintf "codec_tag %S is not four characters" tag);
  String.fold_right (fun c tag -> (tag lsl 8) lor Char.code c) tag 0

type t = {
  format : string option;
  mime_type : string option;
  output : [ `Stream | `Url of string ];
  streams : (string * track) list;
  interleaved : [ `Default | `True | `False ];
  streamable : bool;  (** Readable from the middle, on the script's word. *)
  metadata : (string * string) list;
  opts : opts;
}

let set opts key v =
  if List.mem_assoc key opts then
    List.map (fun (k, v') -> if k = key then (k, v) else (k, v')) opts
  else opts @ [(key, v)]

type lookups = {
  authoritative : bool;
  channels_of_layout : string -> int option;
  pixel_format_has_alpha : string -> bool option;
  codec_media : string -> media option;
}

let layout_table =
  [
    ("mono", 1);
    ("stereo", 2);
    ("2.1", 3);
    ("quad", 4);
    ("5.0", 5);
    ("5.1", 6);
    ("7.1", 8);
  ]

let lookups =
  ref
    {
      authoritative = false;
      channels_of_layout = (fun name -> List.assoc_opt name layout_table);
      pixel_format_has_alpha = (fun _ -> Some false);
      codec_media = (fun _ -> None);
    }

let split_track_name name =
  match String.split_on_char '.' name with
    | [field] -> (field, `Internal)
    | [field; "raw"] -> (field, `Raw)
    | [field; "copy"] -> (field, `Copy)
    | [field; "drop"] -> (field, `Drop)
    | field :: rest ->
        failwith
          (Printf.sprintf "Invalid track mode suffix .%s for track %%%s"
             (String.concat "." rest) field)
    | [] -> assert false

let media_of_marker = function
  | "audio_content" -> Some `Audio
  | "video_content" -> Some `Video
  | "subtitle_content" -> Some `Subtitle
  | _ -> None

let contains ~sub s =
  let n = String.length sub and m = String.length s in
  let rec at i = i + n <= m && (String.sub s i n = sub || at (i + 1)) in
  at 0

let media_of_field field =
  List.find_opt
    (fun (sub, _) -> contains ~sub field)
    [("subtitle", `Subtitle); ("audio", `Audio); ("video", `Video)]
  |> Option.map snd

let media_type ~markers ~field ~codec =
  match
    List.find_opt (fun m -> List.mem m markers) [`Audio; `Video; `Subtitle]
  with
    | Some m -> Some m
    | None -> (
        match media_of_field field with
          | Some m -> Some m
          | None -> Option.bind codec !lookups.codec_media)

type shape =
  [ `Untyped
  | `Copy of Copy_changes.t
  | `Raw_audio
  | `Raw_video
  | `Audio of pcm_kind * int
  | `Video of bool
  | `Subtitle ]

let shape_of_track ~container : track -> shape = function
  | `Drop -> `Untyped
  | `Copy _ -> `Copy (Copy_changes.of_container container)
  | `Encode { mode = `Raw; options = `Audio _ } -> `Raw_audio
  | `Encode { mode = `Raw; options = `Video _ } -> `Raw_video
  | `Encode { options = `Subtitle _ } -> `Subtitle
  | `Encode { options = `Audio { pcm_kind; channels } } ->
      `Audio (pcm_kind, channels)
  | `Encode { options = `Video { alpha } } -> `Video alpha

let kind name =
  try Content_base.kind_of_string name
  with Content_base.Invalid ->
    failwith (Printf.sprintf "%s content is not available" name)

let type_of_shape : shape -> Type.t = function
  | `Untyped -> Type.var ~constraints:[Format_type.track] ()
  | `Copy containers ->
      let copy = kind Kinds.copy_name in
      Type.make
        (Format_type.descr
           (`Format
              (Content_base.parse_param copy "containers"
                 (Copy_changes.value containers))))
  | `Raw_audio ->
      Type.make (Format_type.descr (`Kind (kind Kinds.raw_audio_name)))
  | `Raw_video ->
      Type.make (Format_type.descr (`Kind (kind Kinds.raw_video_name)))
  | `Audio (pcm_kind, channels) ->
      let pcm_kind =
        match pcm_kind with
          | `Pcm -> Audio_format.kind
          | `Pcm_s16 -> Pcm_format.S16.kind
          | `Pcm_f32 -> Pcm_format.F32.kind
      in
      Type.make
        (Format_type.descr
           (`Format (Pcm_format.format_of_channels ~pcm_kind channels)))
  | `Video false -> Format_type.video ()
  | `Video true ->
      Type.make
        (Format_type.descr
           (`Format
              (Video_format.lift_params
                 {
                   Video_format.Specs.width = None;
                   height = None;
                   alpha = Unifier.make (Some true);
                 })))
  | `Subtitle -> Format_type.subtitle ()

let fields_type tracks =
  List.fold_left
    (fun fields (name, shape) ->
      let field = Fields.register name in
      if Fields.mem field fields then
        failwith
          (Printf.sprintf
             "Field %s is used by more than one track of this format" name);
      Fields.add field (type_of_shape shape) fields)
    Fields.empty tracks

let type_of_format f =
  fields_type
    (List.map
       (fun (field, track) -> (field, shape_of_track ~container:f.format track))
       f.streams)

let default_text_to_ass i text = Printf.sprintf "%d,0,Default,,0,0,0,,%s" i text
let string_literal = Lang_string.quote_string

let string_of_value = function
  | `String s -> string_literal s
  | `Int i -> string_of_int i
  | `Int64 i -> Int64.to_string i
  | `Float f -> Utils_base.string_of_float f

let string_of_opts opts =
  List.map (fun (k, v) -> Printf.sprintf "%s=%s" k (string_of_value v)) opts

let string_of_hwaccel = function
  | `Auto -> "auto"
  | `None -> "none"
  | `Internal -> "internal"
  | `Device -> "device"
  | `Frame -> "frame"

let string_of_track (field, track) =
  let marker media =
    match media_of_field field with
      | Some _ -> []
      | None -> (
          match media with
            | `Audio -> ["audio_content"]
            | `Video -> ["video_content"]
            | `Subtitle -> ["subtitle_content"])
  in
  let opt name = function
    | None -> []
    | Some v -> [Printf.sprintf "%s=%s" name (string_literal v)]
  in
  match track with
    | `Drop -> Printf.sprintf "%%%s.drop" field
    | `Copy { keyframe; codec_tag } ->
        let keyframe =
          match keyframe with
            | `Wait_for_keyframe -> "wait_for_keyframe"
            | `Ignore_keyframe -> "ignore_keyframe"
        in
        Printf.sprintf "%%%s.copy(%s)" field
          (String.concat ", " (keyframe :: opt "codec_tag" codec_tag))
    | `Encode { mode; codec; options; opts } ->
        let args =
          match options with
            | `Audio a ->
                marker `Audio
                @ (match a.pcm_kind with
                  | `Pcm -> []
                  | `Pcm_s16 -> ["pcm_s16"]
                  | `Pcm_f32 -> ["pcm_f32"])
                @ opt "codec" codec
                @ (match (a.channel_layout, mode) with
                  | Some l, _ -> opt "channel_layout" (Some l)
                  | None, `Raw when not (List.mem `Channels a.written) -> []
                  | None, _ -> [Printf.sprintf "channels=%d" a.channels])
                @ (match mode with
                  | `Raw when not (List.mem `Samplerate a.written) -> []
                  | _ ->
                      [
                        Printf.sprintf "samplerate=%d"
                          (Deferred.peek a.samplerate);
                      ])
                @ opt "sample_format" a.sample_format
            | `Video v ->
                marker `Video @ opt "codec" codec
                @ [
                    Printf.sprintf "width=%d" (Deferred.peek v.width);
                    Printf.sprintf "height=%d" (Deferred.peek v.height);
                  ]
                @ (match v.framerate with
                  | None -> []
                  | Some (rate, 1) -> [Printf.sprintf "framerate=%d" rate]
                  | Some (num, den) ->
                      [Printf.sprintf "framerate=\"%d/%d\"" num den])
                @ opt "pixel_format" v.pixel_format
                @ opt "hwaccel" (Some (string_of_hwaccel v.hwaccel))
                @ opt "hwaccel_device"
                    (Some (Option.value ~default:"none" v.hwaccel_device))
                @ opt "hwaccel_pixel_format" v.hwaccel_pixel_format
            | `Subtitle _ -> marker `Subtitle @ opt "codec" codec
        in
        Printf.sprintf "%%%s%s(%s)" field
          (match mode with `Internal -> "" | `Raw -> ".raw")
          (String.concat "," (args @ string_of_opts opts))

let to_string f =
  let items =
    (match f.format with
      | Some format -> [Printf.sprintf "format=%s" (string_literal format)]
      | None -> [])
    @ (match f.metadata with
      | [] -> []
      | m ->
          [
            Printf.sprintf "metadata=[%s]"
              (String.concat ","
                 (List.map
                    (fun (k, v) ->
                      Printf.sprintf "(%s,%s)" (string_literal k)
                        (string_literal v))
                    m));
          ])
    @ [
        (match f.interleaved with
          | `Default -> "interleaved=\"default\""
          | `True -> "interleaved=true"
          | `False -> "interleaved=false");
      ]
    @ (if f.streamable then ["streamable=true"] else [])
    @ List.map string_of_track f.streams
    @ string_of_opts
        (List.sort (fun (k, _) (k', _) -> String.compare k k') f.opts)
  in
  Printf.sprintf "%%ffmpeg(%s)" (String.concat "," items)

let adts_extension f =
  match List.filter (fun (_, t) -> t <> `Drop) f.streams with
    | [(_, `Encode { codec = Some "aac" })] -> ("aac", "audio/aac")
    | _ -> ("adts", "application/octet-stream")

let iso_family = ["mp4"; "mov"; "ipod"; "ismv"]

let extension_and_mime f =
  match f.format with
    | Some "ogg" -> (Some "ogg", "application/ogg")
    | Some "opus" -> (Some "opus", "application/ogg")
    | Some "mp3" -> (Some "mp3", "audio/mpeg")
    | Some "matroska" -> (Some "mkv", "video/x-matroska")
    | Some "mpegts" -> (Some "ts", "application/octet-stream")
    | Some "ac3" -> (Some "ac3", "audio/ac3")
    | Some "eac3" -> (Some "eac3", "audio/eac3")
    | Some "adts" ->
        let extension, mime = adts_extension f in
        (Some extension, mime)
    | Some "mp4" -> (Some "mp4", "video/mp4")
    | Some "webvtt" -> (Some "vtt", "text/vtt")
    | Some "webm" -> (Some "webm", "video/webm")
    | Some "wav" -> (Some "wav", "audio/wav")
    | _ -> (None, "application/octet-stream")

let extension f = fst (extension_and_mime f)
let mime f = snd (extension_and_mime f)

let video_size f =
  match
    List.filter_map
      (function
        | _, `Encode { options = `Video { width; height } } ->
            Some (Deferred.force width, Deferred.force height)
        | _ -> None)
      f.streams
  with
    | [size] -> Some size
    | _ -> None

(* A muxer whose stream a reader can start in the middle of, given its header:
   for the ISO family, a fragmented one. *)
let streamable f =
  let flag name =
    match List.assoc_opt "movflags" f.opts with
      | Some (`String flags) -> List.mem name (String.split_on_char '+' flags)
      | _ -> false
  in
  f.streamable
  ||
    match f.format with
    | Some
        ( "mp3" | "adts" | "ac3" | "eac3" | "mpegts" | "ogg" | "oga" | "ogv"
        | "opus" | "spx" | "matroska" | "webm" | "wav" | "flac" ) ->
        true
    | Some muxer when List.mem muxer iso_family -> (
        match List.assoc_opt "movflags" f.opts with
          | None -> true
          | Some _ ->
              flag "empty_moov"
              && (flag "frag_keyframe" || flag "frag_custom")
              && not (flag "delay_moov"))
    | _ -> false

(* An FFmpeg integer option: digits, then an SI suffix. *)
let int_option = function
  | `Int i -> Some i
  | `Int64 i -> Some (Int64.to_int i)
  | `Float f -> Some (int_of_float f)
  | `String s -> (
      let s = String.trim s in
      let length = String.length s in
      let multiplier, digits =
        match if length = 0 then ' ' else s.[length - 1] with
          | 'k' | 'K' -> (1_000, String.sub s 0 (length - 1))
          | 'M' -> (1_000_000, String.sub s 0 (length - 1))
          | _ -> (1, s)
      in
      match float_of_string_opt digits with
        | Some value -> Some (int_of_float (value *. float multiplier))
        | None -> None)
  | _ -> None

(** The bit rate the script set: the sum of the [b] of every encoded stream,
    when each has one and no stream is copied. *)
let explicit_bitrate f =
  let rates =
    List.filter_map
      (function
        | _, `Drop -> None
        | _, `Encode (encoded : encoded) ->
            Some (Option.bind (List.assoc_opt "b" encoded.opts) int_option)
        | _, `Copy _ -> Some None)
      f.streams
  in
  if rates <> [] && List.for_all Option.is_some rates then
    Some (List.fold_left (fun sum rate -> sum + Option.get rate) 0 rates)
  else None
