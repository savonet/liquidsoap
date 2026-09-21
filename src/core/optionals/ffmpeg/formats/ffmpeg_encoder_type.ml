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

open Liquidsoap_lang

(** What only libav can answer about a track. A process without it reads the
    conventions in the track's name and parameters, and leaves the rest unknown.
*)
type codecs = {
  media_type : string -> [ `Audio | `Video | `Subtitle ] option;
  channels_of_layout : string -> int option;
}

let codecs : codecs option ref = ref None
let implement c = codecs := Some c

(* The layouts a script is likely to name, for a process that cannot ask
   libav. *)
let named_layouts =
  [
    ("mono", 1);
    ("stereo", 2);
    ("2.1", 3);
    ("quad", 4);
    ("5.0", 5);
    ("5.1", 6);
    ("7.1", 8);
  ]

let channels_of_layout layout =
  match !codecs with
    | Some { channels_of_layout } -> channels_of_layout layout
    | None -> List.assoc_opt layout named_layouts

let contains needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec search i =
    i + n <= h && (String.sub haystack i n = needle || search (i + 1))
  in
  search 0

let term_string = function Term.{ term = `String s } -> Some s | _ -> None

(* A layout is named, or written as the number of channels it stands for:
   [channel_layout=5.1] is the same as [channel_layout="5.1"]. *)
let layout_name = function
  | Term.{ term = `String layout } -> Some layout
  | Term.{ term = `Float layout } -> Some (Printf.sprintf "%.1f" layout)
  | _ -> None

let has_content ~static_string name args =
  List.exists (fun (label, v) -> label = "" && static_string v = Some name) args

(* The conventions a track's media type can be read from: the name, a bare
   argument, or, where libav is linked, the codec itself. *)
let media_type ~static_string name args =
  let has_content = has_content ~static_string in
  if has_content "audio_content" args then Some `Audio
  else if has_content "video_content" args then Some `Video
  else if has_content "subtitle_content" args then Some `Subtitle
  else if contains "subtitle" name then Some `Subtitle
  else if contains "audio" name then Some `Audio
  else if contains "video" name then Some `Video
  else (
    match
      (!codecs, Option.bind (List.assoc_opt "codec" args) static_string)
    with
      | Some { media_type }, Some codec -> media_type codec
      | _ -> None)

(* A track is typed before anything is evaluated, so a channel count that is
   not written in the script has no value to read. *)
let static_channels name = function
  | Term.{ term = `Int n } -> n
  | Term.{ t = { Type.pos } } as tm ->
      Encoder_types.raise_error ~pos
        (Printf.sprintf
           "Invalid value %s for %s parameter. Only static numbers are allowed."
           (Term.to_string tm) name)

let channels args =
  match Option.bind (List.assoc_opt "channel_layout" args) layout_name with
    | Some layout when channels_of_layout layout <> None ->
        Option.get (channels_of_layout layout)
    | _ -> (
        match (List.assoc_opt "channels" args, List.assoc_opt "ac" args) with
          | Some tm, _ -> static_channels "channels" tm
          | _, Some tm -> static_channels "ac" tm
          | _ -> 2)

let pcm_kind ~static_string args =
  List.fold_left
    (fun kind -> function
      | "", v -> (
          match static_string v with
            | Some "pcm" -> Audio_format.kind
            | Some "pcm_s16" -> Pcm_format.S16.kind
            | Some "pcm_f32" -> Pcm_format.F32.kind
            | _ -> kind)
      | _ -> kind)
    Audio_format.kind args

let parse_encoder_name name =
  match String.split_on_char '.' name with
    | [field] -> (field, `Internal)
    | field :: "copy" :: _ -> (field, `Copy)
    | field :: "raw" :: _ -> (field, `Raw)
    | field :: "drop" :: _ -> (field, `Drop)
    | _ -> failwith "invalid content field"

let unknown_track () = Type.var ~constraints:[Format_type.track] ()
let kind name = Content_base.default_format (Content_base.kind_of_string name)

let format_of_track mode name args =
  match mode with
    | `Drop -> unknown_track ()
    | `Copy -> Type.make (Format_type.descr (`Format (kind "ffmpeg.copy")))
    | (`Raw | `Internal) as mode -> (
        match (media_type ~static_string:term_string name args, mode) with
          | None, _ -> unknown_track ()
          | Some `Audio, `Raw ->
              Type.make (Format_type.descr (`Format (kind "ffmpeg.audio.raw")))
          | Some `Video, `Raw ->
              Type.make (Format_type.descr (`Format (kind "ffmpeg.video.raw")))
          | Some `Audio, `Internal ->
              Type.make
                (Format_type.descr
                   (`Format
                      (Pcm_format.format_of_channels
                         ~pcm_kind:(pcm_kind ~static_string:term_string args)
                         (channels args))))
          | Some `Video, `Internal ->
              Type.make
                (Format_type.descr
                   (`Format (Content_base.default_format Video_format.kind)))
          | Some `Subtitle, _ ->
              Type.make (Format_type.descr (`Format Subtitle_format.format)))

let type_of_encoder params =
  List.fold_left
    (fun content_type param ->
      match param with
        | `Encoder (name, args) ->
            let args =
              List.filter_map
                (function
                  | `Anonymous s -> Some ("", Term.make (`String s))
                  | `Labelled (l, v) -> Some (l, v)
                  | `Encoder _ -> None)
                args
            in
            let field, mode = parse_encoder_name name in
            Fields.add (Fields.register field)
              (format_of_track mode name args)
              content_type
        | _ -> content_type)
    Fields.empty params

let () = Encoder_types.register "ffmpeg" type_of_encoder
