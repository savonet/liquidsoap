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

let log = Log.make ["decoder"; "ffmpeg"; "description"]

type media =
  [ `Audio of Avutil.audio Avcodec.params
  | `Video of Avutil.video Avcodec.params * Avutil.rational option
  | `Subtitle of Avutil.subtitle Avcodec.params
  | `Data of [ `Data ] Avcodec.params ]

type stream = {
  field : Frame.field;
  index : int;
  time_base : Avutil.rational;
  media : media;
}

type t = { format : string option; streams : stream list }

let mp4_brands =
  [
    ( "mp4",
      [
        "isom";
        "iso2";
        "iso3";
        "iso4";
        "iso5";
        "iso6";
        "mp41";
        "mp42";
        "avc1";
        "dash";
        "MSNV";
        "NDAS";
        "NDSC";
        "NDSH";
        "NDSM";
        "NDSP";
        "NDSS";
        "NDXC";
        "NDXH";
        "NDXM";
        "NDXP";
        "NDXS";
      ] );
    ("m4a", ["M4A "; "M4A"]);
    ("m4v", ["M4V "; "M4V"; "M4VP"]);
    ("mov", ["qt  "; "qt"]);
    ("3gp", ["3gp4"; "3gp5"; "3gp6"; "3gp7"; "3gp8"; "3gp9"]);
    ("3g2", ["3g2a"; "3g2b"; "3g2c"]);
    ("mj2", ["mj2s"; "mjp2"]);
    ("f4v", ["f4v "; "f4v"]);
  ]

let starts_with ~prefix s =
  String.length s >= String.length prefix
  && String.sub s 0 (String.length prefix) = prefix

let specific_format ~metadata name =
  if starts_with ~prefix:"mov" name then (
    match List.assoc_opt "major_brand" metadata with
      | None -> name
      | Some brand -> (
          match
            List.find_opt (fun (_, brands) -> List.mem brand brands) mp4_brands
          with
            | Some (format, _) -> format
            | None ->
                log#debug "Unknown major_brand: %S" brand;
                name))
  else name

let read_streams ~medium ~field_n streams params_of =
  List.filter_map Fun.id
    (List.mapi
       (fun rank (index, stream, params) ->
         match params_of stream params with
           | media ->
               Some
                 {
                   field = field_n rank;
                   index;
                   time_base = Av.get_time_base stream;
                   media;
                 }
           | exception exn ->
               log#important "Cannot read the parameters of %s stream %d: %s"
                 medium index (Printexc.to_string exn);
               None)
       streams)

let container ?format ~url input =
  let format =
    match format with
      | Some f -> Some (Av.Format.get_input_name f)
      | None -> Option.map Av.Format.get_input_name (Av.get_input_format input)
  in
  log#important "Analyzing container for format: %s, uri: %S"
    (match format with Some f -> Printf.sprintf "%S" f | None -> "unknown")
    url;
  let audio =
    read_streams ~medium:"audio" ~field_n:Frame.Fields.audio_n
      (Av.get_audio_streams input) (fun _ params -> `Audio params)
  in
  let video =
    read_streams ~medium:"video" ~field_n:Frame.Fields.video_n
      (Av.get_video_streams input) (fun stream params ->
        `Video (params, Av.get_avg_frame_rate stream))
  in
  let subtitles =
    read_streams ~medium:"subtitle" ~field_n:Frame.Fields.subtitles_n
      (Av.get_subtitle_streams input) (fun _ params -> `Subtitle params)
  in
  let data =
    read_streams ~medium:"data" ~field_n:Frame.Fields.data_n
      (Av.get_data_streams input) (fun _ params -> `Data params)
  in
  let streams = audio @ video @ subtitles @ data in
  if streams = [] then failwith "The container has no valid stream";
  {
    format =
      Option.map
        (specific_format ~metadata:(Av.get_input_metadata input))
        format;
    streams;
  }

let pixel_format_name params =
  match Avcodec.Video.get_pixel_format params with
    | None -> "unknown"
    | Some f -> Option.value ~default:"none" (Avutil.Pixel_format.to_string f)

let describe { streams } =
  String.concat ", "
    (List.map
       (fun { field; media } ->
         let field = Frame.Fields.string_of_field field in
         match media with
           | `Audio p ->
               Printf.sprintf "%s: {codec: %s, %dHz, %d channel(s)}" field
                 (Avcodec.Audio.string_of_id (Avcodec.Audio.get_params_id p))
                 (Avcodec.Audio.get_sample_rate p)
                 (Avcodec.Audio.get_nb_channels p)
           | `Video (p, _) ->
               Printf.sprintf "%s: {codec: %s, %dx%d, %s}" field
                 (Avcodec.Video.string_of_id (Avcodec.Video.get_params_id p))
                 (Avcodec.Video.get_width p)
                 (Avcodec.Video.get_height p)
                 (pixel_format_name p)
           | `Subtitle p ->
               Printf.sprintf "%s: {codec: %s, subtitle}" field
                 (Avcodec.Subtitle.string_of_id
                    (Avcodec.Subtitle.get_params_id p))
           | `Data p ->
               Printf.sprintf "%s: {codec: %s}" field
                 (Avcodec.Unknown.string_of_id
                    (Avcodec.Unknown.get_params_id p)))
       streams)

let copy_params { media; time_base } =
  match media with
    | `Audio p -> Some (Copy_params.make (`Audio p))
    | `Video (p, frame_rate) -> Some (Copy_params.make ?frame_rate (`Video p))
    | `Subtitle p -> Some (Copy_params.make ~time_base (`Subtitle p))
    | `Data _ -> None

let merge_copy format stream =
  match copy_params stream with
    | None -> ()
    | Some params ->
        (match Ffmpeg_content.Copy.get_params format with
          | Some current -> (
              match Copy_params.incompatibility current params with
                | Some reason ->
                    failwith
                      (Printf.sprintf
                         "Incompatible copy parameters for field %s: %s. \
                          Current: %s, new: %s"
                         (Frame.Fields.string_of_field stream.field)
                         reason
                         (Copy_params.to_string current)
                         (Copy_params.to_string params))
                | None -> ())
          | None -> ());
        Content.merge format (Ffmpeg_content.Copy.lift_params (Some params))

let has_alpha params =
  match Avcodec.Video.get_pixel_format params with
    | None -> false
    | Some f -> (
        try List.mem `Alpha (Avutil.Pixel_format.descriptor f).flags
        with _ -> false)

let is_text_subtitle params =
  match Avcodec.descriptor params with
    | Some { Avcodec.properties } -> List.mem `Text_sub properties
    | None -> false

let field_type format stream =
  let is kind = Content_base.kind format = kind in
  match stream.media with
    | `Data _ -> None
    | _ when is Ffmpeg_content.Copy.kind ->
        merge_copy format stream;
        Some format
    | `Audio _ when is Ffmpeg_content.Audio.kind -> Some format
    | `Video _ when is Ffmpeg_content.Video.kind -> Some format
    | `Audio p -> (
        let channels = Avcodec.Audio.get_nb_channels p in
        match
          Pcm_format.audio_format ~pcm_kind:(Content_base.kind format)
            (Audio_format.Specs.param_of_channels channels)
        with
          | f -> Some f
          | exception _ -> Some format)
    | `Video (p, _) ->
        if Content.Video.is_format format then (
          let alpha = (Content.Video.get_params format).alpha in
          if Unifier.deref alpha = None then
            Unifier.set alpha (Some (has_alpha p)));
        Some format
    | `Subtitle p ->
        if Subtitle_format.is_format format && is_text_subtitle p then
          Some format
        else Some (Content_base.default_format Video_format.kind)

let get_type ~ctype container =
  log#important "Requested content-type: %s"
    (Frame.string_of_content_type ctype);
  let decoded =
    List.fold_left
      (fun decoded stream ->
        match Frame.Fields.find_opt stream.field ctype with
          | None -> decoded
          | Some format -> (
              match field_type format stream with
                | Some f -> Frame.Fields.add stream.field f decoded
                | None -> decoded))
      Frame.Fields.empty container.streams
  in
  log#important "Decoded content-type: %s"
    (Frame.string_of_content_type decoded);
  decoded
