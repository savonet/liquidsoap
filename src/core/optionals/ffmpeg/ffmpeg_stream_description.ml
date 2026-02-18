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

(* Map ISOBMFF major_brand to specific format name *)
let format_of_major_brand = function
  | "isom" | "iso2" | "iso3" | "iso4" | "iso5" | "iso6" | "mp41" | "mp42"
  | "avc1" | "dash" ->
      Some "mp4"
  | "M4A " | "M4A" -> Some "m4a"
  | "M4V " | "M4V" | "M4VP" -> Some "m4v"
  | "qt  " | "qt" -> Some "mov"
  | "3gp4" | "3gp5" | "3gp6" | "3gp7" | "3gp8" | "3gp9" -> Some "3gp"
  | "3g2a" | "3g2b" | "3g2c" -> Some "3g2"
  | "mj2s" | "mjp2" -> Some "mj2"
  | "f4v " | "f4v" -> Some "f4v"
  | "MSNV" -> Some "mp4" (* Sony PSP *)
  | "NDAS" | "NDSC" | "NDSH" | "NDSM" | "NDSP" | "NDSS" | "NDXC" | "NDXH"
  | "NDXM" | "NDXP" | "NDXS" ->
      Some "mp4" (* Nero Digital *)
  | _ -> None

let get_specific_format container format_name =
  if String.contains format_name ',' then (
    let metadata = Av.get_input_metadata container in
    match List.assoc_opt "major_brand" metadata with
      | Some brand -> (
          match format_of_major_brand brand with
            | Some specific -> specific
            | None ->
                log#debug "Unknown major_brand %S, keeping full format list"
                  brand;
                format_name)
      | None -> format_name)
  else format_name

type audio_params = {
  codec_name : string;
  codec_params : Avutil.audio Avcodec.params;
  samplerate : int;
  channels : int;
  channel_layout : string;
}

type video_params = {
  codec_name : string;
  codec_params : Avutil.video Avcodec.params;
  width : int;
  height : int;
  pixel_format : string;
  frame_rate : Avutil.rational option;
}

type subtitle_params = {
  codec_name : string;
  codec_params : Avutil.subtitle Avcodec.params;
}

type data_params = {
  codec_name : string;
  codec_params : [ `Data ] Avcodec.params;
}

type stream_params =
  [ `Audio of audio_params
  | `Video of video_params
  | `Subtitle of subtitle_params
  | `Data of data_params ]

type stream = {
  field : Frame.Fields.field;
  params : stream_params;
  copy : bool;
}

type container = { format : string option; streams : stream list }

type result = {
  content_type : Frame.content_type;
  container : container;
  description : string;
}

let audio_params_of_codec_params codec_params =
  let codec_name =
    Avcodec.Audio.string_of_id (Avcodec.Audio.get_params_id codec_params)
  in
  let samplerate = Avcodec.Audio.get_sample_rate codec_params in
  let channels = Avcodec.Audio.get_nb_channels codec_params in
  let channel_layout =
    Avutil.Channel_layout.get_description
      (Avcodec.Audio.get_channel_layout codec_params)
  in
  { codec_name; codec_params; samplerate; channels; channel_layout }

let video_params_of_codec_params ~frame_rate codec_params =
  let codec_name =
    Avcodec.Video.string_of_id (Avcodec.Video.get_params_id codec_params)
  in
  let width = Avcodec.Video.get_width codec_params in
  let height = Avcodec.Video.get_height codec_params in
  let pixel_format =
    match Avcodec.Video.get_pixel_format codec_params with
      | None -> "unknown"
      | Some f -> Option.value ~default:"none" (Avutil.Pixel_format.to_string f)
  in
  { codec_name; codec_params; width; height; pixel_format; frame_rate }

let subtitle_params_of_codec_params codec_params : subtitle_params =
  let codec_name =
    Avcodec.Subtitle.string_of_id (Avcodec.Subtitle.get_params_id codec_params)
  in
  { codec_name; codec_params }

let data_params_of_codec_params codec_params : data_params =
  let codec_name =
    Avcodec.Unknown.string_of_id (Avcodec.Unknown.get_params_id codec_params)
  in
  { codec_name; codec_params }

let audio_description ~field (params : audio_params) =
  Printf.sprintf "%s: {codec: %s, %dHz, %d channel(s)}"
    (Frame.Fields.string_of_field field)
    params.codec_name params.samplerate params.channels

let video_description ~field (params : video_params) =
  Printf.sprintf "%s: {codec: %s, %dx%d, %s}"
    (Frame.Fields.string_of_field field)
    params.codec_name params.width params.height params.pixel_format

let subtitle_description ~field (params : subtitle_params) =
  Printf.sprintf "%s: {codec: %s, subtitle}"
    (Frame.Fields.string_of_field field)
    params.codec_name

let data_description ~field (params : data_params) =
  Printf.sprintf "%s: {codec: %s}"
    (Frame.Fields.string_of_field field)
    params.codec_name

let subtitle_codec_is_text params =
  match Avcodec.descriptor params with
    | Some { Avcodec.properties; _ } -> List.mem `Text_sub properties
    | None -> false

let collect_audio_streams container =
  List.fold_left
    (fun (streams, descriptions) (_, _, codec_params) ->
      try
        let field = Frame.Fields.audio_n (List.length streams) in
        let params = audio_params_of_codec_params codec_params in
        let description = audio_description ~field params in
        ((field, codec_params, params) :: streams, description :: descriptions)
      with Avutil.Error _ as exn ->
        let bt = Printexc.get_raw_backtrace () in
        Utils.log_exception ~log
          ~bt:(Printexc.raw_backtrace_to_string bt)
          (Printf.sprintf "Failed to get audio stream info: %s"
             (Printexc.to_string exn));
        (streams, descriptions))
    ([], [])
    (Av.get_audio_streams container)

let collect_video_streams container descriptions =
  List.fold_left
    (fun (streams, descriptions) (_, stream, codec_params) ->
      try
        let field = Frame.Fields.video_n (List.length streams) in
        let frame_rate = Av.get_avg_frame_rate stream in
        let params = video_params_of_codec_params ~frame_rate codec_params in
        let description = video_description ~field params in
        ( streams @ [(field, codec_params, frame_rate, params)],
          descriptions @ [description] )
      with Avutil.Error _ as exn ->
        let bt = Printexc.get_raw_backtrace () in
        Utils.log_exception ~log
          ~bt:(Printexc.raw_backtrace_to_string bt)
          (Printf.sprintf "Failed to get video stream info: %s"
             (Printexc.to_string exn));
        (streams, descriptions))
    ([], descriptions)
    (Av.get_video_streams container)

let collect_subtitle_streams container descriptions =
  List.fold_left
    (fun (streams, descriptions) (_, stream, codec_params) ->
      try
        let field = Frame.Fields.subtitles_n (List.length streams) in
        let params = subtitle_params_of_codec_params codec_params in
        let description = subtitle_description ~field params in
        let time_base = Av.get_time_base stream in
        ( streams @ [(field, codec_params, time_base, params)],
          descriptions @ [description] )
      with Avutil.Error _ as exn ->
        let bt = Printexc.get_raw_backtrace () in
        Utils.log_exception ~log
          ~bt:(Printexc.raw_backtrace_to_string bt)
          (Printf.sprintf "Failed to get subtitle stream info: %s"
             (Printexc.to_string exn));
        (streams, descriptions))
    ([], descriptions)
    (Av.get_subtitle_streams container)

let collect_data_streams container subtitle_count descriptions =
  List.fold_left
    (fun (n, descriptions) (_, _, codec_params) ->
      try
        let field = Frame.Fields.data_n (n + subtitle_count) in
        let params = data_params_of_codec_params codec_params in
        let description = data_description ~field params in
        (n + 1, descriptions @ [description])
      with Avutil.Error _ as exn ->
        let bt = Printexc.get_raw_backtrace () in
        Utils.log_exception ~log
          ~bt:(Printexc.raw_backtrace_to_string bt)
          (Printf.sprintf "Failed to get data stream info: %s"
             (Printexc.to_string exn));
        (n, descriptions))
    (0, descriptions)
    (Av.get_data_streams container)

let build_audio_content_type ~ctype audio_streams =
  List.fold_left
    (fun (content_type, stream_infos) (field, codec_params, params) ->
      match (codec_params, Frame.Fields.find_opt field ctype) with
        | p, Some format when Ffmpeg_copy_content.is_format format ->
            ignore
              (Content.merge format
                 (Ffmpeg_copy_content.lift_params (Some (`Audio p))));
            let stream_info = { field; params = `Audio params; copy = true } in
            ( Frame.Fields.add field format content_type,
              stream_info :: stream_infos )
        | p, Some format when Ffmpeg_raw_content.Audio.is_format format ->
            let dst_format =
              Ffmpeg_raw_content.(Audio.lift_params (AudioSpecs.mk_params p))
            in
            (try ignore (Content.merge format dst_format)
             with _ when Content.compatible format dst_format -> ());
            let stream_info = { field; params = `Audio params; copy = false } in
            ( Frame.Fields.add field dst_format content_type,
              stream_info :: stream_infos )
        | p, Some format ->
            let stream_info = { field; params = `Audio params; copy = false } in
            ( Frame.Fields.add field
                (Frame_base.format_of_channels ~pcm_kind:(Content.kind format)
                   (Avcodec.Audio.get_nb_channels p))
                content_type,
              stream_info :: stream_infos )
        | _ -> (content_type, stream_infos))
    (Frame.Fields.empty, []) audio_streams

let build_video_content_type ~ctype content_type video_streams =
  List.fold_left
    (fun (content_type, stream_infos)
         (field, codec_params, avg_frame_rate, params) ->
      match (codec_params, Frame.Fields.find_opt field ctype) with
        | codec_params, Some format when Ffmpeg_copy_content.is_format format ->
            ignore
              (Content.merge format
                 (Ffmpeg_copy_content.lift_params
                    (Some
                       (`Video
                          { Ffmpeg_copy_content.avg_frame_rate; codec_params }))));
            let stream_info = { field; params = `Video params; copy = true } in
            ( Frame.Fields.add field format content_type,
              stream_info :: stream_infos )
        | p, Some format when Ffmpeg_raw_content.Video.is_format format ->
            ignore
              (Content.merge format
                 Ffmpeg_raw_content.(Video.lift_params (VideoSpecs.mk_params p)));
            let stream_info = { field; params = `Video params; copy = false } in
            ( Frame.Fields.add field format content_type,
              stream_info :: stream_infos )
        | _, Some _ ->
            let stream_info = { field; params = `Video params; copy = false } in
            ( Frame.Fields.add field
                Content.(default_format Video.kind)
                content_type,
              stream_info :: stream_infos )
        | _ -> (content_type, stream_infos))
    (content_type, []) video_streams

let build_subtitle_content_type ~ctype content_type subtitle_streams =
  List.fold_left
    (fun (content_type, stream_infos) (field, codec_params, time_base, params)
       ->
      match Frame.Fields.find_opt field ctype with
        | Some format when Ffmpeg_copy_content.is_format format ->
            ignore
              (Content.merge format
                 (Ffmpeg_copy_content.lift_params
                    (Some
                       (`Subtitle
                          { Ffmpeg_copy_content.time_base; codec_params }))));
            let stream_info =
              { field; params = `Subtitle params; copy = true }
            in
            ( Frame.Fields.add field format content_type,
              stream_info :: stream_infos )
        | Some format
          when Subtitle_content.is_format format
               && subtitle_codec_is_text codec_params ->
            let stream_info =
              { field; params = `Subtitle params; copy = false }
            in
            ( Frame.Fields.add field Subtitle_content.format content_type,
              stream_info :: stream_infos )
        | Some _ ->
            let stream_info =
              { field; params = `Subtitle params; copy = false }
            in
            ( Frame.Fields.add field
                Content.(default_format Video.kind)
                content_type,
              stream_info :: stream_infos )
        | _ -> (content_type, stream_infos))
    (content_type, []) subtitle_streams

let get_type ?format ~ctype ~url container =
  let format =
    match format with None -> Av.get_input_format container | Some f -> Some f
  in
  let uri = Lang_string.quote_string url in
  log#important "Requested content-type for %s%s: %s"
    (match format with
      | Some f ->
          Printf.sprintf "format: %s, uri: "
            (Lang_string.quote_string (Av.Format.get_input_name f))
      | None -> "")
    uri
    (Frame.string_of_content_type ctype);

  let audio_streams, descriptions = collect_audio_streams container in
  let video_streams, descriptions =
    collect_video_streams container descriptions
  in
  let subtitle_streams, descriptions =
    collect_subtitle_streams container descriptions
  in
  let _, descriptions =
    collect_data_streams container (List.length subtitle_streams) descriptions
  in

  if audio_streams = [] && video_streams = [] && subtitle_streams = [] then
    failwith "No valid stream found in container.";

  let content_type, audio_stream_infos =
    build_audio_content_type ~ctype audio_streams
  in
  let content_type, video_stream_infos =
    build_video_content_type ~ctype content_type video_streams
  in
  let content_type, subtitle_stream_infos =
    build_subtitle_content_type ~ctype content_type subtitle_streams
  in

  let streams =
    List.rev audio_stream_infos
    @ List.rev video_stream_infos
    @ List.rev subtitle_stream_infos
  in

  let description = String.concat ", " descriptions in
  log#important "FFmpeg recognizes %s as %s" uri description;
  log#important "Decoded content-type for %s: %s" uri
    (Frame.string_of_content_type content_type);

  let format_name =
    Option.map
      (fun f -> get_specific_format container (Av.Format.get_input_name f))
      format
  in
  { content_type; container = { format = format_name; streams }; description }

let audio_params_to_json (p : audio_params) : Json.t =
  `Assoc
    [
      ("type", `String "audio");
      ("codec", `String p.codec_name);
      ("samplerate", `Int p.samplerate);
      ("channels", `Int p.channels);
      ("channel_layout", `String p.channel_layout);
    ]

let video_params_to_json (p : video_params) : Json.t =
  let frame_rate =
    match p.frame_rate with
      | None -> `Null
      | Some { Avutil.num; den } ->
          `Assoc [("num", `Int num); ("den", `Int den)]
  in
  `Assoc
    [
      ("type", `String "video");
      ("codec", `String p.codec_name);
      ("width", `Int p.width);
      ("height", `Int p.height);
      ("pixel_format", `String p.pixel_format);
      ("frame_rate", frame_rate);
    ]

let subtitle_params_to_json (p : subtitle_params) : Json.t =
  `Assoc [("type", `String "subtitle"); ("codec", `String p.codec_name)]

let data_params_to_json (p : data_params) : Json.t =
  `Assoc [("type", `String "data"); ("codec", `String p.codec_name)]

let stream_params_to_json = function
  | `Audio p -> audio_params_to_json p
  | `Video p -> video_params_to_json p
  | `Subtitle p -> subtitle_params_to_json p
  | `Data p -> data_params_to_json p

let stream_to_json (s : stream) : Json.t =
  `Assoc
    [
      ("field", `String (Frame.Fields.string_of_field s.field));
      ("copy", `Bool s.copy);
      ("params", stream_params_to_json s.params);
    ]

let json_of_container (c : container) : Json.t =
  let format = match c.format with None -> `Null | Some f -> `String f in
  `Assoc
    [
      ("format", format); ("streams", `Tuple (List.map stream_to_json c.streams));
    ]
