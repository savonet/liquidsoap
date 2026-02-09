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

(** Decode and read metadata using ffmpeg. *)

exception End_of_file
exception No_stream
exception Invalid_file

let log = Log.make ["decoder"; "ffmpeg"]

(* Workaround for https://trac.ffmpeg.org/ticket/9540. Should be fixed with
   the next FFMpeg release. *)
let parse_timed_id3 content =
  if String.length content < 3 then failwith "Invalid content";
  if String.sub content 0 3 = "ID3" then
    Metadata.Reader.with_string Metadata.ID3.parse content
  else (
    try
      let metadata = Printf.sprintf "ID3\003\000%s" content in
      Metadata.Reader.with_string Metadata.ID3.parse metadata
    with _ ->
      let metadata = Printf.sprintf "ID3\004\000%s" content in
      Metadata.Reader.with_string Metadata.ID3.parse metadata)

module Streams = Map.Make (struct
  type t = int

  let compare = Stdlib.compare
end)

type 'a stream = {
  index : int;
  time_base : Avutil.rational;
  sparse : [ `False | `True of buffer:Decoder.buffer -> int -> unit ];
  mutable decoder : 'a;
  mutable seen : bool;
  (* All positions are in main ticks *)
  mutable first_position : int option;
  mutable pts : int option;
  mutable position : int option;
}

let mk_stream ~index ~sparse ~time_base decoder =
  {
    index;
    time_base;
    sparse;
    seen = false;
    decoder;
    first_position = None;
    pts = None;
    position = None;
  }

let add_stream (type a b c) ~sparse idx (av_stream : (a, b, c) Av.stream)
    decoder streams =
  let time_base = Av.get_time_base av_stream in
  Streams.add idx (mk_stream ~index:idx ~sparse ~time_base decoder) streams

(** Configuration keys for ffmpeg. *)
let mime_types =
  Dtools.Conf.list
    ~p:(Decoder.conf_mime_types#plug "ffmpeg")
    "Mime-types used for decoding with ffmpeg"
    ~d:
      [
        "application/f4v";
        "application/ffmpeg";
        "application/mp4";
        "application/mxf";
        "application/octet-stream";
        "application/octet-stream";
        "application/ogg";
        "application/vnd.pg.format";
        "application/vnd.rn-realmedia";
        "application/vnd.smaf";
        "application/x-mpegURL";
        "application/x-ogg";
        "application/x-pgs";
        "application/x-shockwave-flash";
        "application/x-subrip";
        "application/xml";
        "audio/G722";
        "audio/MP4A-LATM";
        "audio/MPA";
        "audio/aac";
        "audio/aacp";
        "audio/aiff";
        "audio/amr";
        "audio/basic";
        "audio/bit";
        "audio/flac";
        "audio/g723";
        "audio/iLBC";
        "audio/mp4";
        "audio/mpeg";
        "audio/ogg";
        "audio/vnd.wave";
        "audio/wav";
        "audio/wave";
        "audio/webm";
        "audio/x-ac3";
        "audio/x-adpcm";
        "audio/x-caf";
        "audio/x-dca";
        "audio/x-eac3";
        "audio/x-flac";
        "audio/x-gsm";
        "audio/x-hx-aac-adts";
        "audio/x-ogg";
        "audio/x-oma";
        "audio/x-tta";
        "audio/x-voc";
        "audio/x-wav";
        "audio/x-wavpack";
        "multipart/x-mixed-replace;boundary=ffserver";
        "text/vtt";
        "text/x-ass";
        "text/x-jacosub";
        "text/x-microdvd";
        "video/3gpp";
        "video/3gpp2";
        "video/MP2T";
        "video/mp2t";
        "video/mp4";
        "video/mpeg";
        "video/ogg";
        "video/webm";
        "video/x-flv";
        "video/x-h261";
        "video/x-h263";
        "video/x-m4v";
        "video/x-matroska";
        "video/x-mjpeg";
        "video/x-ms-asf";
        "video/x-msvideo";
        "video/x-nut";
      ]

let image_mime_types =
  Dtools.Conf.list ~p:(mime_types#plug "images")
    "Mime-types used for decoding images with ffmpeg"
    ~d:
      [
        "image/gif";
        "image/jpeg";
        "image/png";
        "image/vnd.microsoft.icon";
        "image/webp";
      ]

let file_extensions =
  Dtools.Conf.list
    ~p:(Decoder.conf_file_extensions#plug "ffmpeg")
    "File extensions used for decoding media files (except images) with ffmpeg"
    ~d:
      [
        "264";
        "265";
        "302";
        "3g2";
        "3gp";
        "669";
        "722";
        "A64";
        "a64";
        "aa";
        "aa3";
        "aac";
        "aax";
        "ac3";
        "acm";
        "adf";
        "adp";
        "ads";
        "adts";
        "adx";
        "aea";
        "afc";
        "aif";
        "aifc";
        "aiff";
        "aix";
        "amf";
        "amr";
        "ams";
        "amv";
        "ape";
        "apl";
        "apm";
        "apng";
        "aptx";
        "aptxhd";
        "aqt";
        "asf";
        "ass";
        "ast";
        "au";
        "aud";
        "avi";
        "avr";
        "avs";
        "avs2";
        "bcstm";
        "bfstm";
        "binka";
        "bit";
        "bmv";
        "brstm";
        "c2";
        "calf";
        "cavs";
        "cdata";
        "cdg";
        "cdxl";
        "cgi";
        "chk";
        "cif";
        "cpk";
        "cvg";
        "dat";
        "daud";
        "dav";
        "dbm";
        "dif";
        "digi";
        "dmf";
        "dnxhd";
        "dnxhr";
        "drc";
        "dsm";
        "dss";
        "dtk";
        "dtm";
        "dts";
        "dtshd";
        "dv";
        "dvd";
        "eac3";
        "f4v";
        "fap";
        "far";
        "ffmeta";
        "fits";
        "flac";
        "flm";
        "flv";
        "fsb";
        "fwse";
        "g722";
        "g723_1";
        "g729";
        "gdm";
        "genh";
        "gif";
        "gsm";
        "gxf";
        "h261";
        "h263";
        "h264";
        "h265";
        "hca";
        "hevc";
        "ice";
        "ico";
        "idf";
        "idx";
        "ifv";
        "imf";
        "imx";
        "ipu";
        "ircam";
        "ism";
        "isma";
        "ismv";
        "it";
        "ivf";
        "ivr";
        "j2b";
        "jss";
        "kux";
        "latm";
        "lbc";
        "loas";
        "lrc";
        "lvf";
        "m15";
        "m1v";
        "m2a";
        "m2t";
        "m2ts";
        "m2v";
        "m3u8";
        "m4a";
        "m4b";
        "m4v";
        "mac";
        "mca";
        "mcc";
        "mdl";
        "med";
        "mj2";
        "mjpeg";
        "mjpg";
        "mk3d";
        "mka";
        "mks";
        "mkv";
        "mlp";
        "mmcmp";
        "mmf";
        "mms";
        "mo3";
        "mod";
        "mods";
        "moflex";
        "mov";
        "mp2";
        "mp3";
        "mp4";
        "mpa";
        "mpc";
        "mpd";
        "mpeg";
        "mpg";
        "mpl2";
        "mptm";
        "msbc";
        "msf";
        "mt2";
        "mtaf";
        "mtm";
        "mts";
        "musx";
        "mvi";
        "mxf";
        "mxg";
        "nist";
        "nsp";
        "nst";
        "nut";
        "obu";
        "oga";
        "ogg";
        "ogv";
        "okt";
        "oma";
        "omg";
        "opus";
        "paf";
        "pcm";
        "pjs";
        "plm";
        "psm";
        "psp";
        "pt36";
        "ptm";
        "pvf";
        "qcif";
        "ra";
        "rco";
        "rcv";
        "rgb";
        "rm";
        "roq";
        "rsd";
        "rso";
        "rt";
        "s3m";
        "sami";
        "sbc";
        "sbg";
        "scc";
        "sdr2";
        "sds";
        "sdx";
        "ser";
        "sf";
        "sfx";
        "sfx2";
        "sga";
        "shn";
        "sln";
        "smi";
        "son";
        "sox";
        "spdif";
        "sph";
        "spx";
        "srt";
        "ss2";
        "ssa";
        "st26";
        "stk";
        "stl";
        "stm";
        "stp";
        "str";
        "sub";
        "sup";
        "svag";
        "svs";
        "swf";
        "tak";
        "tco";
        "thd";
        "ts";
        "tta";
        "ttml";
        "tun";
        "txt";
        "ty";
        "ty+";
        "ult";
        "umx";
        "v";
        "v210";
        "vag";
        "vb";
        "vc1";
        "vc2";
        "viv";
        "vob";
        "voc";
        "vpk";
        "vqe";
        "vqf";
        "vql";
        "vtt";
        "w64";
        "wav";
        "webm";
        "wma";
        "wmv";
        "wow";
        "wsd";
        "wtv";
        "wv";
        "xl";
        "xm";
        "xml";
        "xmv";
        "xpk";
        "xvag";
        "y4m";
        "yop";
        "yuv";
      ]

let image_file_extensions =
  Dtools.Conf.list
    ~p:(file_extensions#plug "images")
    "File extensions used for decoding images with ffmpeg"
    ~d:
      [
        "bmp";
        "cri";
        "dds";
        "dng";
        "dpx";
        "exr";
        "im1";
        "im24";
        "im32";
        "im8";
        "j2c";
        "j2k";
        "jls";
        "jp2";
        "jpc";
        "jpeg";
        "jpg";
        "jps";
        "ljpg";
        "mng";
        "mpg1-img";
        "mpg2-img";
        "mpg4-img";
        "mpo";
        "pam";
        "pbm";
        "pcd";
        "pct";
        "pcx";
        "pfm";
        "pgm";
        "pgmyuv";
        "pic";
        "pict";
        "pix";
        "png";
        "pnm";
        "pns";
        "ppm";
        "ptx";
        "ras";
        "raw";
        "rs";
        "sgi";
        "sun";
        "sunras";
        "svg";
        "svgz";
        "tga";
        "tif";
        "tiff";
        "webp";
        "xbm";
        "xface";
        "xpm";
        "xwd";
        "y";
        "yuv10";
      ]

let priority =
  Dtools.Conf.int
    ~p:(Decoder.conf_priorities#plug "ffmpeg")
    "Priority for the ffmpeg decoder" ~d:10

let parse_encoder_params =
  let processor =
    MenhirLib.Convert.Simplified.traditional2revised
      Liquidsoap_lang.Parser.plain_encoder_params
  in
  fun s ->
    let lexbuf = Sedlexing.Utf8.from_string ("(" ^ s ^ ")") in
    let tokenizer = Liquidsoap_lang.Preprocessor.mk_tokenizer lexbuf in
    Liquidsoap_lang.Term_reducer.to_encoder_params
      ~throw:(fun ~bt exn -> Printexc.raise_with_backtrace exn bt)
      (processor tokenizer)

let parse_input_args args =
  try
    let args = parse_encoder_params args in
    List.fold_left
      (fun (args, format) -> function
        | `Labelled ("f", Term.{ term = `Var format; _ })
        | `Labelled ("format", Term.{ term = `Var format; _ }) ->
            (args, Av.Format.find_input_format format)
        | `Labelled (k, Term.{ term = `Var v; _ }) ->
            ((k, `String v) :: args, format)
        | `Labelled (k, Term.{ term = `String s; _ }) ->
            ((k, `String s) :: args, format)
        | `Labelled (k, Term.{ term = `Int i; _ }) ->
            ((k, `Int i) :: args, format)
        | `Labelled (k, Term.{ term = `Float f; _ }) ->
            ((k, `Float f) :: args, format)
        | _ -> assert false)
      ([], None) args
  with _ ->
    Runtime_error.raise ~pos:[] ~message:"Invalid mime type arguments!"
      "ffmpeg_decoder"

let parse_file_decoder_args metadata =
  match Frame.Metadata.find_opt "ffmpeg_options" metadata with
    | Some args -> parse_input_args args
    | None -> ([], None)

let dresolver ~metadata file =
  let args, format = parse_file_decoder_args metadata in
  let opts = Hashtbl.create 10 in
  List.iter (fun (k, v) -> Hashtbl.replace opts k v) args;
  let container = Av.open_input ?format ~opts file in
  Fun.protect
    ~finally:(fun () -> Av.close container)
    (fun () ->
      let duration = Av.get_input_duration container ~format:`Millisecond in
      Option.map (fun d -> Int64.to_float d /. 1000.) duration)

let () =
  Plug.register Request.dresolvers "ffmpeg" ~doc:""
    {
      dpriority = (fun () -> priority#get);
      file_extensions = (fun () -> file_extensions#get);
      dresolver =
        (fun ~metadata fname ->
          match dresolver ~metadata fname with
            | None -> raise Not_found
            | Some d -> d);
    }

let tags_substitutions = [("track", "tracknumber")]

let get_tags ~metadata ~extension ~mime file =
  try
    if
      not
        (Decoder.test_file ~log ~extension ~mime ~mimes:(Some mime_types#get)
           ~extensions:(Some file_extensions#get) file)
    then raise Invalid_file;
    let args, format = parse_file_decoder_args metadata in
    let opts = Hashtbl.create 10 in
    List.iter (fun (k, v) -> Hashtbl.replace opts k v) args;
    let container = Av.open_input ?format ~opts file in
    Fun.protect
      ~finally:(fun () -> Av.close container)
      (fun () ->
        (* For now we only add the metadata from the best audio track *)
        let audio_tags =
          try
            let _, s, _ = Av.find_best_audio_stream container in
            Av.get_metadata s
          with _ -> []
        in
        let tags = Av.get_input_metadata container in
        List.map
          (fun (lbl, v) ->
            try (List.assoc lbl tags_substitutions, v) with _ -> (lbl, v))
          (audio_tags @ tags))
  with
    | Invalid_file -> []
    | e ->
        let bt = Printexc.get_backtrace () in
        Utils.log_exception ~log ~bt
          (Printf.sprintf "Error while decoding file tags: %s"
             (Printexc.to_string e));
        raise Not_found

let metadata_decoder_priority =
  Dtools.Conf.int
    ~p:(Request.conf_metadata_decoder_priorities#plug "ffmpeg")
    "Priority for the ffmpeg metadata decoder" ~d:1

let () =
  Plug.register Request.mresolvers "ffmpeg" ~doc:""
    {
      Request.priority = (fun () -> metadata_decoder_priority#get);
      resolver = get_tags;
    }

(* Get the type of an input container. *)
let get_type ~ctype ~format ~url container =
  let uri = Lang_string.quote_string url in
  log#important "Requested content-type for %s%s: %s"
    (match format with
      | Some f ->
          Printf.sprintf "format: %s, uri: "
            (Lang_string.quote_string (Av.Format.get_input_name f))
      | None -> "")
    uri
    (Frame.string_of_content_type ctype);
  let audio_streams, descriptions =
    List.fold_left
      (fun (audio_streams, descriptions) (_, _, params) ->
        try
          let field = Frame.Fields.audio_n (List.length audio_streams) in
          let channels = Avcodec.Audio.get_nb_channels params in
          let samplerate = Avcodec.Audio.get_sample_rate params in
          let codec_name =
            Avcodec.Audio.string_of_id (Avcodec.Audio.get_params_id params)
          in
          let description =
            Printf.sprintf "%s: {codec: %s, %dHz, %d channel(s)}"
              (Frame.Fields.string_of_field field)
              codec_name samplerate channels
          in
          ((field, params) :: audio_streams, description :: descriptions)
        with Avutil.Error _ as exn ->
          let bt = Printexc.get_raw_backtrace () in
          Utils.log_exception ~log
            ~bt:(Printexc.raw_backtrace_to_string bt)
            (Printf.sprintf "Failed to get an audio stream info: %s"
               (Printexc.to_string exn));
          (audio_streams, descriptions))
      ([], [])
      (Av.get_audio_streams container)
  in
  let video_streams, descriptions =
    List.fold_left
      (fun (video_streams, descriptions) (_, stream, params) ->
        try
          let field = Frame.Fields.video_n (List.length video_streams) in
          let width = Avcodec.Video.get_width params in
          let height = Avcodec.Video.get_height params in
          let pixel_format =
            match Avcodec.Video.get_pixel_format params with
              | None -> "unknown"
              | Some f -> (
                  match Avutil.Pixel_format.to_string f with
                    | None -> "none"
                    | Some s -> s)
          in
          let codec_name =
            Avcodec.Video.string_of_id (Avcodec.Video.get_params_id params)
          in
          let description =
            Printf.sprintf "%s: {codec: %s, %dx%d, %s}"
              (Frame.Fields.string_of_field field)
              codec_name width height pixel_format
          in
          ( video_streams @ [(field, Av.get_avg_frame_rate stream, params)],
            descriptions @ [description] )
        with Avutil.Error _ as exn ->
          let bt = Printexc.get_raw_backtrace () in
          Utils.log_exception ~log
            ~bt:(Printexc.raw_backtrace_to_string bt)
            (Printf.sprintf "Failed to get video stream info: %s"
               (Printexc.to_string exn));
          (video_streams, descriptions))
      ([], descriptions)
      (Av.get_video_streams container)
  in
  let subtitle_streams, descriptions =
    List.fold_left
      (fun (subtitle_streams, descriptions) (_, stream, params) ->
        try
          let field = Frame.Fields.subtitles_n (List.length subtitle_streams) in
          let codec_name =
            Avcodec.Subtitle.string_of_id
              (Avcodec.Subtitle.get_params_id params)
          in
          let description =
            Printf.sprintf "%s: {codec: %s, subtitle}"
              (Frame.Fields.string_of_field field)
              codec_name
          in
          ( subtitle_streams @ [(field, Av.get_time_base stream, params)],
            descriptions @ [description] )
        with Avutil.Error _ as exn ->
          let bt = Printexc.get_raw_backtrace () in
          Utils.log_exception ~log
            ~bt:(Printexc.raw_backtrace_to_string bt)
            (Printf.sprintf "Failed to get subtitle stream info: %s"
               (Printexc.to_string exn));
          (subtitle_streams, descriptions))
      ([], descriptions)
      (Av.get_subtitle_streams container)
  in
  let _, descriptions =
    List.fold_left
      (fun (n, descriptions) (_, _, params) ->
        try
          let field = Frame.Fields.data_n (n + List.length subtitle_streams) in
          let codec_name =
            Avcodec.Unknown.string_of_id (Avcodec.Unknown.get_params_id params)
          in
          ( n + 1,
            descriptions
            @ [
                Printf.sprintf "%s: {codec: %s}"
                  (Frame.Fields.string_of_field field)
                  codec_name;
              ] )
        with Avutil.Error _ as exn ->
          let bt = Printexc.get_raw_backtrace () in
          Utils.log_exception ~log
            ~bt:(Printexc.raw_backtrace_to_string bt)
            (Printf.sprintf "Failed to get stream info: %s"
               (Printexc.to_string exn));
          (n, descriptions))
      (0, descriptions)
      (Av.get_data_streams container)
  in
  if audio_streams = [] && video_streams = [] && subtitle_streams = [] then
    failwith "No valid stream found in container.";
  let content_type =
    List.fold_left
      (fun content_type (field, params) ->
        match (params, Frame.Fields.find_opt field ctype) with
          | p, Some format when Ffmpeg_copy_content.is_format format ->
              ignore
                (Content.merge format
                   (Ffmpeg_copy_content.lift_params (Some (`Audio p))));
              Frame.Fields.add field format content_type
          | p, Some format when Ffmpeg_raw_content.Audio.is_format format ->
              let dst_format =
                Ffmpeg_raw_content.(Audio.lift_params (AudioSpecs.mk_params p))
              in
              (try ignore (Content.merge format dst_format)
               with _ when Content.compatible format dst_format -> ());
              Frame.Fields.add field dst_format content_type
          | p, Some format ->
              Frame.Fields.add field
                (Frame_base.format_of_channels ~pcm_kind:(Content.kind format)
                   (Avcodec.Audio.get_nb_channels p))
                content_type
          | _ -> content_type)
      Frame.Fields.empty audio_streams
  in
  let content_type =
    List.fold_left
      (fun content_type (field, avg_frame_rate, params) ->
        match (params, Frame.Fields.find_opt field ctype) with
          | params, Some format when Ffmpeg_copy_content.is_format format ->
              ignore
                (Content.merge format
                   (Ffmpeg_copy_content.lift_params
                      (Some
                         (`Video
                            {
                              Ffmpeg_copy_content.avg_frame_rate;
                              codec_params = params;
                            }))));
              Frame.Fields.add field format content_type
          | p, Some format when Ffmpeg_raw_content.Video.is_format format ->
              ignore
                (Content.merge format
                   Ffmpeg_raw_content.(
                     Video.lift_params (VideoSpecs.mk_params p)));
              Frame.Fields.add field format content_type
          | _, Some _ ->
              Frame.Fields.add field
                Content.(default_format Video.kind)
                content_type
          | _ -> content_type)
      content_type video_streams
  in
  let content_type =
    List.fold_left
      (fun content_type (field, time_base, params) ->
        match Frame.Fields.find_opt field ctype with
          | Some format when Ffmpeg_copy_content.is_format format ->
              ignore
                (Content.merge format
                   (Ffmpeg_copy_content.lift_params
                      (Some
                         (`Subtitle
                            {
                              Ffmpeg_copy_content.time_base;
                              codec_params = params;
                            }))));
              Frame.Fields.add field format content_type
          | _ -> content_type)
      content_type subtitle_streams
  in
  log#important "FFmpeg recognizes %s as %s" uri
    (String.concat ", " descriptions);
  log#important "Decoded content-type for %s: %s" uri
    (Frame.string_of_content_type content_type);
  content_type

let reset_streams streams =
  Streams.iter
    (fun _ s ->
      s.seen <- false;
      s.first_position <- None;
      s.pts <- None;
      s.position <- None)
    streams

let seek ~target_position ~streams ~container ticks =
  let tpos = Frame.seconds_of_main ticks in
  log#debug "Setting target position to %f" tpos;
  target_position := Some ticks;
  let ts = Int64.of_float (tpos *. 1000.) in
  let frame_duration = Lazy.force Frame.duration in
  let min_ts = Int64.of_float ((tpos -. frame_duration) *. 1000.) in
  let max_ts = ts in
  Av.seek ~fmt:`Millisecond ~min_ts ~max_ts ~ts container;
  reset_streams streams;
  ticks

let mk_eof streams buffer =
  Streams.iter
    (fun _ s ->
      match s.decoder with
        | `Audio_frame (_, decoder) -> decoder ~buffer `Flush
        | `Video_frame (_, decoder) -> decoder ~buffer `Flush
        | `Audio_packet (_, decoder) -> decoder ~buffer `Flush
        | `Video_packet (_, decoder) -> decoder ~buffer `Flush
        | `Subtitle_packet (_, decoder) -> decoder ~buffer `Flush
        | `Data_packet _ -> ())
    streams;
  Generator.add_track_mark buffer.Decoder.generator

let mk_update_position () =
  let liq_main_ticks_time_base = Ffmpeg_utils.liq_main_ticks_time_base () in
  let last_advanced_position = ref None in
  let advance_sparse_streams ~buffer streams =
    let decoded_position =
      Streams.fold
        (fun _ s acc ->
          match s.sparse with
            | `True _ -> acc
            | `False -> (
                match s.position with
                  | None -> acc
                  | Some p -> (
                      match acc with
                        | None -> Some p
                        | Some p' -> Some (min p p'))))
        streams None
    in
    match (decoded_position, !last_advanced_position) with
      | None, _ -> ()
      | Some p, Some l when p <= l -> ()
      | Some p, _ ->
          last_advanced_position := Some p;
          Streams.iter
            (fun _ s ->
              match s.sparse with
                | `True advance -> advance ~buffer p
                | `False -> ())
            streams
  in
  let ticks ~stream pts =
    Int64.to_int
      (Ffmpeg_utils.convert_time_base ~src:stream.time_base
         ~dst:liq_main_ticks_time_base pts)
  in
  fun ~buffer ~pts ~streams stream ->
    match pts with
      | None -> ()
      | Some pts -> (
          let ticks = ticks ~stream pts in
          stream.pts <- Some ticks;
          match stream.first_position with
            | Some first_pos ->
                let pos = ticks - first_pos in
                stream.position <- Some pos;
                if stream.sparse = `False then
                  advance_sparse_streams ~buffer streams
            | None ->
                stream.first_position <- Some ticks;
                stream.position <- Some 0)

let mk_push_flush ~target_position =
  let max_interleave_duration =
    Frame.main_of_seconds Ffmpeg_decoder_common.conf_max_interleave_duration#get
  in
  let decodable = ref [] in
  let push (position, ts, decode) =
    decodable :=
      (position, ts, decode)
      :: List.filter
           (fun (p, _, _) -> abs (p - position) <= max_interleave_duration)
           !decodable
  in
  let flush position =
    match !decodable with
      | [] -> ()
      | d ->
          let d =
            match !target_position with
              | None -> d
              | Some target_position ->
                  List.filter (fun (p, _, _) -> target_position <= p) d
          in
          let d =
            List.sort (fun (_, p, _) (_, p', _) -> Int64.compare p p') d
          in
          let min_position = position - max_interleave_duration in
          List.iter
            (fun (p, _, decode) -> if min_position <= p then decode ())
            d;
          decodable := []
  in
  (push, flush)

let mk_check_position ~streams ~target_position () =
  let push, flush = mk_push_flush ~target_position in
  let update_position = mk_update_position () in
  fun ~buffer ~decode ~ts ~stream pts ->
    update_position ~buffer ~pts ~streams stream;
    match (stream.pts, !target_position) with
      | Some pts, Some target_position when pts < target_position -> ()
      | Some pts, _ ->
          if not stream.seen then stream.seen <- true;
          let all_seen =
            Streams.for_all (fun _ s -> s.sparse <> `False || s.seen) streams
          in
          if all_seen then (
            flush pts;
            decode ())
          else push (pts, ts, decode)
      | None, _ ->
          log#important
            "Got packet or frame with no timestamp! Synchronization issues may \
             happen.";
          decode ()

let mk_decoder ~streams ~target_position container =
  let check_position = mk_check_position ~streams ~target_position () in
  let audio_frame =
    Streams.fold
      (fun _ s cur ->
        match s.decoder with `Audio_frame (s, _) -> s :: cur | _ -> cur)
      streams []
  in
  let audio_packet =
    Streams.fold
      (fun _ s cur ->
        match s.decoder with `Audio_packet (s, _) -> s :: cur | _ -> cur)
      streams []
  in
  let video_frame =
    Streams.fold
      (fun _ s cur ->
        match s.decoder with `Video_frame (s, _) -> s :: cur | _ -> cur)
      streams []
  in
  let video_packet =
    Streams.fold
      (fun _ s cur ->
        match s.decoder with `Video_packet (s, _) -> s :: cur | _ -> cur)
      streams []
  in
  let data_packet =
    Streams.fold
      (fun _ s cur ->
        match s.decoder with `Data_packet (s, _) -> s :: cur | _ -> cur)
      streams []
  in
  let subtitle_packet =
    Streams.fold
      (fun _ s cur ->
        match s.decoder with `Subtitle_packet (s, _) -> s :: cur | _ -> cur)
      streams []
  in
  fun buffer ->
    let rec f () =
      try
        let data =
          Av.read_input ~audio_frame ~audio_packet ~video_frame ~video_packet
            ~data_packet ~subtitle_packet container
        in
        match data with
          | `Audio_frame (i, frame) -> (
              match Streams.find_opt i streams with
                | Some ({ decoder = `Audio_frame (_, decode); _ } as stream) ->
                    check_position ~buffer
                      ~ts:(Option.value ~default:0L (Avutil.Frame.pts frame))
                      ~decode:(fun () -> decode ~buffer (`Frame frame))
                      ~stream (Avutil.Frame.pts frame)
                | _ -> f ())
          | `Audio_packet (i, packet) -> (
              match Streams.find_opt i streams with
                | Some ({ decoder = `Audio_packet (_, decode); _ } as stream) ->
                    check_position ~buffer
                      ~ts:
                        (Option.value ~default:0L
                           (Avcodec.Packet.get_dts packet))
                      ~decode:(fun () -> decode ~buffer (`Packet packet))
                      ~stream
                      (Avcodec.Packet.get_pts packet)
                | _ -> f ())
          | `Video_frame (i, frame) -> (
              match Streams.find_opt i streams with
                | Some ({ decoder = `Video_frame (_, decode); _ } as stream) ->
                    check_position ~buffer
                      ~ts:(Option.value ~default:0L (Avutil.Frame.pts frame))
                      ~decode:(fun () -> decode ~buffer (`Frame frame))
                      ~stream (Avutil.Frame.pts frame)
                | _ -> f ())
          | `Video_packet (i, packet) -> (
              match Streams.find_opt i streams with
                | Some ({ decoder = `Video_packet (_, decode); _ } as stream) ->
                    check_position ~buffer
                      ~ts:
                        (Option.value ~default:0L
                           (Avcodec.Packet.get_dts packet))
                      ~decode:(fun () -> decode ~buffer (`Packet packet))
                      ~stream
                      (Avcodec.Packet.get_pts packet)
                | _ -> f ())
          | `Subtitle_packet (i, packet) -> (
              match Streams.find_opt i streams with
                | Some ({ decoder = `Subtitle_packet (_, decode); _ } as stream)
                  ->
                    check_position ~buffer
                      ~ts:
                        (Option.value ~default:0L
                           (Avcodec.Packet.get_dts packet))
                      ~decode:(fun () -> decode ~buffer (`Subtitle packet))
                      ~stream
                      (Avcodec.Packet.get_pts packet)
                | _ -> f ())
          | `Subtitle_frame _ -> assert false
          | `Data_packet (i, packet) -> (
              match Streams.find_opt i streams with
                | Some ({ decoder = `Data_packet (_, decode); _ } as stream) ->
                    check_position ~buffer
                      ~ts:
                        (Option.value ~default:0L
                           (Avcodec.Packet.get_dts packet))
                      ~decode:(fun () -> decode ~buffer packet)
                      ~stream
                      (Avcodec.Packet.get_pts packet)
                | _ -> f ())
      with
        | Avutil.Error `Eagain | Avutil.Error `Invalid_data -> f ()
        | Avutil.Error `Exit | Avutil.Error `Eof -> raise End_of_file
        | exn ->
            let bt = Printexc.get_raw_backtrace () in
            Printexc.raise_with_backtrace exn bt
    in
    f ()

let mk_streams ~ctype ~decode_first_metadata container =
  let check_metadata stream fn =
    let is_first = ref true in
    let latest_metadata = ref None in
    fun ~buffer data ->
      let m = Av.get_metadata stream in
      Av.set_metadata stream [];
      if
        ((not !is_first) || decode_first_metadata)
        && Some m <> !latest_metadata && m <> []
      then (
        is_first := false;
        latest_metadata := Some m;
        Generator.add_metadata buffer.Decoder.generator
          (Frame.Metadata.from_list m));
      fn ~buffer data
  in
  let stream_idx = Ffmpeg_content_base.new_stream_idx () in
  let streams, _ =
    List.fold_left
      (fun (streams, pos) (idx, stream, params) ->
        let field = Frame.Fields.audio_n pos in
        match Frame.Fields.find_opt field ctype with
          | Some format when Ffmpeg_copy_content.is_format format ->
              ( add_stream ~sparse:`False idx stream
                  (`Audio_packet
                     ( stream,
                       check_metadata stream
                         (Ffmpeg_copy_decoder.mk_audio_decoder ~stream_idx
                            ~format ~field ~stream params) ))
                  streams,
                pos + 1 )
          | _ -> (streams, pos + 1))
      (Streams.empty, 0)
      (Av.get_audio_streams container)
  in
  let streams, _ =
    List.fold_left
      (fun (streams, pos) (idx, stream, params) ->
        let field = Frame.Fields.audio_n pos in
        match Frame.Fields.find_opt field ctype with
          | Some format when Ffmpeg_raw_content.Audio.is_format format ->
              ( add_stream ~sparse:`False idx stream
                  (`Audio_frame
                     ( stream,
                       check_metadata stream
                         (Ffmpeg_raw_decoder.mk_audio_decoder ~stream_idx
                            ~format ~stream ~field params) ))
                  streams,
                pos + 1 )
          | Some format when Content.Audio.is_format format ->
              let channels = Content.Audio.channels_of_format format in
              ( add_stream ~sparse:`False idx stream
                  (`Audio_frame
                     ( stream,
                       check_metadata stream
                         (Ffmpeg_internal_decoder.mk_audio_decoder ~channels
                            ~stream ~field ~pcm_kind:Content.Audio.kind params)
                     ))
                  streams,
                pos + 1 )
          | Some format when Content_pcm_s16.is_format format ->
              let channels = Content_pcm_s16.channels_of_format format in
              ( add_stream ~sparse:`False idx stream
                  (`Audio_frame
                     ( stream,
                       check_metadata stream
                         (Ffmpeg_internal_decoder.mk_audio_decoder ~channels
                            ~stream ~field ~pcm_kind:Content_pcm_s16.kind params)
                     ))
                  streams,
                pos + 1 )
          | Some format when Content_pcm_f32.is_format format ->
              let channels = Content_pcm_f32.channels_of_format format in
              ( add_stream ~sparse:`False idx stream
                  (`Audio_frame
                     ( stream,
                       check_metadata stream
                         (Ffmpeg_internal_decoder.mk_audio_decoder ~channels
                            ~stream ~field ~pcm_kind:Content_pcm_f32.kind params)
                     ))
                  streams,
                pos + 1 )
          | _ -> (streams, pos + 1))
      (streams, 0)
      (Av.get_audio_streams container)
  in
  let streams, _ =
    List.fold_left
      (fun (streams, pos) (idx, stream, params) ->
        let field = Frame.Fields.video_n pos in
        match Frame.Fields.find_opt field ctype with
          | Some format when Ffmpeg_copy_content.is_format format ->
              ( add_stream ~sparse:`False idx stream
                  (`Video_packet
                     ( stream,
                       check_metadata stream
                         (Ffmpeg_copy_decoder.mk_video_decoder ~stream_idx
                            ~format ~field ~stream params) ))
                  streams,
                pos + 1 )
          | _ -> (streams, pos + 1))
      (streams, 0)
      (Av.get_video_streams container)
  in
  let streams, _ =
    List.fold_left
      (fun (streams, pos) (idx, stream, params) ->
        let field = Frame.Fields.video_n pos in
        match Frame.Fields.find_opt field ctype with
          | Some format when Ffmpeg_raw_content.Video.is_format format ->
              ( add_stream ~sparse:`False idx stream
                  (`Video_frame
                     ( stream,
                       check_metadata stream
                         (Ffmpeg_raw_decoder.mk_video_decoder ~stream_idx
                            ~format ~stream ~field params) ))
                  streams,
                pos + 1 )
          | Some format when Content.Video.is_format format ->
              (* Set ideal video dimensions from source file *)
              let src_width = Avcodec.Video.get_width params in
              let src_height = Avcodec.Video.get_height params in
              let ideal_size =
                Frame.
                  {
                    width = src_width;
                    height = src_height;
                    source = "ffmpeg decoder";
                  }
              in
              ignore (Frame.video_dimensions ~ideal_size ());
              let width, height = Content.Video.dimensions_of_format format in
              ( add_stream ~sparse:`False idx stream
                  (`Video_frame
                     ( stream,
                       check_metadata stream
                         (Ffmpeg_internal_decoder.mk_video_decoder ~width
                            ~height ~stream ~field params) ))
                  streams,
                pos + 1 )
          | _ -> (streams, pos + 1))
      (streams, 0)
      (Av.get_video_streams container)
  in
  let streams, _ =
    List.fold_left
      (fun (streams, pos) (idx, stream, params) ->
        let field = Frame.Fields.subtitles_n pos in
        match Frame.Fields.find_opt field ctype with
          | Some format when Ffmpeg_copy_content.is_format format ->
              let { Ffmpeg_decoder_common.decoder; advance } =
                Ffmpeg_copy_decoder.mk_subtitle_decoder ~stream_idx ~format
                  ~field ~stream params
              in
              ( add_stream ~sparse:(`True advance) idx stream
                  (`Subtitle_packet (stream, decoder))
                  streams,
                pos + 1 )
          | _ -> (streams, pos + 1))
      (streams, 0)
      (Av.get_subtitle_streams container)
  in
  let streams, _ =
    List.fold_left
      (fun (streams, pos) (idx, stream, params) ->
        try
          if Avcodec.Unknown.get_params_id params = `Timed_id3 then
            ( add_stream
                ~sparse:(`True (fun ~buffer:_ _ -> ()))
                idx stream
                (`Data_packet
                   ( stream,
                     fun ~buffer p ->
                       let metadata =
                         try parse_timed_id3 (Avcodec.Packet.content p)
                         with _ -> []
                       in
                       if metadata <> [] then
                         Generator.add_metadata buffer.Decoder.generator
                           (Frame.Metadata.from_list metadata) ))
                streams,
              pos + 1 )
          else (streams, pos + 1)
        with Avutil.Error _ as exn ->
          let bt = Printexc.get_raw_backtrace () in
          Utils.log_exception ~log
            ~bt:(Printexc.raw_backtrace_to_string bt)
            (Printf.sprintf "Failed to get stream info: %s"
               (Printexc.to_string exn));
          (streams, pos + 1))
      (streams, 0)
      (Av.get_data_streams container)
  in
  streams

let create_decoder ~ctype ~metadata fname =
  let args, format = parse_file_decoder_args metadata in
  let file_duration = dresolver ~metadata fname in
  let remaining = Atomic.make file_duration in
  let set_remaining ~pts ~duration stream =
    let pts =
      Option.map
        (fun pts -> Int64.add pts (Option.value ~default:0L duration))
        pts
    in
    match (file_duration, pts) with
      | None, _ | Some _, None -> ()
      | Some d, Some pts -> (
          let { Avutil.num; den } = Av.get_time_base stream in
          let position =
            Int64.to_float (Int64.mul (Int64.of_int num) pts) /. float den
          in
          match Atomic.get remaining with
            | None -> Atomic.set remaining (Some (d -. position))
            | Some r -> Atomic.set remaining (Some (min (d -. position) r)))
  in
  let get_remaining () =
    match Atomic.get remaining with
      | None -> -1
      | Some r -> Frame.main_of_seconds r
  in
  let opts = Hashtbl.create 10 in
  List.iter (fun (k, v) -> Hashtbl.replace opts k v) args;
  let ext = Filename.extension fname in
  if List.exists (fun s -> ext = "." ^ s) image_file_extensions#get then (
    Hashtbl.replace opts "loop" (`Int 1);
    Hashtbl.replace opts "framerate" (`Int (Lazy.force Frame.video_rate)));
  let container = Av.open_input ?format ~opts fname in
  let streams = mk_streams ~ctype ~decode_first_metadata:false container in
  Streams.iter
    (fun _ s ->
      let decoder =
        match s.decoder with
          | `Audio_packet (stream, decoder) ->
              let decoder ~buffer packet =
                (match packet with
                  | `Packet packet ->
                      set_remaining stream
                        ~pts:(Avcodec.Packet.get_pts packet)
                        ~duration:(Avcodec.Packet.get_duration packet)
                  | _ -> ());
                decoder ~buffer packet
              in
              `Audio_packet (stream, decoder)
          | `Audio_frame (stream, decoder) ->
              let decoder ~buffer frame =
                (match frame with
                  | `Frame frame ->
                      set_remaining stream ~pts:(Avutil.Frame.pts frame)
                        ~duration:(Avutil.Frame.duration frame)
                  | _ -> ());
                decoder ~buffer frame
              in
              `Audio_frame (stream, decoder)
          | `Video_packet (stream, decoder) ->
              let decoder ~buffer packet =
                (match packet with
                  | `Packet packet ->
                      set_remaining stream
                        ~pts:(Avcodec.Packet.get_pts packet)
                        ~duration:(Avcodec.Packet.get_duration packet)
                  | _ -> ());
                decoder ~buffer packet
              in
              `Video_packet (stream, decoder)
          | `Video_frame (stream, decoder) ->
              let decoder ~buffer frame =
                (match frame with
                  | `Frame frame ->
                      set_remaining stream ~pts:(Avutil.Frame.pts frame)
                        ~duration:(Avutil.Frame.duration frame)
                  | _ -> ());
                decoder ~buffer frame
              in
              `Video_frame (stream, decoder)
          | `Subtitle_packet (stream, decoder) ->
              `Subtitle_packet (stream, decoder)
          | `Data_packet (stream, decoder) -> `Data_packet (stream, decoder)
      in
      s.decoder <- decoder)
    streams;
  let close () = Av.close container in
  let target_position = ref None in
  ( {
      Decoder.seek =
        (fun ticks ->
          match file_duration with
            | None -> -1
            | Some d -> (
                let target =
                  ticks + Frame.main_of_seconds d - get_remaining ()
                in
                match seek ~target_position ~streams ~container target with
                  | 0 -> 0
                  | _ -> ticks));
      decode = mk_decoder ~streams ~target_position container;
      eof = mk_eof streams;
      close;
    },
    get_remaining )

let create_file_decoder ~metadata ~ctype filename =
  let decoder, remaining = create_decoder ~ctype ~metadata filename in
  Decoder.file_decoder ~filename ~remaining ~ctype decoder

let create_stream_decoder ~ctype mime input =
  let seek_input =
    match input.Decoder.lseek with
      | None -> None
      | Some fn -> Some (fun len _ -> fn len)
  in
  let mime, (args, format) =
    match String.split_on_char ';' mime with
      | "application/ffmpeg" :: args ->
          ("application/ffmpeg", parse_input_args (String.concat ";" args))
      | _ -> (mime, ([], None))
  in
  let opts = Hashtbl.create 10 in
  List.iter (fun (k, v) -> Hashtbl.replace opts k v) args;
  if List.exists (fun s -> mime = s) image_mime_types#get then (
    Hashtbl.replace opts "loop" (`Int 1);
    Hashtbl.replace opts "framerate" (`Int (Lazy.force Frame.video_rate)));
  let container =
    Av.open_input_stream ?seek:seek_input ~opts ?format input.Decoder.read
  in
  if Hashtbl.length opts > 0 then
    Runtime_error.raise ~pos:[]
      ~message:
        (Printf.sprintf "Unrecognized options: %s"
           (Ffmpeg_format.string_of_options opts))
      "ffmpeg_decoder";
  let streams = mk_streams ~ctype ~decode_first_metadata:true container in
  let target_position = ref None in
  let close () = Av.close container in
  {
    Decoder.seek = seek ~target_position ~streams ~container;
    decode = mk_decoder ~streams ~target_position container;
    eof = mk_eof streams;
    close;
  }

let get_file_type ~metadata ~ctype filename =
  (* If file is an image, leave internal decoding to
     the image decoder. *)
    match
      ( Utils.get_ext_opt filename,
        Frame.Fields.find_opt Frame.Fields.video ctype )
    with
    | Some ext, Some format
      when List.mem ext image_file_extensions#get
           && Content.Video.is_format format ->
        Frame.Fields.make ()
    | _ ->
        let args, format = parse_file_decoder_args metadata in
        let opts = Hashtbl.create 10 in
        List.iter (fun (k, v) -> Hashtbl.replace opts k v) args;
        let container = Av.open_input ?format ~opts filename in
        Fun.protect
          ~finally:(fun () -> Av.close container)
          (fun () -> get_type ~format ~ctype ~url:filename container)

let () =
  Plug.register Decoder.decoders "ffmpeg"
    ~doc:
      "Use FFmpeg to decode any file or stream if its MIME type or file \
       extension is appropriate."
    {
      Decoder.priority = (fun () -> priority#get);
      file_extensions =
        (fun () -> Some (file_extensions#get @ image_file_extensions#get));
      mime_types = (fun () -> Some (mime_types#get @ image_mime_types#get));
      file_type =
        (fun ~metadata ~ctype filename ->
          Some (get_file_type ~metadata ~ctype filename));
      file_decoder = Some create_file_decoder;
      stream_decoder = Some create_stream_decoder;
    }
