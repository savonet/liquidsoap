(*****************************************************************************

   Liquidsoap, a programmable audio stream generator.
   Copyright 2003-2021 Savonet team

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

let log = Log.make ["decoder"; "ffmpeg"]

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
        "caf";
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

let duration file =
  let container = Av.open_input file in
  Tutils.finalize
    ~k:(fun () -> Av.close container)
    (fun () ->
      let duration = Av.get_input_duration container ~format:`Millisecond in
      Option.map (fun d -> Int64.to_float d /. 1000.) duration)

let () =
  Request.dresolvers#register "FFMPEG" (fun fname ->
      match duration fname with None -> raise Not_found | Some d -> d)

let tags_substitutions = [("track", "tracknumber")]

let get_tags file =
  let container = Av.open_input file in
  Tutils.finalize
    ~k:(fun () -> Av.close container)
    (fun () ->
      let tags = Av.get_input_metadata container in
      List.map
        (fun (lbl, v) ->
          try (List.assoc lbl tags_substitutions, v) with _ -> (lbl, v))
        tags)

let () = Request.mresolvers#register "FFMPEG" get_tags

(* Get the type of an input container. *)
let get_type ~ctype ~url container =
  let audio_params, descr =
    try
      let _, _, params = Av.find_best_audio_stream container in
      let channels = Avcodec.Audio.get_nb_channels params in
      let samplerate = Avcodec.Audio.get_sample_rate params in
      let codec_name =
        Avcodec.Audio.string_of_id (Avcodec.Audio.get_params_id params)
      in
      ( Some params,
        [
          Printf.sprintf "audio: {codec: %s, %dHz, %d channel(s)}" codec_name
            samplerate channels;
        ] )
    with Avutil.Error _ -> (None, [])
  in
  let video_params, descr =
    try
      let _, _, params = Av.find_best_video_stream container in
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
      ( Some params,
        Printf.sprintf "video: {codec: %s, %dx%d, %s}" codec_name width height
          pixel_format
        :: descr )
    with Avutil.Error _ -> (None, descr)
  in
  if audio_params = None && video_params = None then
    failwith "No valid stream found in file.";
  let audio =
    match (audio_params, ctype.Frame.audio) with
      | None, _ -> Content.None.format
      | Some p, format when Ffmpeg_copy_content.Audio.is_format format ->
          ignore
            (Content.merge format
               Ffmpeg_copy_content.(Audio.lift_params (Some p)));
          format
      | Some p, format when Ffmpeg_raw_content.Audio.is_format format ->
          ignore
            (Content.merge format
               Ffmpeg_raw_content.(Audio.lift_params (AudioSpecs.mk_params p)));
          format
      | Some p, _ ->
          Content.(
            Audio.lift_params
              {
                Contents.channel_layout =
                  lazy
                    (Audio_converter.Channel_layout.layout_of_channels
                       (Avcodec.Audio.get_nb_channels p));
              })
  in
  let video =
    match (video_params, ctype.Frame.video) with
      | None, _ -> Content.None.format
      | Some p, format when Ffmpeg_copy_content.Video.is_format format ->
          ignore
            (Content.merge format
               Ffmpeg_copy_content.(Video.lift_params (Some p)));
          format
      | Some p, format when Ffmpeg_raw_content.Video.is_format format ->
          ignore
            (Content.merge format
               Ffmpeg_raw_content.(Video.lift_params (VideoSpecs.mk_params p)));
          format
      | _ -> Content.(default_format Video.kind)
  in
  let ctype = { Frame.audio; video; midi = Content.None.format } in
  log#info "ffmpeg recognizes %s as: %s and content-type: %s."
    (Utils.quote_string url)
    (String.concat ", " (List.rev descr))
    (Frame.string_of_content_type ctype);
  ctype

let seek ~target_position ~container ticks =
  let tpos = Frame.seconds_of_main ticks in
  log#debug "Setting target position to %f" tpos;
  target_position := Some tpos;
  let ts = Int64.of_float (tpos *. 1000.) in
  let frame_duration = Lazy.force Frame.duration in
  let min_ts = Int64.of_float ((tpos -. frame_duration) *. 1000.) in
  let max_ts = ts in
  Av.seek ~fmt:`Millisecond ~min_ts ~max_ts ~ts container;
  ticks

let mk_decoder ?audio ?video ~decode_first_metadata ~target_position container =
  let check_metadata (_, stream, _) =
    let is_first = ref true in
    let latest_metadata = ref None in
    fun buffer ->
      let m = Av.get_metadata stream in
      if
        ((not !is_first) || decode_first_metadata) && Some m <> !latest_metadata
      then (
        is_first := false;
        latest_metadata := Some m;
        Generator.add_metadata buffer.Decoder.generator
          (Frame.metadata_of_list m))
  in
  let no_metadata _ = () in
  let no_decoder = (-1, [], fun ~buffer:_ _ -> assert false) in
  let pack (idx, stream, decoder) = (idx, [stream], decoder) in
  let ( (audio_frame_idx, audio_frame, audio_frame_decoder),
        (audio_packet_idx, audio_packet, audio_packet_decoder),
        check_audio_metadata ) =
    match audio with
      | None -> (no_decoder, no_decoder, no_metadata)
      | Some (`Packet packet) -> (no_decoder, pack packet, check_metadata packet)
      | Some (`Frame frame) -> (pack frame, no_decoder, check_metadata frame)
  in
  let ( (video_frame_idx, video_frame, video_frame_decoder),
        (video_packet_idx, video_packet, video_packet_decoder),
        check_video_metadata ) =
    match video with
      | None -> (no_decoder, no_decoder, no_metadata)
      | Some (`Packet packet) -> (no_decoder, pack packet, check_metadata packet)
      | Some (`Frame frame) -> (pack frame, no_decoder, check_metadata frame)
  in
  let check_pts stream pts =
    match (pts, !target_position) with
      | Some pts, Some target_position ->
          let { Avutil.num; den } = Av.get_time_base stream in
          let position = Int64.to_float pts *. float num /. float den in
          target_position <= position
      | _ -> true
  in
  fun buffer ->
    let rec f () =
      try
        let data =
          Av.read_input ~audio_frame ~audio_packet ~video_frame ~video_packet
            container
        in
        check_audio_metadata buffer;
        check_video_metadata buffer;
        match data with
          | `Audio_frame (i, frame) when i = audio_frame_idx ->
              if check_pts (List.hd audio_frame) (Ffmpeg_utils.best_pts frame)
              then audio_frame_decoder ~buffer frame
              else f ()
          | `Audio_packet (i, packet) when i = audio_packet_idx ->
              if
                check_pts (List.hd audio_packet) (Avcodec.Packet.get_pts packet)
              then audio_packet_decoder ~buffer packet
              else f ()
          | `Video_frame (i, frame) when i = video_frame_idx ->
              if check_pts (List.hd video_frame) (Ffmpeg_utils.best_pts frame)
              then video_frame_decoder ~buffer frame
              else f ()
          | `Video_packet (i, packet) when i = video_packet_idx ->
              if
                check_pts (List.hd video_packet) (Avcodec.Packet.get_pts packet)
              then video_packet_decoder ~buffer packet
              else f ()
          | _ -> ()
      with
        | Avutil.Error `Invalid_data -> f ()
        | Avutil.Error `Eof ->
            Generator.add_break ?sync:(Some true) buffer.Decoder.generator;
            raise End_of_file
        | exn ->
            let bt = Printexc.get_raw_backtrace () in
            Generator.add_break ?sync:(Some true) buffer.Decoder.generator;
            Printexc.raise_with_backtrace exn bt
    in
    f ()

let mk_streams ~ctype container =
  let audio =
    try
      match ctype.Frame.audio with
        | f when Content.None.is_format f -> None
        | f when Ffmpeg_copy_content.Audio.is_format f ->
            Some
              (`Packet
                (Ffmpeg_copy_decoder.mk_audio_decoder ~format:f container))
        | f when Ffmpeg_raw_content.Audio.is_format f ->
            Some
              (`Frame (Ffmpeg_raw_decoder.mk_audio_decoder ~format:f container))
        | f ->
            let channels = Content.Audio.channels_of_format f in
            Some
              (`Frame
                (Ffmpeg_internal_decoder.mk_audio_decoder ~channels container))
    with Avutil.Error _ -> None
  in
  let video =
    try
      match ctype.Frame.video with
        | f when Content.None.is_format f -> None
        | f when Ffmpeg_copy_content.Video.is_format f ->
            Some
              (`Packet
                (Ffmpeg_copy_decoder.mk_video_decoder ~format:f container))
        | f when Ffmpeg_raw_content.Video.is_format f ->
            Some
              (`Frame (Ffmpeg_raw_decoder.mk_video_decoder ~format:f container))
        | _ ->
            Some (`Frame (Ffmpeg_internal_decoder.mk_video_decoder container))
    with Avutil.Error _ -> None
  in
  (audio, video)

let create_decoder ~ctype fname =
  let duration = duration fname in
  let remaining = ref duration in
  let m = Mutex.create () in
  let set_remaining stream pts =
    Tutils.mutexify m
      (fun () ->
        match (duration, pts) with
          | None, _ | Some _, None -> ()
          | Some d, Some pts -> (
              let { Avutil.num; den } = Av.get_time_base stream in
              let position =
                Int64.to_float (Int64.mul (Int64.of_int num) pts) /. float den
              in
              match !remaining with
                | None -> remaining := Some (d -. position)
                | Some r -> remaining := Some (min (d -. position) r)))
      ()
  in
  let get_remaining =
    Tutils.mutexify m (fun () ->
        match !remaining with None -> -1 | Some r -> Frame.main_of_seconds r)
  in
  let opts = Hashtbl.create 10 in
  let ext = Filename.extension fname in
  if List.exists (fun s -> ext = "." ^ s) image_file_extensions#get then (
    Hashtbl.add opts "loop" (`Int 1);
    Hashtbl.add opts "framerate" (`Int (Lazy.force Frame.video_rate)));
  let container = Av.open_input ~opts fname in
  let audio, video = mk_streams ~ctype container in
  let audio =
    match audio with
      | Some (`Packet (idx, stream, decoder)) ->
          let decoder ~buffer packet =
            set_remaining stream (Avcodec.Packet.get_pts packet);
            decoder ~buffer packet
          in
          Some (`Packet (idx, stream, decoder))
      | Some (`Frame (idx, stream, decoder)) ->
          let decoder ~buffer frame =
            set_remaining stream (Ffmpeg_utils.best_pts frame);
            decoder ~buffer frame
          in
          Some (`Frame (idx, stream, decoder))
      | None -> None
  in
  let video =
    match video with
      | Some (`Packet (idx, stream, decoder)) ->
          let decoder ~buffer packet =
            set_remaining stream (Avcodec.Packet.get_pts packet);
            decoder ~buffer packet
          in
          Some (`Packet (idx, stream, decoder))
      | Some (`Frame (idx, stream, decoder)) ->
          let decoder ~buffer frame =
            set_remaining stream (Ffmpeg_utils.best_pts frame);
            decoder ~buffer frame
          in
          Some (`Frame (idx, stream, decoder))
      | None -> None
  in
  let close () = Av.close container in
  let target_position = ref None in
  ( {
      Decoder.seek =
        (fun ticks ->
          match duration with
            | None -> -1
            | Some d -> (
                let target =
                  ticks + Frame.main_of_seconds d - get_remaining ()
                in
                match seek ~target_position ~container target with
                  | 0 -> 0
                  | _ -> ticks));
      decode =
        mk_decoder ?audio ?video ~decode_first_metadata:false ~target_position
          container;
    },
    close,
    get_remaining )

let create_file_decoder ~metadata:_ ~ctype filename =
  let decoder, close, remaining = create_decoder ~ctype filename in
  Decoder.file_decoder ~filename ~close ~remaining ~ctype decoder

let create_stream_decoder ~ctype mime input =
  let seek_input =
    match input.Decoder.lseek with
      | None -> None
      | Some fn -> Some (fun len _ -> fn len)
  in
  let opts = Hashtbl.create 10 in
  if List.exists (fun s -> mime = s) image_mime_types#get then (
    Hashtbl.add opts "loop" (`Int 1);
    Hashtbl.add opts "framerate" (`Int (Lazy.force Frame.video_rate)));
  let container =
    Av.open_input_stream ?seek:seek_input ~opts input.Decoder.read
  in
  let audio, video = mk_streams ~ctype container in
  let target_position = ref None in
  {
    Decoder.seek = seek ~target_position ~container;
    decode =
      mk_decoder ?audio ?video ~decode_first_metadata:true ~target_position
        container;
  }

let get_file_type ~ctype filename =
  (* If file is an image, leave internal decoding to
     the image decoder. *)
  match (Utils.get_ext_opt filename, ctype.Frame.video) with
    | Some ext, format
      when List.mem ext image_file_extensions#get
           && Content.Video.is_format format ->
        Frame.
          {
            audio = Content.None.format;
            video = Content.None.format;
            midi = Content.None.format;
          }
    | _ ->
        let container = Av.open_input filename in
        Tutils.finalize
          ~k:(fun () -> Av.close container)
          (fun () -> get_type ~ctype ~url:filename container)

let () =
  Decoder.decoders#register "FFMPEG"
    ~sdoc:
      "Use libffmpeg to decode any file or stream if its MIME type or file \
       extension is appropriate."
    {
      Decoder.media_type = `Audio_video;
      priority = (fun () -> priority#get);
      file_extensions =
        (fun () -> Some (file_extensions#get @ image_file_extensions#get));
      mime_types = (fun () -> Some (mime_types#get @ image_mime_types#get));
      file_type = (fun ~ctype filename -> Some (get_file_type ~ctype filename));
      file_decoder = Some create_file_decoder;
      stream_decoder = Some create_stream_decoder;
    }
