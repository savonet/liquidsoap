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

open Mm

(** Decode and read ogg files. *)

module P = Image.Generic.Pixel

let log = Log.make ["decoder"; "ogg"]

(** Generic decoder *)

exception Channels of int

let converter () =
  let current_format = ref None in
  fun format ->
    let format =
      match format with
        | Ogg_decoder.Yuvj_422 -> P.YUVJ422
        | Ogg_decoder.Yuvj_420 -> P.YUVJ420
        | Ogg_decoder.Yuvj_444 -> P.YUVJ444
    in
    match !current_format with
      | Some x when fst x = format -> snd x
      | _ ->
          let converter =
            Video_converter.find_converter (P.YUV format) (P.YUV P.YUVJ420)
          in
          current_format := Some (format, converter);
          converter

(** Convert a video frame to YUV *)
let video_convert scale =
  let converter = converter () in
  fun buf ->
    let width = Lazy.force Frame.video_width in
    let height = Lazy.force Frame.video_height in
    if buf.Ogg_decoder.format <> Ogg_decoder.Yuvj_420 then (
      let img =
        Image.YUV420.make buf.Ogg_decoder.frame_width
          buf.Ogg_decoder.frame_height buf.Ogg_decoder.y
          buf.Ogg_decoder.y_stride buf.Ogg_decoder.u buf.Ogg_decoder.v
          buf.Ogg_decoder.uv_stride
      in
      let img2 = Video.Image.create width height in
      scale img img2;
      img2)
    else (
      let converter = converter buf.Ogg_decoder.format in
      let yuv = Video.Image.create width height in
      let frame = Image.Generic.of_YUV420 yuv in
      let sframe =
        Image.YUV420.make buf.Ogg_decoder.frame_width
          buf.Ogg_decoder.frame_height buf.Ogg_decoder.y
          buf.Ogg_decoder.y_stride buf.Ogg_decoder.u buf.Ogg_decoder.v
          buf.Ogg_decoder.uv_stride
      in
      converter (Image.Generic.of_YUV420 sframe) frame;
      yuv)

let demuxer_log x = log#debug "%s" x

let create_decoder ?(merge_tracks = false) source input =
  let decoder =
    let callbacks =
      {
        Ogg_decoder.read = input.Decoder.read;
        seek = input.Decoder.lseek;
        tell = input.Decoder.tell;
      }
    in
    Ogg_decoder.init ~log:demuxer_log callbacks
  in
  let video_scale = Video_converter.scaler () ~proportional:true in
  let started = ref false in
  let tracks = Ogg_decoder.get_standard_tracks decoder in
  let first_meta = ref true in
  let init ~reset buffer =
    if reset then (
      Ogg_decoder.reset decoder;
      Ogg_decoder.update_standard_tracks decoder tracks;

      (* We enforce that all contents end together, otherwise there will
       * be a lag between different content types in the next track. *)
      if not merge_tracks then
        Generator.add_break ~sync:true buffer.Decoder.generator);
    let add_meta f t =
      (* Initial metadata in files is handled separately. *)
      if source = `Stream || (merge_tracks && not !first_meta) then (
        let _, (v, m) = f decoder t in
        let metas = Hashtbl.create 10 in
        List.iter
          (fun (x, y) -> Hashtbl.add metas (String.lowercase_ascii x) y)
          m;
        Hashtbl.add metas "vendor" v;
        Generator.add_metadata buffer.Decoder.generator metas);
      first_meta := false
    in
    let drop_track d t =
      match t with None -> () | Some t -> Ogg_decoder.drop_track d t
    in
    (* Make sure the stream has what we need *)
    (* TODO this should be done based on the kind, not the mode,
     *      which should be (re)set accordingly *)
    match
      ( tracks.Ogg_decoder.audio_track,
        tracks.Ogg_decoder.video_track,
        Generator.mode buffer.Decoder.generator )
    with
      | Some audio, Some video, `Both ->
          add_meta Ogg_decoder.audio_info audio;
          add_meta Ogg_decoder.video_info video
      | Some audio, video, `Audio ->
          drop_track decoder video;
          add_meta Ogg_decoder.audio_info audio
      | audio, Some video, `Video ->
          drop_track decoder audio;
          add_meta Ogg_decoder.video_info video
      | _ -> failwith "Ogg stream does not contain required data"
  in
  let decode buffer =
    let decode_audio, decode_video =
      match Generator.mode buffer.Decoder.generator with
        | `Both -> (true, true)
        | `Audio -> (true, false)
        | `Video -> (false, true)
        | _ -> assert false
    in
    try
      if not !started then (
        init ~reset:false buffer;
        started := true);
      if Ogg_decoder.eos decoder then
        if merge_tracks || source = `Stream then init ~reset:true buffer
        else raise Ogg_decoder.End_of_stream;
      let audio_feed track buf =
        let info, _ = Ogg_decoder.audio_info decoder track in
        buffer.Decoder.put_pcm ~samplerate:info.Ogg_decoder.sample_rate buf
      in
      let video_feed track buf =
        let info, _ = Ogg_decoder.video_info decoder track in
        let rgb = video_convert video_scale buf in
        let fps =
          {
            Decoder.num = info.Ogg_decoder.fps_numerator;
            den = info.Ogg_decoder.fps_denominator;
          }
        in
        buffer.Decoder.put_yuva420p ~fps (Video.single rgb)
      in
      let decode_audio, decode_video =
        if decode_audio && decode_video then
          (* Only decode the one which is late, so that we don't have memory
             problems. *)
          if
            Generator.audio_length buffer.Decoder.generator
            < Generator.video_length buffer.Decoder.generator
          then (true, false)
          else (false, true)
        else (decode_audio, decode_video)
      in
      if decode_audio then (
        let track = Option.get tracks.Ogg_decoder.audio_track in
        Ogg_decoder.decode_audio decoder track (fun buf ->
            audio_feed track (Audio.of_array buf)));
      if decode_video then (
        let track = Option.get tracks.Ogg_decoder.video_track in
        Ogg_decoder.decode_video decoder track (video_feed track))
    with
      (* We catch [Ogg_decoder.End_of_stream] only if asked to
       * to merge logical tracks or with a stream source. 
       * In this case, we try to reset the decoder to see if 
       * there could be another sequentialized logical stream
       * starting. Actual reset is handled in the
       * decoding function since we need the actual
       * buffer to add metadata etc. *)
      | Ogg_decoder.End_of_stream when merge_tracks || source = `Stream ->
          ()
          (* We catch Ogg.Out_of_sync only in
           * stream mode. Ogg/theora streams, for instance,
           * in icecast contain the header (packet 0) and
           * then current stream, with packet 1543 for instance..
           * Note: we only catch during audio/video decoding
           * which implies that the stream has already been
           * parsed as ogg. Indeed, Ogg.Out_of_sync when
           * parsing ogg means that the stream is not ogg... *)
      | Ogg.Out_of_sync when source = `Stream -> ()
  in
  let seek offset =
    try
      let time_offset = Frame.seconds_of_main offset in
      let new_time = Ogg_decoder.seek ~relative:true decoder time_offset in
      Frame.main_of_seconds new_time
    with Ogg_decoder.End_of_stream | Ogg.End_of_stream ->
      log#info "End of track reached while seeking!";
      0
  in
  { Decoder.decode; seek }

(** File decoder *)

let file_type ~ctype:_ filename =
  let decoder, fd = Ogg_decoder.init_from_file ~log:demuxer_log filename in
  let tracks = Ogg_decoder.get_standard_tracks decoder in
  Tutils.finalize
    ~k:(fun () -> Unix.close fd)
    (fun () ->
      let audio =
        match tracks.Ogg_decoder.audio_track with
          | None -> 0
          | Some t ->
              let info, _ = Ogg_decoder.audio_info decoder t in
              info.Ogg_decoder.channels
      in
      let video = if tracks.Ogg_decoder.video_track <> None then 1 else 0 in
      log#info "File %s recognized as audio=%d video=%d."
        (Utils.quote_string filename)
        audio video;
      let audio =
        if audio = 0 then Content.None.format
        else
          Content.(
            Audio.lift_params
              {
                Contents.channel_layout =
                  lazy (Audio_converter.Channel_layout.layout_of_channels audio);
              })
      in
      let video =
        if video = 0 then Content.None.format
        else Content.(default_format Video.kind)
      in
      Some { Frame.audio; video; midi = Content.None.format })

let mime_types =
  Dtools.Conf.list
    ~p:(Decoder.conf_mime_types#plug "ogg")
    ~d:
      [
        "application/ogg";
        "application/x-ogg";
        "audio/x-ogg";
        "audio/ogg";
        "video/ogg";
      ]
    "Mime-types used for guessing OGG format."

let file_extensions =
  Dtools.Conf.list
    ~p:(Decoder.conf_file_extensions#plug "ogg")
    "File extensions used for guessing OGG format"
    ~d:["ogv"; "oga"; "ogx"; "ogg"; "opus"]

let priority =
  Dtools.Conf.int
    ~p:(Decoder.conf_priorities#plug "ogg")
    "Priority for the OGG decoder" ~d:1

let create_file_decoder ~metadata:_ ~ctype filename =
  Decoder.opaque_file_decoder ~filename ~ctype
    (create_decoder ~merge_tracks:true `File)

let () =
  Decoder.decoders#register "OGG"
    ~sdoc:"Decode a file as OGG provided that libogg accepts it."
    {
      Decoder.media_type = `Audio_video;
      priority = (fun () -> priority#get);
      file_extensions = (fun () -> Some file_extensions#get);
      mime_types = (fun () -> Some mime_types#get);
      file_type;
      file_decoder = Some create_file_decoder;
      stream_decoder = Some (fun ~ctype:_ _ -> create_decoder `Stream);
    }

(** Metadata *)

let get_tags file =
  if
    not
      (Decoder.test_file ~log ~mimes:mime_types#get
         ~extensions:file_extensions#get file)
  then raise Not_found;
  let decoder, fd = Ogg_decoder.init_from_file ~log:demuxer_log file in
  let tracks = Ogg_decoder.get_standard_tracks decoder in
  Tutils.finalize
    ~k:(fun () -> Unix.close fd)
    (fun () ->
      let get f t =
        match t with
          | Some t ->
              let _, (_, m) = f decoder t in
              m
          | _ -> []
      in
      get Ogg_decoder.audio_info tracks.Ogg_decoder.audio_track
      @ get Ogg_decoder.video_info tracks.Ogg_decoder.video_track)

let () = Request.mresolvers#register "OGG" get_tags
