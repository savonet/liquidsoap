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
        | Ogg_demuxer.Yuvj_422 -> P.YUVJ422
        | Ogg_demuxer.Yuvj_420 -> P.YUVJ420
        | Ogg_demuxer.Yuvj_444 -> P.YUVJ444
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
    if buf.Ogg_demuxer.format <> Ogg_demuxer.Yuvj_420 then (
      let img =
        Image.YUV420.make buf.Ogg_demuxer.frame_width
          buf.Ogg_demuxer.frame_height buf.Ogg_demuxer.y
          buf.Ogg_demuxer.y_stride buf.Ogg_demuxer.u buf.Ogg_demuxer.v
          buf.Ogg_demuxer.uv_stride
      in
      let img2 = Video.Image.create width height in
      scale img img2;
      img2 )
    else (
      let converter = converter buf.Ogg_demuxer.format in
      let yuv = Video.Image.create width height in
      let frame = Image.Generic.of_YUV420 yuv in
      let sframe =
        Image.YUV420.make buf.Ogg_demuxer.frame_width
          buf.Ogg_demuxer.frame_height buf.Ogg_demuxer.y
          buf.Ogg_demuxer.y_stride buf.Ogg_demuxer.u buf.Ogg_demuxer.v
          buf.Ogg_demuxer.uv_stride
      in
      converter (Image.Generic.of_YUV420 sframe) frame;
      yuv )

(** Stupid nearest neighbour resampling.
  * For meaningful results, one should first partially apply the freq params,
  * and re-use the resulting functions on consecutive chunks of a single
  * input stream. *)
let resample ~in_freq ~out_freq =
  (* We have something like this:
   *
   * i i i i i i i i i i i i i i i i i i i ...
   * o     o       o     o       o     o   ...
   *
   * (1) We ensure that out_len/out_freq = in_len/in_freq asymptotically.
   *     For doing so, we must keep track of the full input length,
   *     modulo in_freq.
   * (2) We do the simplest possible thing to choose which i becomes
   *     which o: nearest neighbour in the currently available buffer.
   *     This is not as good as nearest neighbour in the real stream.
   *
   * Turns out the same code codes for when out_freq>in_freq too. *)
  (* TODO: fractional sample rate! *)
  let in_pos = ref 0 in
  let in_freq = int_of_float in_freq in
  fun input off len ->
    let new_in_pos = !in_pos + len in
    let already_out_len = !in_pos * out_freq / in_freq in
    let needed_out_len = new_in_pos * out_freq / in_freq in
    let out_len = needed_out_len - already_out_len in
    in_pos := new_in_pos mod in_freq;
    Array.init out_len (fun i -> input.(off + (i * in_freq / out_freq)))

let video_resample () =
  let in_out = ref None in
  let resampler = ref None in
  fun ~in_freq ~out_freq buf off len ->
    if !in_out = Some (in_freq, out_freq) then
      (Utils.get_some !resampler) buf off len
    else (
      in_out := Some (in_freq, out_freq);
      resampler := Some (resample ~in_freq ~out_freq);
      (Utils.get_some !resampler) buf off len )

let demuxer_log x = log#debug "%s" x

module Make (Generator : Generator.S_Asio) = struct
  (* Note: the following code is still
   * written as if there was a case where 
   * we decode only the first logical track.
   * In fact, both file and stream decoding 
   * do decode all tracks. However, this may 
   * change in the future so the possibility
   * is left. *)

  let create_decoder ?(merge_tracks = false) source mode input =
    let decoder =
      let callbacks =
        {
          Ogg_demuxer.read = input.Decoder.read;
          seek = input.Decoder.lseek;
          tell = input.Decoder.tell;
        }
      in
      Ogg_demuxer.init ~log:demuxer_log callbacks
    in
    let audio_resample = Rutils.create_audio () in
    let video_resample = video_resample () in
    let video_scale = Video_converter.scaler () ~proportional:true in
    let decode_audio = mode = `Both || mode = `Audio in
    let decode_video = mode = `Both || mode = `Video in
    let started = ref false in
    let tracks = Ogg_demuxer.get_standard_tracks decoder in
    let first_meta = ref true in
    let init ~reset buffer =
      if reset then (
        Ogg_demuxer.reset decoder;
        Ogg_demuxer.update_standard_tracks decoder tracks;
        (* We enforce that all contents end together, otherwise there will
         * be a lag between different content types in the next track. *)
        if not merge_tracks then Generator.add_break ~sync:`Drop buffer );
      Generator.set_mode buffer mode;
      let add_meta f t =
        (* Initial metadata in files is handled separately. *)
        if source = `Stream || (merge_tracks && not !first_meta) then (
          let _, (v, m) = f decoder t in
          let metas = Hashtbl.create 10 in
          List.iter
            (fun (x, y) -> Hashtbl.add metas (String.lowercase_ascii x) y)
            m;
          Hashtbl.add metas "vendor" v;
          Generator.add_metadata buffer metas );
        first_meta := false
      in
      let drop_track d t =
        match t with None -> () | Some t -> Ogg_demuxer.drop_track d t
      in
      (* Make sure the stream has what we need *)
      (* TODO this should be done based on the kind, not the mode,
       *      which should be (re)set accordingly *)
      match
        (tracks.Ogg_demuxer.audio_track, tracks.Ogg_demuxer.video_track, mode)
      with
        | Some audio, Some video, `Both ->
            add_meta Ogg_demuxer.audio_info audio;
            add_meta Ogg_demuxer.video_info video
        | Some audio, video, `Audio ->
            drop_track decoder video;
            add_meta Ogg_demuxer.audio_info audio
        | audio, Some video, `Video ->
            drop_track decoder audio;
            add_meta Ogg_demuxer.video_info video
        | _ -> failwith "Ogg stream does not contain required data"
    in
    let decode buffer =
      try
        if not !started then (
          init ~reset:false buffer;
          started := true );
        if Ogg_demuxer.eos decoder then
          if merge_tracks || source = `Stream then init ~reset:true buffer
          else raise Ogg_demuxer.End_of_stream;
        let audio_feed track buf =
          let info, _ = Ogg_demuxer.audio_info decoder track in
          let content =
            audio_resample
              ~audio_src_rate:(float info.Ogg_demuxer.sample_rate)
              buf
          in
          Generator.put_audio buffer content 0 (Audio.length content)
        in
        let video_feed track buf =
          let info, _ = Ogg_demuxer.video_info decoder track in
          let out_freq = Lazy.force Frame.video_rate in
          let rgb = video_convert video_scale buf in
          let in_freq =
            float info.Ogg_demuxer.fps_numerator
            /. float info.Ogg_demuxer.fps_denominator
          in
          let stream =
            video_resample ~in_freq ~out_freq (Video.single rgb) 0 1
          in
          Generator.put_video buffer [| stream |] 0 (Video.length stream)
        in
        let decode_audio, decode_video =
          if decode_audio && decode_video then
            (* Only decode the one which is late, so that we don't have memory
             problems. *)
            if Generator.audio_length buffer < Generator.video_length buffer
            then (true, false)
            else (false, true)
          else (decode_audio, decode_video)
        in
        if decode_audio then (
          let track = Utils.get_some tracks.Ogg_demuxer.audio_track in
          Ogg_demuxer.decode_audio decoder track (fun buf ->
              audio_feed track (Audio.of_array buf)) );
        if decode_video then (
          let track = Utils.get_some tracks.Ogg_demuxer.video_track in
          Ogg_demuxer.decode_video decoder track (video_feed track) )
      with
        (* We catch [Ogg_demuxer.End_of_stream] only if asked to
         * to merge logical tracks or with a stream source. 
         * In this case, we try to reset the decoder to see if 
         * there could be another sequentialized logical stream
         * starting. Actual reset is handled in the
         * decoding function since we need the actual
         * buffer to add metadata etc. *)
        | Ogg_demuxer.End_of_stream when merge_tracks || source = `Stream ->
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
        let time_offset = Frame.seconds_of_master offset in
        let new_time = Ogg_demuxer.seek ~relative:true decoder time_offset in
        Frame.master_of_seconds new_time
      with Ogg_demuxer.End_of_stream | Ogg.End_of_stream ->
        log#info "End of track reached while seeking!";
        0
    in
    { Decoder.decode; seek }
end

(** File decoder *)

module G = Generator.From_audio_video
module Buffered = Decoder.Buffered (G)
module D = Make (G)

let create_file_decoder filename content_type kind =
  let mode =
    match (content_type.Frame.video, content_type.Frame.audio) with
      | 0, _ -> `Audio
      | _, 0 -> `Video
      | _, _ -> `Both
  in
  let generator = G.create mode in
  Buffered.file_decoder filename kind
    (D.create_decoder ~merge_tracks:true `File mode)
    generator

let get_type filename =
  let decoder, fd = Ogg_demuxer.init_from_file ~log:demuxer_log filename in
  let tracks = Ogg_demuxer.get_standard_tracks decoder in
  Tutils.finalize
    ~k:(fun () -> Unix.close fd)
    (fun () ->
      let audio =
        match tracks.Ogg_demuxer.audio_track with
          | None -> 0
          | Some t ->
              let info, _ = Ogg_demuxer.audio_info decoder t in
              info.Ogg_demuxer.channels
      in
      let video = if tracks.Ogg_demuxer.video_track <> None then 1 else 0 in
      log#info "File %S recognized as audio=%d video=%d." filename audio video;
      { Frame.audio; video; midi = 0 })

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

let () =
  Decoder.file_decoders#register "OGG"
    ~sdoc:"Decode a file as OGG provided that libogg accepts it."
    (fun ~metadata:_ filename kind ->
      (* First, test file extension and mime *)
      if
        Decoder.test_file ~mimes:mime_types#get ~extensions:file_extensions#get
          ~log filename
      then (
        let content_type = get_type filename in
        let content_type =
          (* If the kind doesn't allow audio, or video,
           * pretend that we don't have any: it will be dropped
           * anyway.
           * A more fine-grained approach might or might not
           * be possible, based on the number of channels. *)
          if kind.Frame.video = Frame.Zero then
            { content_type with Frame.video = 0 }
          else if kind.Frame.audio = Frame.Zero then
            { content_type with Frame.audio = 0 }
          else content_type
        in
        if Frame.type_has_kind content_type kind then
          Some (fun () -> create_file_decoder filename content_type kind)
        else None )
      else None)

(** Stream decoder *)

module D_stream = Make (Generator.From_audio_video_plus)

let () =
  Decoder.stream_decoders#register "OGG"
    ~sdoc:"Decode as OGG any stream with an appropriate MIME type."
    (fun mime kind ->
      if List.mem mime mime_types#get then (
        let mode =
          let content_type = Frame.type_of_kind kind in
          match (content_type.Frame.video, content_type.Frame.audio) with
            | 0, _ -> `Audio
            | _, 0 -> `Video
            | _, _ -> `Both
        in
        Some (D_stream.create_decoder `Stream mode) )
      else None)

(** Metadata *)

let get_tags file =
  if
    not
      (Decoder.test_file ~mimes:mime_types#get ~extensions:file_extensions#get
         ~log file)
  then raise Not_found;
  let decoder, fd = Ogg_demuxer.init_from_file ~log:demuxer_log file in
  let tracks = Ogg_demuxer.get_standard_tracks decoder in
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
      get Ogg_demuxer.audio_info tracks.Ogg_demuxer.audio_track
      @ get Ogg_demuxer.video_info tracks.Ogg_demuxer.video_track)

let () = Request.mresolvers#register "OGG" get_tags
