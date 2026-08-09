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

(** What a container stream decodes into. Each constructor pairs the ffmpeg
    stream handed to [Av.read_input] with the decoder that consumes what that
    stream yields, so the two can never drift apart. *)
type decoder =
  | Audio_frame of
      (Avutil.input, Avutil.audio, [ `Frame ]) Av.stream
      * (buffer:Decoder.buffer ->
        [ `Frame of Avutil.audio Avutil.frame | `Flush ] ->
        unit)
  | Audio_packet of
      (Avutil.input, Avutil.audio, [ `Packet ]) Av.stream
      * (buffer:Decoder.buffer ->
        [ `Packet of Avutil.audio Avcodec.Packet.t | `Flush ] ->
        unit)
  | Video_frame of
      (Avutil.input, Avutil.video, [ `Frame ]) Av.stream
      * (buffer:Decoder.buffer ->
        [ `Frame of Avutil.video Avutil.frame | `Flush ] ->
        unit)
  | Video_packet of
      (Avutil.input, Avutil.video, [ `Packet ]) Av.stream
      * (buffer:Decoder.buffer ->
        [ `Packet of Avutil.video Avcodec.Packet.t | `Flush ] ->
        unit)
  | Subtitle_frame of
      (Avutil.input, Avutil.subtitle, [ `Frame ]) Av.stream
      * (buffer:Decoder.buffer ->
        [ `Subtitle of Avutil.Subtitle.frame | `Flush ] ->
        unit)
  | Subtitle_packet of
      (Avutil.input, Avutil.subtitle, [ `Packet ]) Av.stream
      * (buffer:Decoder.buffer ->
        [ `Subtitle of Avutil.subtitle Avcodec.Packet.t | `Flush ] ->
        unit)
  | Data_packet of
      (Avutil.input, [ `Data ], [ `Packet ]) Av.stream
      * (buffer:Decoder.buffer -> [ `Data ] Avcodec.Packet.t -> unit)

type stream = {
  index : int;
  time_base : Avutil.rational;
  sparse : [ `False | `True of buffer:Decoder.buffer -> int -> unit ];
  decoder : decoder;
  mutable seen : bool;
  (* All positions are in main ticks *)
  mutable first_position : int option;
  mutable pts : int option;
  mutable position : int option;
}

(* Timed id3 streams carry no media, so there is nothing to flush. *)
let flush_decoder ~buffer = function
  | Audio_frame (_, decode) -> decode ~buffer `Flush
  | Audio_packet (_, decode) -> decode ~buffer `Flush
  | Video_frame (_, decode) -> decode ~buffer `Flush
  | Video_packet (_, decode) -> decode ~buffer `Flush
  | Subtitle_frame (_, decode) -> decode ~buffer `Flush
  | Subtitle_packet (_, decode) -> decode ~buffer `Flush
  | Data_packet _ -> ()

(** Stream index, then the timestamp used for position bookkeeping and the one
    used to detect a seek. Packets are ordered by dts, frames by pts. *)
let stream_timestamps = function
  | `Audio_frame (i, f) -> (i, Avutil.Frame.pts f, Avutil.Frame.pts f)
  | `Video_frame (i, f) -> (i, Avutil.Frame.pts f, Avutil.Frame.pts f)
  | `Subtitle_frame (i, f) ->
      (i, Avutil.Subtitle.get_pts f, Avutil.Subtitle.get_pts f)
  | `Audio_packet (i, p) ->
      (i, Avcodec.Packet.get_dts p, Avcodec.Packet.get_pts p)
  | `Video_packet (i, p) ->
      (i, Avcodec.Packet.get_dts p, Avcodec.Packet.get_pts p)
  | `Subtitle_packet (i, p) ->
      (i, Avcodec.Packet.get_dts p, Avcodec.Packet.get_pts p)
  | `Data_packet (i, p) ->
      (i, Avcodec.Packet.get_dts p, Avcodec.Packet.get_pts p)

(* The last case is unreachable: a stream index and its decoder are assigned
   together in [mk_streams], so they always agree. *)
let decode_data ~buffer data decoder =
  match (data, decoder) with
    | `Audio_frame (_, f), Audio_frame (_, decode) -> decode ~buffer (`Frame f)
    | `Audio_packet (_, p), Audio_packet (_, decode) ->
        decode ~buffer (`Packet p)
    | `Video_frame (_, f), Video_frame (_, decode) -> decode ~buffer (`Frame f)
    | `Video_packet (_, p), Video_packet (_, decode) ->
        decode ~buffer (`Packet p)
    | `Subtitle_frame (_, f), Subtitle_frame (_, decode) ->
        decode ~buffer (`Subtitle f)
    | `Subtitle_packet (_, p), Subtitle_packet (_, decode) ->
        decode ~buffer (`Subtitle p)
    | `Data_packet (_, p), Data_packet (_, decode) -> decode ~buffer p
    | _ -> ()

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

let parse_encoder_params =
  let processor =
    MenhirLib.Convert.Simplified.traditional2revised Parser.plain_encoder_params
  in
  fun s ->
    let lexbuf = Sedlexing.Utf8.from_string ("(" ^ s ^ ")") in
    let tokenizer = Preprocessor.mk_tokenizer lexbuf in
    Term_reducer.to_encoder_params
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

let get_duration container =
  let duration = Av.get_input_duration container ~format:`Millisecond in
  Option.map (fun d -> Int64.to_float d /. 1000.) duration

let opts_of_args args =
  let opts = Hashtbl.create 10 in
  List.iter (fun (k, v) -> Hashtbl.replace opts k v) args;
  opts

(* Opening a container removes the options ffmpeg consumed, so whatever is left
   was not understood by the demuxer. *)
let check_opts opts =
  if Hashtbl.length opts > 0 then
    Runtime_error.raise ~pos:[]
      ~message:
        (Printf.sprintf "Unrecognized options: %s"
           (Ffmpeg_format.string_of_options opts))
      "ffmpeg_decoder"

let open_decoding_input ?format ~opts source =
  Av.open_input ?format ~opts
    ~configure_audio_stream:Ffmpeg_decoder_common.configure_audio_stream
    ~configure_video_stream:Ffmpeg_decoder_common.configure_video_stream
    ~configure_subtitle_stream:Ffmpeg_decoder_common.configure_subtitle_stream
    source

let dresolver ~metadata file =
  let args, format = parse_file_decoder_args metadata in
  let opts = opts_of_args args in
  let container = Av.open_input ?format ~opts file in
  Fun.protect
    ~finally:(fun () -> Av.close container)
    (fun () -> get_duration container)

let () =
  Plug.register Request.dresolvers "ffmpeg" ~doc:""
    {
      dpriority = (fun () -> Ffmpeg_decoder_conf.priority#get);
      file_extensions = (fun () -> Ffmpeg_decoder_conf.file_extensions#get);
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
        (Decoder.test_file ~log ~extension ~mime
           ~mimes:(Some Ffmpeg_decoder_conf.mime_types#get)
           ~extensions:(Some Ffmpeg_decoder_conf.file_extensions#get) file)
    then raise Invalid_file;
    let args, format = parse_file_decoder_args metadata in
    let opts = opts_of_args args in
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

let get_type ?format ~ctype ~url container =
  let c = Ffmpeg_stream_description.container ?format ~url container in
  let description = Ffmpeg_stream_description.describe c in
  let uri = Lang_string.quote_string url in
  log#important "FFmpeg recognizes %s as %s" uri description;
  Ffmpeg_stream_description.get_type ~ctype c

(* Codecs carrying state across packets, mp3 and its bit reservoir in
   particular, need their preceding packets before they produce sound again:
   decoding straight from the seek point yields up to ~150ms of silence. We
   land this much ahead of the target instead and decode our way to it,
   discarding what comes out. *)
let seek_preroll = 0.5

let seek ~state ~target_position ~container ticks =
  Mutex_utils.mutable_lock ~state
    (fun () ->
      let tpos = Frame.seconds_of_main ticks in
      log#important "Setting target position to %f" tpos;
      Atomic.set target_position (Some ticks);
      let ts = Int64.of_float (max 0. (tpos -. seek_preroll) *. 1000.) in
      Av.seek ~fmt:`Millisecond ~min_ts:Int64.min_int ~max_ts:ts ~ts container)
    ()

let mk_eof streams buffer =
  Streams.iter (fun _ s -> flush_decoder ~buffer s.decoder) streams;
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
            match Atomic.get target_position with
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
  (* The pre-roll is decoded into a buffer of its own. Decoding it into the real
     generator and truncating it away would not do: truncating shrinks the
     metadata and track mark contents along with the media ones, and nothing
     grows those back, which caps every later slice at zero. *)
  let discarded = ref None in
  let discard_buffer generator =
    match !discarded with
      | Some (buffer, generator) ->
          Generator.clear generator;
          buffer
      | None ->
          let ctype = Generator.content_type generator in
          let generator = Generator.create ~log ctype in
          let buffer = Decoder.mk_buffer ~ctype generator in
          discarded := Some (buffer, generator);
          buffer
  in
  fun ~buffer ~decode ~ts ~stream pts ->
    update_position ~buffer ~pts ~streams stream;
    match (stream.pts, Atomic.get target_position) with
      | Some pts, Some target when pts < target ->
          (* Feed the decoder its pre-roll and throw the result away: codecs
             carrying state across packets, mp3 and its bit reservoir in
             particular, stay silent until they have caught up. *)
          decode (discard_buffer buffer.Decoder.generator)
      | Some pts, _ ->
          if not stream.seen then stream.seen <- true;
          let all_seen =
            Streams.for_all (fun _ s -> s.sparse <> `False || s.seen) streams
          in
          if all_seen then (
            flush pts;
            decode buffer)
          else push (pts, ts, fun () -> decode buffer)
      | None, _ ->
          log#important
            "Got packet or frame with no timestamp! Synchronization issues may \
             happen.";
          decode buffer

let mk_decoder ~streams ~target_position ~state container =
  let check_position = mk_check_position ~streams ~target_position () in
  let ( audio_frame,
        audio_packet,
        video_frame,
        video_packet,
        subtitle_frame,
        subtitle_packet,
        data_packet ) =
    Streams.fold
      (fun _ s (af, ap, vf, vp, sf, sp, dp) ->
        match s.decoder with
          | Audio_frame (s, _) -> (s :: af, ap, vf, vp, sf, sp, dp)
          | Audio_packet (s, _) -> (af, s :: ap, vf, vp, sf, sp, dp)
          | Video_frame (s, _) -> (af, ap, s :: vf, vp, sf, sp, dp)
          | Video_packet (s, _) -> (af, ap, vf, s :: vp, sf, sp, dp)
          | Subtitle_frame (s, _) -> (af, ap, vf, vp, s :: sf, sp, dp)
          | Subtitle_packet (s, _) -> (af, ap, vf, vp, sf, s :: sp, dp)
          | Data_packet (s, _) -> (af, ap, vf, vp, sf, sp, s :: dp))
      streams
      ([], [], [], [], [], [], [])
  in
  let rec decode buffer =
    try
      let data =
        Av.read_input ~audio_frame ~audio_packet ~video_frame ~video_packet
          ~data_packet ~subtitle_packet ~subtitle_frame container
      in
      let index, ts, pts = stream_timestamps data in
      match Streams.find_opt index streams with
        | None -> decode buffer
        | Some stream ->
            check_position ~buffer
              ~ts:(Option.value ~default:0L ts)
              ~decode:(fun buffer -> decode_data ~buffer data stream.decoder)
              ~stream pts
    with
      | Avutil.Error `Eagain | Avutil.Error `Invalid_data -> decode buffer
      | Avutil.Error `Exit | Avutil.Error `Eof -> raise End_of_file
      | exn ->
          let bt = Printexc.get_raw_backtrace () in
          Printexc.raise_with_backtrace exn bt
  in
  let last_meta = ref None in
  fun buffer ->
    Mutex_utils.atomic_lock ~state
      (fun () ->
        let m = Av.get_input_metadata container in
        if Some m <> !last_meta && m <> [] then (
          last_meta := Some m;
          Generator.add_metadata buffer.Decoder.generator
            (Frame.Metadata.from_list m));
        decode buffer)
      ()

let mk_streams ~ctype ~decode_first_metadata ~set_remaining container =
  let track_packet ~stream = function
    | `Packet packet ->
        set_remaining
          ~pts:(Avcodec.Packet.get_pts packet)
          ~duration:(Avcodec.Packet.get_duration packet)
          ~time_base:(Av.get_time_base stream)
    | `Flush -> ()
  in
  let track_frame ~stream = function
    | `Frame frame ->
        set_remaining ~pts:(Avutil.Frame.pts frame)
          ~duration:(Avutil.Frame.duration frame)
          ~time_base:(Av.get_time_base stream)
    | `Flush -> ()
  in
  let tracked_stream ~track_data stream fn =
    let is_first = ref true in
    let latest_metadata = ref None in
    fun ~buffer data ->
      let m = Av.get_metadata stream in
      (* FFmpeg has memory leaks with chained ogg stream so we manually
           reset the metadata after fetching it. *)
      Av.set_metadata stream [];
      if
        ((not !is_first) || decode_first_metadata)
        && Some m <> !latest_metadata && m <> []
      then (
        is_first := false;
        latest_metadata := Some m;
        Generator.add_metadata buffer.Decoder.generator
          (Frame.Metadata.from_list m));
      track_data ~stream data;
      fn ~buffer data
  in
  let stream_idx = Ffmpeg_content_base.new_stream_idx () in
  (* [pos] numbers the fields in container stream order and advances whether or
     not the stream ends up selected, so which field a stream maps to does not
     depend on what [ctype] asks for. *)
  let fold_media mk container_streams streams =
    fst
      (List.fold_left
         (fun (streams, pos) entry -> (mk ~pos streams entry, pos + 1))
         (streams, 0) container_streams)
  in
  (* Each media type is walked twice, once per mode: `Av.stream` records in its
     type whether it will be read as packets or as frames, so a single pass
     cannot hold both a copy decoder and a decoding one. `Av.get_*_streams` is
     polymorphic in that mode, so calling it again gives the same streams at the
     other type. *)
  let is_pcm format =
    Content.Audio.is_format format
    || Content_pcm_s16.is_format format
    || Content_pcm_f32.is_format format
  in
  let pcm_channels format =
    if Content.Audio.is_format format then
      Content.Audio.channels_of_format format
    else if Content_pcm_s16.is_format format then
      Content_pcm_s16.channels_of_format format
    else Content_pcm_f32.channels_of_format format
  in
  let streams =
    fold_media
      (fun ~pos streams (idx, stream, params) ->
        let field = Frame.Fields.audio_n pos in
        match Frame.Fields.find_opt field ctype with
          | Some format when Ffmpeg_copy_content.is_format format ->
              add_stream ~sparse:`False idx stream
                (Audio_packet
                   ( stream,
                     tracked_stream ~track_data:track_packet stream
                       (Ffmpeg_copy_decoder.mk_audio_decoder ~stream_idx ~format
                          ~field ~stream params) ))
                streams
          | _ -> streams)
      (Av.get_audio_streams container)
      Streams.empty
  in
  let streams =
    fold_media
      (fun ~pos streams (idx, stream, params) ->
        let field = Frame.Fields.audio_n pos in
        let add decoder =
          add_stream ~sparse:`False idx stream decoder streams
        in
        let decode fn =
          add
            (Audio_frame
               (stream, tracked_stream ~track_data:track_frame stream fn))
        in
        match Frame.Fields.find_opt field ctype with
          | Some format when Ffmpeg_raw_content.Audio.is_format format ->
              decode
                (Ffmpeg_raw_decoder.mk_audio_decoder ~stream_idx ~format ~stream
                   ~field params)
          | Some format when is_pcm format ->
              decode
                (Ffmpeg_internal_decoder.mk_audio_decoder
                   ~channels:(pcm_channels format) ~field
                   ~pcm_kind:(Content.kind format) params)
          | _ -> streams)
      (Av.get_audio_streams container)
      streams
  in
  let streams =
    fold_media
      (fun ~pos streams (idx, stream, params) ->
        let field = Frame.Fields.video_n pos in
        match Frame.Fields.find_opt field ctype with
          | Some format when Ffmpeg_copy_content.is_format format ->
              add_stream ~sparse:`False idx stream
                (Video_packet
                   ( stream,
                     tracked_stream ~track_data:track_packet stream
                       (Ffmpeg_copy_decoder.mk_video_decoder ~stream_idx ~format
                          ~field ~stream params) ))
                streams
          | _ -> streams)
      (Av.get_video_streams container)
      streams
  in
  let streams =
    fold_media
      (fun ~pos streams (idx, stream, params) ->
        let field = Frame.Fields.video_n pos in
        let add decoder =
          add_stream ~sparse:`False idx stream decoder streams
        in
        let decode fn =
          add
            (Video_frame
               (stream, tracked_stream ~track_data:track_frame stream fn))
        in
        match Frame.Fields.find_opt field ctype with
          | Some format when Ffmpeg_raw_content.Video.is_format format ->
              decode
                (Ffmpeg_raw_decoder.mk_video_decoder ~stream_idx ~format ~stream
                   ~field params)
          | Some format when Content.Video.is_format format ->
              (* Offered as ideal size; the negotiated ones are read back below. *)
              let ideal_size =
                Frame.
                  {
                    width = Avcodec.Video.get_width params;
                    height = Avcodec.Video.get_height params;
                    source = "ffmpeg decoder";
                  }
              in
              ignore (Frame.video_dimensions ~ideal_size ());
              let width, height = Content.Video.dimensions_of_format format in
              Ffmpeg_utils.set_format_alpha ~codec_params:params format;
              let alpha = Content.Video.alpha_of_format format in
              decode
                (Ffmpeg_internal_decoder.mk_video_decoder ~width ~height ~alpha
                   ~stream ~field params)
          | _ -> streams)
      (Av.get_video_streams container)
      streams
  in
  let streams =
    fold_media
      (fun ~pos streams (idx, stream, params) ->
        let field = Frame.Fields.subtitles_n pos in
        match Frame.Fields.find_opt field ctype with
          | Some format when Ffmpeg_copy_content.is_format format ->
              let { Ffmpeg_decoder_common.decoder; advance } =
                Ffmpeg_copy_decoder.mk_subtitle_decoder ~stream_idx ~format
                  ~field ~stream params
              in
              add_stream ~sparse:(`True advance) idx stream
                (Subtitle_packet (stream, decoder))
                streams
          | _ -> streams)
      (Av.get_subtitle_streams container)
      streams
  in
  let streams =
    fold_media
      (fun ~pos streams (idx, stream, _) ->
        let field = Frame.Fields.subtitles_n pos in
        let add { Ffmpeg_decoder_common.decoder; advance } =
          add_stream ~sparse:(`True advance) idx stream
            (Subtitle_frame (stream, decoder))
            streams
        in
        match Frame.Fields.find_opt field ctype with
          | Some format when Subtitle_content.is_format format ->
              add (Ffmpeg_internal_decoder.mk_text_subtitle_decoder ~field)
          | Some format when Content.Video.is_format format ->
              let width, height = Content.Video.dimensions_of_format format in
              add
                (Ffmpeg_internal_decoder.mk_bitmap_subtitle_decoder ~field
                   ~width ~height)
          | _ -> streams)
      (Av.get_subtitle_streams container)
      streams
  in
  (* Timed id3 is the only data stream we know how to use: it carries metadata,
     no media, so it is sparse with nothing to advance. *)
  List.fold_left
    (fun streams (idx, stream, params) ->
      try
        if Avcodec.Unknown.get_params_id params <> `Timed_id3 then streams
        else
          add_stream
            ~sparse:(`True (fun ~buffer:_ _ -> ()))
            idx stream
            (Data_packet
               ( stream,
                 fun ~buffer p ->
                   let metadata =
                     try parse_timed_id3 (Avcodec.Packet.content p)
                     with _ -> []
                   in
                   if metadata <> [] then
                     Generator.add_metadata buffer.Decoder.generator
                       (Frame.Metadata.from_list metadata) ))
            streams
      with Avutil.Error _ as exn ->
        let bt = Printexc.get_raw_backtrace () in
        Utils.log_exception ~log
          ~bt:(Printexc.raw_backtrace_to_string bt)
          (Printf.sprintf "Failed to get stream info: %s"
             (Printexc.to_string exn));
        streams)
    streams
    (Av.get_data_streams container)

let mk_decoder_record ~ctype ~decode_first_metadata container =
  let container_duration = try get_duration container with _ -> None in
  let remaining = Atomic.make container_duration in
  let set_remaining ~pts ~duration ~time_base =
    let pts =
      Option.map
        (fun pts -> Int64.add pts (Option.value ~default:0L duration))
        pts
    in
    match (container_duration, pts) with
      | None, _ | Some _, None -> ()
      | Some d, Some pts -> (
          let { Avutil.num; den } = time_base in
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
  let position () =
    Option.map
      (fun d -> Frame.main_of_seconds d - get_remaining ())
      container_duration
  in
  let target_position = Atomic.make None in
  let state = Mutex_utils.mk_state () in
  let prepare_decoder ~decode_first_metadata () =
    let streams =
      mk_streams ~ctype ~decode_first_metadata ~set_remaining container
    in
    let decoder = mk_decoder ~state ~streams ~target_position container in
    let eof = mk_eof streams in
    (streams, decoder, eof)
  in
  let decoder_ref = Atomic.make (prepare_decoder ~decode_first_metadata ()) in
  let stream_position () =
    let streams, _, _ = Atomic.get decoder_ref in
    List.find_map
      (fun (_, s) -> if s.sparse = `False && s.seen then s.position else None)
      (Streams.bindings streams)
  in
  let seek pos =
    match (position (), stream_position ()) with
      | Some current_position, _ | None, Some current_position ->
          seek ~state ~target_position ~container (current_position + pos);
          Atomic.set decoder_ref (prepare_decoder ~decode_first_metadata ());
          Atomic.set remaining None;
          pos
      | _ -> 0
  in
  let decode buffer =
    let _, decoder, _ = Atomic.get decoder_ref in
    decoder buffer
  in
  let eof buffer =
    let _, _, eof = Atomic.get decoder_ref in
    eof buffer
  in
  let close () = Av.close container in
  ({ Decoder.seek; decode; eof; close }, get_remaining)

let create_decoder ~ctype ~metadata fname =
  let args, format = parse_file_decoder_args metadata in
  let opts = opts_of_args args in
  let ext = Filename.extension fname in
  if
    List.exists
      (fun s -> ext = "." ^ s)
      Ffmpeg_decoder_conf.image_file_extensions#get
  then (
    Hashtbl.replace opts "loop" (`Int 1);
    Hashtbl.replace opts "framerate" (`Int (Lazy.force Frame.video_rate)));
  let container = open_decoding_input ?format ~opts fname in
  check_opts opts;
  mk_decoder_record ~ctype ~decode_first_metadata:false container

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
  let opts = opts_of_args args in
  if List.exists (fun s -> mime = s) Ffmpeg_decoder_conf.image_mime_types#get
  then (
    Hashtbl.replace opts "loop" (`Int 1);
    Hashtbl.replace opts "framerate" (`Int (Lazy.force Frame.video_rate)));
  let container =
    Av.open_input_stream ?seek:seek_input ~opts ?format input.Decoder.read
  in
  check_opts opts;
  fst (mk_decoder_record ~ctype ~decode_first_metadata:true container)

let get_file_type ~metadata ~ctype filename =
  (* If file is an image, leave internal decoding to
     the image decoder. *)
    match
      ( Utils.get_ext_opt filename,
        Frame.Fields.find_opt Frame.Fields.video ctype )
    with
    | Some ext, Some format
      when List.mem ext Ffmpeg_decoder_conf.image_file_extensions#get
           && Content.Video.is_format format ->
        Frame.Fields.make ()
    | _ ->
        let args, format = parse_file_decoder_args metadata in
        let opts = opts_of_args args in
        let container = open_decoding_input ?format ~opts filename in
        Fun.protect
          ~finally:(fun () -> Av.close container)
          (fun () -> get_type ?format ~ctype ~url:filename container)

let () =
  Plug.register Decoder.decoders "ffmpeg"
    ~doc:
      "Use FFmpeg to decode any file or stream if its MIME type or file \
       extension is appropriate."
    {
      Decoder.priority = (fun () -> Ffmpeg_decoder_conf.priority#get);
      file_extensions =
        (fun () ->
          Some
            (Ffmpeg_decoder_conf.file_extensions#get
           @ Ffmpeg_decoder_conf.image_file_extensions#get));
      mime_types =
        (fun () ->
          Some
            (Ffmpeg_decoder_conf.mime_types#get
           @ Ffmpeg_decoder_conf.image_mime_types#get));
      file_type =
        (fun ~metadata ~ctype filename ->
          Some (get_file_type ~metadata ~ctype filename));
      file_decoder = Some create_file_decoder;
      stream_decoder = Some create_stream_decoder;
    }
