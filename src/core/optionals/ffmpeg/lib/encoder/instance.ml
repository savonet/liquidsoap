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

module F = Liquidsoap_ffmpeg_formats.Encoder_format

let error ?pos message = Lang_encoder.raise_error ~pos message

type track =
  [ `Copy of Copy_encoder.track | `Encoded of Encoded_stream.t | `Drop ]

type container = {
  output : Avutil.output Avutil.container;
  muxer : string;
  copy : Copy_encoder.t;
  tracks : (string * track) list;
  mutable streams_created : bool;
  mutable wait_start : int option;
}

type state = [ `Built | `Started | `Failed of exn | `Stopped ]

(* HLS mode (encoder.md §8). *)
type hls = {
  split : Hls_split.t;
  order : [ `Track of string | `Id3 ] Hls_order.t;
  mutable id3 : Encoder.hls_id3_mode;
  mutable id3_version : int;
  mutable id3_stream : (Avutil.output, [ `Data ], [ `Packet ]) Av.stream option;
  mutable id3_queue : (int * (unit -> unit)) list;
  mutable media_written : bool;
}

(* [header] is the current container's header (encoder.md §10): [`Pending
   start] until its first packet, [start] being where its bytes begin in
   [buffer]. [link_start] is where the last restart's container begins in
   [buffer], and [returned_link_start] the same offset in the bytes last
   returned. *)
type t = {
  log : Log.t;
  format : F.t;
  pos : Pos.t option;
  interrupt : (unit -> bool) option;
  buffer : Buffer.t;
  mutable metadata : (string * string) list;
  mutable current : container;
  mutable state : state;
  mutable position : int;
  mutable header : [ `Pending of int | `Captured of Strings.t ];
  mutable link_start : int option;
  mutable returned_link_start : int option;
  mutable access_points : int list;
  mutable returned_access_points : int list;
  direct : Encoder.sink option;
  filename : string option;
  hls : hls option;
}

let convert_container_opts pos opts =
  List.map
    (fun (key, value) ->
      match (key, value) with
        | "sample_fmt", `String name -> (
            match Avutil.Sample_format.find name with
              | f -> (key, `Int (Avutil.Sample_format.get_id f))
              | exception _ ->
                  error ?pos (Printf.sprintf "Unknown sample format: %s" name))
        | "channel_layout", `String name -> (
            match Avutil.Channel_layout.find name with
              | l -> (key, `String (Avutil.Channel_layout.get_description l))
              | exception _ ->
                  error ?pos (Printf.sprintf "Unknown channel layout: %s" name))
        | ("sample_fmt" | "channel_layout"), _ ->
            error ?pos (Printf.sprintf "Invalid value for %s" key)
        | _ -> (key, value))
    opts

let non_drop format = List.filter (fun (_, t) -> t <> `Drop) format.F.streams
let close_output c = Av.close c.output

(* Muxers that tell the I/O what they write: a reader can start at the units
   they mark as sync points, and the I/O is flushed around them. *)
let marking_muxers =
  ["matroska"; "webm"; "flv"; "asf"; "asf_stream"; "mxf"; "mpeg"; "vob"; "dvd"]
  @ F.iso_family

let open_container ?interrupt
    ?(on_packet = fun _ ~key:_ ~time:_ ~write -> write ())
    ?(on_sync_point = fun () -> ()) ?direct ?filename ~log ~(format : F.t) ~pos
    ~buffer () =
  let opts = Opts.of_list (convert_container_opts pos format.opts) in
  let interleaved =
    match format.interleaved with
      | `True -> true
      | `False -> false
      | `Default -> List.length (non_drop format) > 1
  in
  (* A named muxer is that muxer or none: a guess from the file name or the
     MIME type would write another format than the script asked for. *)
  let guess () =
    match format.format with
      | Some short_name -> (
          match Av.Format.guess_output_format ~short_name () with
            | Some muxer when Av.Format.get_output_name muxer = short_name ->
                Some muxer
            | _ -> None)
      | None ->
          Av.Format.guess_output_format
            ?filename:
              (match format.output with
                | `Url u -> Some u
                | `Stream -> filename)
            ?mime:format.mime_type ()
  in
  let output =
    match format.output with
      | `Stream -> (
          match (format.format, guess ()) with
            | None, None -> error ?pos "Format is required!"
            | Some f, None ->
                error ?pos
                  (Printf.sprintf
                     "No ffmpeg format could be found for format=%S" f)
            | _, Some muxer when direct <> None ->
                let sink : Encoder.sink = Option.get direct in
                let sink_seek = Option.get sink.seek in
                let position = ref 0 in
                let write bytes offset length =
                  sink.write
                    (Strings.of_string (Bytes.sub_string bytes offset length));
                  position := !position + length;
                  length
                in
                let seek offset whence =
                  (position :=
                     match (whence, sink.size) with
                       | Unix.SEEK_SET, _ -> offset
                       | Unix.SEEK_CUR, _ -> !position + offset
                       | Unix.SEEK_END, Some size -> size () + offset
                       | Unix.SEEK_END, None ->
                           failwith "the destination has no known size");
                  sink_seek !position;
                  !position
                in
                (* What the muxer reads back when it moves data in its
                     output: up to [length] bytes, fewer at the end. *)
                let read_back =
                  Option.map
                    (fun read offset buffer length ->
                      let data = read offset length in
                      Bytes.blit_string data 0 buffer 0 (String.length data);
                      String.length data)
                    sink.read
                in
                Av.open_output_stream ~opts ~interleaved ~seek ?read_back write
                  muxer
            | _, Some muxer ->
                let write bytes offset length =
                  Buffer.add_subbytes buffer bytes offset length;
                  length
                in
                let marker kind _ =
                  match kind with
                    | Av.Sync_point -> on_sync_point ()
                    | Av.Header | Av.Boundary_point | Av.Unknown | Av.Trailer ->
                        ()
                in
                Av.open_output_stream ~opts ~interleaved ~marker write muxer)
      | `Url url ->
          Av.open_output ?interrupt ?format:(guess ()) ~interleaved ~opts url
  in
  (match Opts.unused_message [("container", opts)] with
    | Some message ->
        (try Av.close output with _ -> ());
        error ?pos message
    | None -> ());
  let muxer =
    match guess () with Some f -> Av.Format.get_output_name f | None -> ""
  in
  let release_and_raise exn =
    (try Av.close output with _ -> ());
    raise exn
  in
  try
    let copy =
      Copy_encoder.create ~log
        ~whole_output:
          (List.for_all
             (function _, `Copy _ -> true | _ -> false)
             (non_drop format))
        ~muxer output
    in
    let tracks =
      List.map
        (fun (field, track) ->
          match track with
            | `Drop -> (field, `Drop)
            | `Copy { F.keyframe; codec_tag } ->
                ( field,
                  `Copy
                    (Copy_encoder.track copy
                       ~field:(Frame.Fields.register field)
                       ~keyframe
                       ?codec_tag:(Option.map F.fourcc codec_tag)
                       ~on_packet:(on_packet field)) )
            | `Encode encoded ->
                ( field,
                  `Encoded
                    (Encoded_stream.create ~log ~pos
                       ~field:(Frame.Fields.register field)
                       ~on_packet:(on_packet field) encoded output) ))
        format.streams
    in
    { output; muxer; copy; tracks; streams_created = false; wait_start = None }
  with exn -> release_and_raise exn

let copy_tracks c =
  List.filter_map (function _, `Copy t -> Some t | _ -> None) c.tracks

let encoded_tracks c =
  List.filter_map (function _, `Encoded t -> Some t | _ -> None) c.tracks

let reset t =
  Buffer.clear t.buffer;
  t.returned_link_start <- t.link_start;
  t.link_start <- None;
  t.returned_access_points <- List.rev t.access_points;
  t.access_points <- [];
  match t.header with `Pending _ -> t.header <- `Pending 0 | `Captured _ -> ()

let take t =
  let s = Buffer.contents t.buffer in
  reset t;
  Strings.of_string s

(* The container header is written as soon as its streams exist and captured
   from the bytes since [`Pending start]: waiting for the first packet would
   capture nothing for a codec with a delay, whose first frame's bytes have
   already been taken by then. *)
let capture_header t =
  match (t.header, t.hls) with
    | `Pending _, Some _ ->
        Av.write_header t.current.output;
        t.header <- `Captured Strings.empty
    | `Pending start, None ->
        Av.write_header t.current.output;
        t.header <-
          `Captured
            (Strings.of_string
               (Buffer.sub t.buffer start (Buffer.length t.buffer - start)))
    | `Captured _, _ -> ()

let hls_flush t () =
  Av.flush t.current.output;
  if t.current.muxer = "mpegts" then
    Av.set_output_option t.current.output "mpegts_flags" "+resend_headers"

let encoded_video t =
  List.filter_map
    (function
      | field, `Encode { F.options = `Video _; _ } -> Some field | _ -> None)
    t.format.F.streams

(* The video streams: the encoded ones, and the copied ones by their name. *)
let video_fields t =
  List.filter_map
    (function
      | field, `Encode { F.options = `Video _; _ } -> Some field
      | field, `Copy _ when String.starts_with ~prefix:"video" field ->
          Some field
      | _ -> None)
    t.format.F.streams

(* The next byte written starts a unit a reader can start at. *)
let access_point t =
  let offset = Buffer.length t.buffer in
  if List.nth_opt t.access_points 0 <> Some offset then
    t.access_points <- offset :: t.access_points

(* A muxer that does not mark its output writes one packet after the other:
   with the I/O flushed between two packets, a reader can start at a key packet
   of the video, or at any packet of a stream without video. *)
let packet_access_point t field ~key =
  if
    t.hls = None
    && (not (List.mem t.current.muxer marking_muxers))
    && key
    && (video_fields t = [] || List.mem field (video_fields t))
  then (
    Av.flush_io t.current.output;
    access_point t)

(* Runs when a packet is due at the muxer: at once outside HLS, in media-time
   order in HLS mode (encoder.md §8.2). The bytes before the first packet are
   the header, written once the streams exist (capture_header). *)
let write_packet t field ~key ~time ~write =
  capture_header t;
  packet_access_point t field ~key;
  match (t.hls, time) with
    | Some h, Some time ->
        h.media_written <- true;
        Hls_split.packet h.split ~flush:(hls_flush t) ~field ~key ~time;
        write ()
    | Some h, None ->
        h.media_written <- true;
        write ()
    | None, _ -> write ()

let release_packets ?all h =
  Hls_order.release ?all
    ~gating:
      (List.map
         (fun field -> `Track field)
         (Hls_split.inter_coded_fields h.split))
    h.order

(* Before a container closes no packet may stay held: its write targets that
   container. A write that fails is logged and the others still go out. *)
let rec drain_held t h =
  match release_packets ~all:true h with
    | () -> ()
    | exception exn ->
        t.log#severe "Packet dropped at the end of the container: %s"
          (Printexc.to_string exn);
        drain_held t h

let on_packet t field ~key ~time ~write =
  match t.hls with
    | Some h ->
        Hls_order.push ~stream:(`Track field) ~time
          ~write:(fun () -> write_packet t field ~key ~time ~write)
          h.order;
        release_packets h
    | None -> write_packet t field ~key ~time ~write

let fail t exn =
  t.state <- `Failed exn;
  raise exn

let create_streams t frame =
  let c = t.current in
  if
    List.for_all
      (fun track -> Copy_encoder.first_params track frame <> None)
      (copy_tracks c)
  then (
    List.iter
      (function
        | _, `Copy track -> Copy_encoder.mk_stream track frame
        | _, `Encoded track -> track.Encoded_stream.mk_stream frame
        | _, `Drop -> ())
      c.tracks;
    c.streams_created <- true;
    Option.iter
      (fun h ->
        List.iter
          (function
            | field, `Encoded _ when List.mem field (encoded_video t) ->
                Hls_split.inter_coded h.split field
            | field, `Copy track
              when Copy_encoder.video_size track <> None
                   && not (Copy_encoder.intra_only track) ->
                Hls_split.inter_coded h.split field
            | _ -> ())
          c.tracks)
      t.hls;
    (match t.hls with
      | Some ({ id3 = `In_stream; _ } as h) ->
          h.id3_stream <-
            Some
              (Av.new_data_stream
                 ~time_base:
                   { Avutil.num = 1; den = Lazy.Mutexed.force Frame.audio_rate }
                 ~codec:`Timed_id3 c.output)
      | _ -> ());
    (* A header the muxer refuses is not an item fault: every later packet
       would be refused the same way. *)
    (try capture_header t with exn -> fail t exn);
    t.state <- `Started)
  else (
    let start = Option.value ~default:t.position c.wait_start in
    c.wait_start <- Some start;
    if t.position - start > Frame.main_of_seconds Settings.max_keyframe_wait#get
    then
      fail t
        (Failure
           "Copy fields received no parameters within the keyframe wait: the \
            output cannot start"))

let encode_frame t frame =
  let c = t.current in
  if not c.streams_created then create_streams t frame;
  let length = Frame.position frame in
  if c.streams_created && length > 0 then (
    try
      List.iter
        (fun track -> track.Encoded_stream.encode ~position:t.position frame)
        (encoded_tracks c);
      List.iter
        (fun track -> Copy_encoder.encode track ~position:t.position frame)
        (copy_tracks c);
      Copy_encoder.end_of_frame c.copy ~position:t.position
    with
      | Copy_encoder.Incompatible { field; reason; header; source } ->
          fail t
            (Clock.Unrecoverable
               (Printf.sprintf
                  "Track %s cannot copy the next source into this %s stream: \
                   %s. Every source of a copied track must share the \
                   parameters of the stream's first one, and this container \
                   cannot change them mid-stream. Encode the sources with \
                   identical settings. Stream: %s. New source: %s."
                  (Frame.Fields.string_of_field field)
                  (match c.muxer with "" -> "container" | muxer -> muxer)
                  reason header source))
      | exn -> fail t exn);
  if length > 0 then t.position <- t.position + length

let close_container t =
  let c = t.current in
  (try
     if c.streams_created then (
       List.iter (fun track -> track.Encoded_stream.flush ()) (encoded_tracks c);
       Copy_encoder.flush c.copy ~position:t.position)
   with exn -> t.log#severe "Draining failed: %s" (Printexc.to_string exn));
  Option.iter (drain_held t) t.hls;
  try close_output c
  with exn -> t.log#severe "Closing failed: %s" (Printexc.to_string exn)

(* encoder.md §13.1: Ogg chains, into a stream or by reopening the file. *)
let chains t =
  t.current.muxer = "ogg"
  &&
    match t.format.output with
    | `Stream -> true
    | `Url u -> String.length u > 5 && String.sub u 0 5 = "file:"

(* encoder.md §13.2 step 3. *)
let continued_format (format : F.t) =
  match (format.format, List.assoc_opt "movflags" format.opts) with
    | Some m, Some (`String flags) when List.mem m F.iso_family ->
        {
          format with
          opts =
            F.set format.opts "movflags" (`String (flags ^ "+frag_discont"));
        }
    | _ -> format

(* encoder.md §13.2: an HLS restart keeps the variant's timeline. *)
let hls_restart t h =
  close_container t;
  let previous = t.current.copy in
  t.current <-
    open_container ?interrupt:t.interrupt ~on_packet:(on_packet t) ~log:t.log
      ~format:(continued_format t.format)
      ~pos:t.pos ~buffer:t.buffer ();
  Copy_encoder.continue_from t.current.copy ~from:previous;
  t.header <- `Pending (Buffer.length t.buffer);
  h.id3_stream <- None;
  h.media_written <- false;
  Hls_split.container_start h.split ~first:false ~time:t.position;
  t.state <- `Built;
  Av.set_output_metadata t.current.output t.metadata

(* encoder.md §13.2: an Ogg link starts its own timeline. *)
let restart t =
  close_container t;
  t.current <-
    open_container ?interrupt:t.interrupt ~on_packet:(on_packet t)
      ~on_sync_point:(fun () -> access_point t)
      ?direct:t.direct ?filename:t.filename ~log:t.log ~format:t.format
      ~pos:t.pos ~buffer:t.buffer ();
  t.header <- `Pending (Buffer.length t.buffer);
  t.link_start <- Some (Buffer.length t.buffer);
  t.position <- 0;
  t.state <- `Built;
  Av.set_output_metadata t.current.output t.metadata

(* encoder.md §13.3: a source whose header properties the container cannot
   follow restarts it where the target defines a restart; the frame is cut at
   the switch, the part before it written to the old container. *)
let rec encode_frames t frame =
  let switch =
    if t.current.streams_created && (chains t || t.hls <> None) then
      Copy_encoder.switch_position t.current.copy frame
    else None
  in
  match switch with
    | Some position ->
        if position > 0 then encode_frame t (Frame.slice frame position);
        t.log#important "Source parameters changed: restarting the container";
        (match t.hls with Some h -> hls_restart t h | None -> restart t);
        encode_frames t (Frame.after frame position)
    | None -> encode_frame t frame

let encode_pending t frame =
  match t.state with
    | `Failed exn -> raise exn
    | `Stopped -> failwith "Encoder used after stop"
    | `Built | `Started -> encode_frames t frame

let encode t frame =
  encode_pending t frame;
  take t

let encode_metadata t m =
  let m = Frame.Metadata.Export.to_list m in
  t.metadata <-
    m @ List.filter (fun (k, _) -> not (List.mem_assoc k m)) t.format.metadata;
  match t.state with
    | `Failed _ | `Stopped -> ()
    | _ ->
        if not (Av.output_started t.current.output) then
          Av.set_output_metadata t.current.output t.metadata
        else if chains t then restart t
        else t.log#debug "Metadata update not carried by this container"

let stop t =
  t.returned_link_start <- None;
  match t.state with
    | `Stopped -> Strings.empty
    | `Failed _ ->
        (try close_output t.current with _ -> ());
        t.state <- `Stopped;
        Strings.empty
    | `Built | `Started ->
        close_container t;
        t.state <- `Stopped;
        take t

let make ?hls ?interrupt ?direct ?filename ~pos ~name metadata format =
  let log = Log.make ["ffmpeg"; "encoder"; name] in
  let buffer =
    match hls with
      | Some h -> Hls_split.buffer h.split
      | None -> Buffer.create 4096
  in
  let self = ref None in
  let hook field ~key ~time ~write =
    match !self with
      | Some t -> on_packet t field ~key ~time ~write
      | None -> write ()
  in
  let on_sync_point () = Option.iter access_point !self in
  let t =
    {
      log;
      format;
      pos;
      interrupt;
      buffer;
      metadata = format.F.metadata;
      current =
        open_container ?interrupt ~on_packet:hook ~on_sync_point ?direct
          ?filename ~log ~format ~pos ~buffer ();
      state = `Built;
      position = 0;
      header = `Pending 0;
      link_start = None;
      returned_link_start = None;
      access_points = [];
      returned_access_points = [];
      direct;
      filename;
      hls;
    }
  in
  self := Some t;
  encode_metadata t metadata;
  t

let packed_audio = ["adts"; "mp3"; "ac3"; "eac3"]

(* HLS variants.md §2: the segment formats of RFC 8216 §3. *)
let segment_muxer = function
  | Some m ->
      m = "mpegts" || m = "webvtt" || List.mem m packed_audio
      || List.mem m F.iso_family
  | None -> false

(* encoder.md §3 step 2: a key frame at most every 2 s, as close as whole
   frames allow to an even split of the segment. *)
let key_frame_seconds ~segment_duration =
  segment_duration /. Float.max 1. (Float.ceil (segment_duration /. 2.))

(* A raw track with no framerate learns its rate from its first frame: the
   stream encoder sizes its key frame interval then. *)
let key_frame_defaults ~segment_duration (encoded : F.encoded) (video : F.video)
    =
  let default key value opts =
    if List.mem_assoc key opts then opts else F.set opts key (`Int value)
  in
  let seconds = key_frame_seconds ~segment_duration in
  let video, opts =
    match (encoded.mode, video.framerate) with
      | `Raw, None ->
          ({ video with raw_key_frame_interval = Some seconds }, encoded.opts)
      | _ ->
          let interval =
            Time_base.frames ~seconds (Time_base.frame_rate video.framerate)
          in
          ( video,
            encoded.opts |> default "g" interval
            |> default "keyint_min" interval )
  in
  (* [sc_threshold] is a private option of libx264: another encoder rejects it
     as unused. *)
  let opts =
    match encoded.codec with
      | Some ("libx264" | "libx264rgb") -> default "sc_threshold" 0 opts
      | _ -> opts
  in
  { encoded with options = `Video video; opts }

(* encoder.md §3 step 2. *)
let hls_defaults ~segment_duration (format : F.t) =
  let default key value opts =
    if List.mem_assoc key opts then opts else F.set opts key (`String value)
  in
  let opts =
    match format.format with
      | Some m when List.mem m F.iso_family ->
          default "movflags" "+frag_custom+dash+delay_moov" format.opts
      | Some "mpegts" -> default "avoid_negative_ts" "disabled" format.opts
      | _ -> format.opts
  in
  (* Every key frame is a cut candidate: in an open GOP it is not a decoder
     entry point (libx265 makes CRA pictures). *)
  let streams =
    List.map
      (function
        | name, `Encode ({ F.options = `Video video; _ } as encoded) ->
            let encoded = key_frame_defaults ~segment_duration encoded video in
            ( name,
              `Encode
                { encoded with opts = Opts.add_named_flag encoded.opts "cgop" }
            )
        | stream -> stream)
      format.streams
  in
  let interleaved =
    match format.interleaved with `Default -> `False | v -> v
  in
  { format with opts; streams; interleaved; output = `Stream }

(* The init segment of a fragmented MP4 container: its boxes before the first
   fragment's. *)
let mp4_init_length buffer offset =
  let length = Buffer.length buffer in
  let byte i = Char.code (Buffer.nth buffer i) in
  let int32 i =
    (byte i lsl 24)
    lor (byte (i + 1) lsl 16)
    lor (byte (i + 2) lsl 8)
    lor byte (i + 3)
  in
  let rec scan pos =
    if pos + 8 > length then None
    else (
      match Buffer.sub buffer (pos + 4) 4 with
        | "styp" | "sidx" | "moof" -> Some (pos - offset)
        | _ ->
            let size = int32 pos in
            if size < 8 then None else scan (pos + size))
  in
  scan offset

let init_mode t ?id3 () = Encoder.hls_id3_mode ?id3 (Encoder.Ffmpeg t.format)

(* Tags are ordered with the media: written straight away, a tag would come
   before media still held behind the video encoder's latency. *)
let run_id3_queue h =
  if h.media_written then (
    let queue = List.rev h.id3_queue in
    h.id3_queue <- [];
    List.iter
      (fun (time, write) ->
        Hls_order.push ~stream:`Id3 ~time:(Some time) ~write h.order)
      queue;
    release_packets h)

let hls_encode t h ~boundary frame =
  Hls_split.request h.split boundary;
  (match t.state with
    | `Failed exn -> raise exn
    | `Stopped -> failwith "Encoder used after stop"
    | `Built | `Started -> encode_frames t frame);
  run_id3_queue h;
  Hls_split.take h.split

let insert_id3 t h ~time m =
  let tag () =
    Utils.id3v2_of_metadata ~version:h.id3_version
      (Frame.Metadata.Export.to_list m)
  in
  match h.id3 with
    | `None -> None
    | `Prepended -> Some (Hls_id3.make ~version:h.id3_version ~time m)
    | `In_stream ->
        let tag = tag () in
        let write () =
          match h.id3_stream with
            | None -> ()
            | Some stream -> (
                let rate = Lazy.Mutexed.force Frame.audio_rate in
                let time_base = { Avutil.num = 1; den = rate } in
                let ts =
                  Time_base.rescale ~src:(Time_base.main ()) ~dst:time_base time
                in
                let packet = Avcodec.Packet.create tag in
                Avcodec.Packet.set_pts packet (Option.map Int64.of_int ts);
                Avcodec.Packet.set_dts packet (Option.map Int64.of_int ts);
                try Av.write_packet stream time_base packet
                with exn ->
                  t.log#important "Timed ID3 tag dropped: %s"
                    (Printexc.to_string exn))
        in
        h.id3_queue <- (time, write) :: h.id3_queue;
        run_id3_queue h;
        None

type stream_attrs = {
  codec_attr : string option;
  bitrate : int option;
  video_size : (int * int) option;
}

let streams_attrs t =
  List.filter_map
    (function
      | _, `Copy track ->
          Some
            {
              codec_attr = Copy_encoder.codec_attr track;
              bitrate = Copy_encoder.bitrate track;
              video_size = Copy_encoder.video_size track;
            }
      | _, `Encoded track ->
          Some
            {
              codec_attr = track.Encoded_stream.codec_attr ();
              bitrate = track.bitrate ();
              video_size = track.video_size ();
            }
      | _, `Drop -> None)
    t.current.tracks

let codec_attrs t =
  let attrs = List.map (fun a -> a.codec_attr) (streams_attrs t) in
  if attrs <> [] && List.for_all Option.is_some attrs then
    Some (String.concat "," (List.filter_map Fun.id attrs))
  else (
    t.log#important "A stream has no codec string: the variant announces none";
    None)

let bitrate t =
  match F.explicit_bitrate t.format with
    | Some bitrate -> Some bitrate
    | None ->
        let rates = List.map (fun a -> a.bitrate) (streams_attrs t) in
        if rates <> [] && List.for_all Option.is_some rates then
          Some (List.fold_left (fun sum b -> sum + Option.get b) 0 rates)
        else None

let video_size t =
  match List.filter_map (fun a -> a.video_size) (streams_attrs t) with
    | [v] -> Some v
    | _ -> None

let independent_segments t =
  List.for_all
    (function _, `Copy track -> Copy_encoder.intra_only track | _ -> true)
    t.current.tracks

let hls_stop t h =
  (match t.state with
    | `Stopped -> ()
    | `Failed _ -> ( try close_output t.current with _ -> ())
    | `Built | `Started -> close_container t);
  t.state <- `Stopped;
  let taken = Hls_split.take h.split in
  let buffer = Hls_split.buffer h.split in
  let held = Buffer.contents buffer in
  Buffer.clear buffer;
  {
    Encoder.init = Option.join taken.init;
    pieces =
      (taken.pieces
      @ if held = "" then [] else [Encoder.Bytes (Strings.of_string held)]);
  }

let create_hls ~pos ~segment_duration ~name format : Encoder.hls_encoder =
  let format = hls_defaults ~segment_duration format in
  let fields = List.map fst (non_drop format) in
  let muxer = ref "" in
  let init_length buffer offset =
    if List.mem !muxer F.iso_family then mp4_init_length buffer offset
    else Some 0
  in
  let h =
    {
      split = Hls_split.create ~fields ~init_length ();
      order = Hls_order.create ();
      id3 = `None;
      id3_version = 3;
      id3_stream = None;
      id3_queue = [];
      media_written = false;
    }
  in
  let t = make ~hls:h ~pos ~name Frame.Metadata.Export.empty format in
  muxer := t.current.muxer;
  Hls_split.container_start h.split ~first:true ~time:0;
  {
    init =
      (fun ?id3 ?id3_version () ->
        Option.iter (fun v -> h.id3_version <- v) id3_version;
        let mode = init_mode t ?id3 () in
        Result.iter (fun m -> h.id3 <- m) mode;
        mode);
    init_encode =
      (fun ~boundary frame ->
        match hls_encode t h ~boundary frame with
          | { init = Some init; pieces } -> Init { init; pieces }
          | { init = None; _ } -> Pending);
    split_encode =
      (fun ~boundary frame -> (hls_encode t h ~boundary frame).pieces);
    insert_id3 = (fun ~time m -> insert_id3 t h ~time m);
    hls_stop = (fun () -> hls_stop t h);
    codec_attrs = (fun () -> codec_attrs t);
    independent_segments = (fun () -> independent_segments t);
    free_split = (fun () -> Hls_split.all_key h.split);
    hls_bitrate = (fun () -> bitrate t);
    hls_video_size = (fun () -> video_size t);
  }

let create ?interrupt ?direct ?filename ~pos ~name metadata format =
  let t = make ?interrupt ?direct ?filename ~pos ~name metadata format in
  {
    Encoder.encode_metadata = encode_metadata t;
    header =
      (fun () ->
        match t.header with `Captured h -> h | `Pending _ -> Strings.empty);
    encode = encode t;
    access_points = Some (fun () -> t.returned_access_points);
    stop = (fun () -> stop t);
    bitrate = (fun () -> bitrate t);
    restart_offset = (fun () -> t.returned_link_start);
  }

(* An ISO file that is not a file written from its first byte can only be
   read when fragmented: a movie box first, then fragments that start at a key
   frame of the video, or of one second of audio. Explicit [movflags] are the
   script's. *)
let fragmented (format : F.t) =
  let iso =
    match format.format with
      | Some muxer -> List.mem muxer F.iso_family
      | None -> false
  in
  let has_video =
    List.exists
      (function
        | _, `Encode { F.options = `Video _; _ } -> true
        | field, `Copy _ -> String.starts_with ~prefix:"video" field
        | _ -> false)
      format.streams
  in
  if (not iso) || List.mem_assoc "movflags" format.opts then format
  else if has_video then
    {
      format with
      opts =
        F.set format.opts "movflags"
          (`String "+frag_keyframe+empty_moov+default_base_moof");
    }
  else
    {
      format with
      opts =
        F.set
          (F.set format.opts "movflags"
             (`String "+empty_moov+default_base_moof"))
          "frag_duration" (`Int 1_000_000);
    }

let () =
  Plug.register Encoder.plug "ffmpeg" ~doc:"FFmpeg encoder." (function
    | Encoder.Ffmpeg format ->
        Some
          (fun { Encoder.pos; name; capabilities; sink; initial_metadata } ->
            (* A file written from its first byte: the muxer writes and seeks
               in the sink itself, and completes its header at the end. *)
            let file =
              Encoder.file_form capabilities && sink.Encoder.seek <> None
            in
            let direct = if file then Some sink else None in
            let format = if file then format else fragmented format in
            Encoder.writer_of_encoder
              (create ?direct ?filename:sink.Encoder.name ~pos ~name
                 initial_metadata format)
              sink)
    | _ -> None);
  Plug.register Encoder.hls_plug "ffmpeg" ~doc:"FFmpeg HLS segments." (function
    | Encoder.Ffmpeg format when segment_muxer format.F.format ->
        Some
          (fun ~pos ~segment_duration name ->
            create_hls ~pos ~segment_duration ~name format)
    | _ -> None)
