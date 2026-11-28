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
  appended : Unix.file_descr option;
  muxer : string;
  copy : Copy_encoder.t;
  tracks : (string * track) list;
  mutable streams_created : bool;
  mutable flushed : bool;
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
  (* encoder.md §8.9: the audio and video fields, the first DTS of those
     that produced a packet, and those that had none when the offset was set. *)
  mutable counted : string list;
  mutable first_dts : (string * int) list;
  mutable offset : int option;
  mutable late : string list;
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

(* FFmpeg's [file] protocol has no append mode: [truncate] = 0 writes from the
   start of the file. An appending file output writes through a descriptor
   opened for appending; [output.file] asks for it with [truncate] = 0. *)
let appended_path ~append (format : F.t) =
  match format.output with
    | `Url u
      when String.starts_with ~prefix:"file:" u
           && (append || List.assoc_opt "truncate" format.opts = Some (`Int 0))
      ->
        Some (String.sub u 5 (String.length u - 5))
    | _ -> None

let close_output c =
  Fun.protect
    ~finally:(fun () -> Option.iter Unix.close c.appended)
    (fun () -> Av.close c.output)

let open_container ?interrupt
    ?(on_packet = fun _ ~key:_ ~time:_ ~dts:_ ~write -> write ()) ~log
    ~(format : F.t) ~pos ~buffer ?(append = false) () =
  let appended_path = appended_path ~append format in
  let opts =
    Opts.of_list
      (convert_container_opts pos
         (if appended_path = None then format.opts
          else List.remove_assoc "truncate" format.opts))
  in
  let interleaved =
    match format.interleaved with
      | `True -> true
      | `False -> false
      | `Default -> List.length (non_drop format) > 1
  in
  let guess () =
    Av.Format.guess_output_format ?short_name:format.format
      ?filename:(match format.output with `Url u -> Some u | `Stream -> None)
      ?mime:format.mime_type ()
  in
  let appended =
    Option.map
      (fun path ->
        Unix.openfile path [Unix.O_WRONLY; Unix.O_APPEND; Unix.O_CREAT] 0o644)
      appended_path
  in
  let close_appended () = Option.iter Unix.close appended in
  let output =
    try
      match (format.output, appended) with
        | `Url _, Some fd -> (
            match guess () with
              | None ->
                  error ?pos "No ffmpeg format could be found for this file"
              | Some muxer ->
                  Av.open_output_stream ~opts ~interleaved
                    (fun bytes offset length ->
                      Unix.write fd bytes offset length)
                    muxer)
        | `Stream, _ -> (
            match (format.format, guess ()) with
              | None, _ -> error ?pos "Format is required!"
              | Some f, None ->
                  error ?pos
                    (Printf.sprintf
                       "No ffmpeg format could be found for format=%S" f)
              | Some _, Some muxer ->
                  let write bytes offset length =
                    Buffer.add_subbytes buffer bytes offset length;
                    length
                  in
                  Av.open_output_stream ~opts ~interleaved write muxer)
        | `Url url, None ->
            Av.open_output ?interrupt ?format:(guess ()) ~interleaved ~opts url
    with exn ->
      close_appended ();
      raise exn
  in
  (match Opts.unused_message [("container", opts)] with
    | Some message ->
        (try Av.close output with _ -> ());
        close_appended ();
        error ?pos message
    | None -> ());
  let muxer =
    match guess () with Some f -> Av.Format.get_output_name f | None -> ""
  in
  let release_and_raise exn =
    (try Av.close output with _ -> ());
    close_appended ();
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
    {
      output;
      appended;
      muxer;
      copy;
      tracks;
      streams_created = false;
      flushed = false;
      wait_start = None;
    }
  with exn -> release_and_raise exn

let copy_tracks c =
  List.filter_map (function _, `Copy t -> Some t | _ -> None) c.tracks

let encoded_tracks c =
  List.filter_map (function _, `Encoded t -> Some t | _ -> None) c.tracks

let reset t =
  Buffer.clear t.buffer;
  t.returned_link_start <- t.link_start;
  t.link_start <- None;
  match t.header with `Pending _ -> t.header <- `Pending 0 | `Captured _ -> ()

let take t =
  let s = Buffer.contents t.buffer in
  reset t;
  Strings.of_string s

(* The pending bytes handed to [f] straight from [buffer], which keeps its
   capacity: nothing is allocated. *)
let take_into t f =
  Fun.protect
    ~finally:(fun () -> reset t)
    (fun () ->
      f
        {
          Encoder.length = Buffer.length t.buffer;
          blit =
            (fun offset destination destination_offset length ->
              Buffer.blit t.buffer offset destination destination_offset length);
        })

let hls_flush t () =
  Av.flush t.current.output;
  if t.current.muxer = "mpegts" then
    Av.set_output_option t.current.output "mpegts_flags" "+resend_headers"

let subtitle_fields t =
  List.filter_map
    (function
      | field, `Encode { F.options = `Subtitle _; _ } -> Some field | _ -> None)
    t.format.F.streams

let encoded_video t =
  List.filter_map
    (function
      | field, `Encode { F.options = `Video _; _ } -> Some field | _ -> None)
    t.format.F.streams

(* Runs when a packet is due at the muxer: at once outside HLS, in media-time
   order in HLS mode (encoder.md §8.2). The bytes before the first packet are
   the header; the bindings would only write it with that packet, so it is
   written here. *)
let write_packet t field ~key ~time ~write =
  (match (t.header, t.hls) with
    | `Pending _, Some _ ->
        Av.write_header t.current.output;
        t.header <- `Captured Strings.empty
    | `Pending start, None ->
        Av.write_header t.current.output;
        t.header <-
          `Captured
            (Strings.of_string
               (Buffer.sub t.buffer start (Buffer.length t.buffer - start)))
    | _ -> ());
  match (t.hls, time) with
    | Some h, Some time ->
        h.media_written <- true;
        Hls_split.packet h.split ~flush:(hls_flush t) ~field ~key ~time;
        write ()
    | Some h, None ->
        h.media_written <- true;
        write ()
    | None, _ -> write ()

(* encoder.md §8.9: nothing reaches the muxer before the offset is set. *)
let release_packets ?all h =
  if h.offset <> None then
    Hls_order.release ?all
      ~gating:
        (List.map
           (fun field -> `Track field)
           (Hls_split.inter_coded_fields h.split))
      h.order

(* The variant's own delay: the lowest first DTS, fields without a packet
   counting as 0. *)
let own_delay h =
  List.fold_left (fun delay (_, dts) -> max delay (-dts)) 0 h.first_dts

(* encoder.md §8.9: the muxer adds the offset to every packet it writes. *)
let apply_offset t h =
  Option.iter
    (fun offset ->
      let micros =
        Option.get
          (Time_base.rescale ~rounding:`Up ~src:(Time_base.main ())
             ~dst:{ Avutil.num = 1; den = 1_000_000 }
             offset)
      in
      Av.set_output_option t.current.output "output_ts_offset"
        (Printf.sprintf "%dus" micros))
    h.offset

let fix_offset t h offset =
  h.offset <- Some offset;
  h.late <-
    List.filter (fun field -> not (List.mem_assoc field h.first_dts)) h.counted;
  apply_offset t h

let set_offset t h offset =
  fix_offset t h offset;
  release_packets h

(* Before a container closes no packet may stay held: its write targets that
   container. A write that fails is logged and the others still go out. *)
let rec drain_held t h =
  if h.offset = None then (
    if h.first_dts <> [] then
      t.log#important
        "Container closed before the output's offset was set: using this \
         variant's own";
    fix_offset t h (own_delay h));
  match release_packets ~all:true h with
    | () -> ()
    | exception exn ->
        t.log#severe "Packet dropped at the end of the container: %s"
          (Printexc.to_string exn);
        drain_held t h

(* encoder.md §8.9: the shared offset keeps every timestamp of the
   container positive. Only a field without a first packet when the offset was
   set may fall below it. *)
let above_offset t field h dts =
  match (h.offset, dts) with
    | Some offset, Some dts when dts + offset < 0 ->
        let log =
          if List.mem field h.late then t.log#severe else t.log#critical
        in
        log "Track %s: DTS %.3fs is below the output's initial offset %.3fs"
          field
          (Frame.seconds_of_main dts)
          (Frame.seconds_of_main offset);
        assert (List.mem field h.late);
        false
    | _ -> true

let record_first_dts h field dts =
  match dts with
    | Some dts
      when List.mem field h.counted && not (List.mem_assoc field h.first_dts) ->
        h.first_dts <- (field, dts) :: h.first_dts
    | _ -> ()

let on_packet t field ~key ~time ~dts ~write =
  match t.hls with
    | Some h ->
        record_first_dts h field dts;
        if above_offset t field h dts then (
          Hls_order.push ~stream:(`Track field) ~time
            ~write:(fun () -> write_packet t field ~key ~time ~write)
            h.order;
          release_packets h)
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
          c.tracks;
        if h.offset = None then
          h.counted <-
            List.filter_map
              (function
                | field, `Encoded _ when List.mem field (subtitle_fields t) ->
                    None
                | field, `Copy track
                  when Copy_encoder.media track = Some `Subtitle ->
                    None
                | _, `Drop -> None
                | field, _ -> Some field)
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
      | Avutil.Error _ as exn -> fail t exn);
  if length > 0 then t.position <- t.position + length

let flush_codecs t =
  let c = t.current in
  if c.streams_created && not c.flushed then (
    c.flushed <- true;
    try
      List.iter (fun track -> track.Encoded_stream.flush ()) (encoded_tracks c);
      Copy_encoder.flush c.copy ~position:t.position
    with exn -> t.log#severe "Draining failed: %s" (Printexc.to_string exn))

let close_container t =
  let c = t.current in
  flush_codecs t;
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
  apply_offset t h;
  Hls_split.container_start h.split ~first:false ~time:t.position;
  t.state <- `Built;
  Av.set_output_metadata t.current.output t.metadata

(* encoder.md §13.2: an Ogg link starts its own timeline. *)
let restart t =
  close_container t;
  t.current <-
    open_container ?interrupt:t.interrupt ~on_packet:(on_packet t) ~log:t.log
      ~format:t.format ~pos:t.pos ~buffer:t.buffer ~append:true ();
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

let encode_into t frame f =
  encode_pending t frame;
  take_into t f

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

let make ?hls ?interrupt ~pos ~name metadata format =
  let log = Log.make ["ffmpeg"; "encoder"; name] in
  let buffer =
    match hls with
      | Some h -> Hls_split.buffer h.split
      | None -> Buffer.create 4096
  in
  let self = ref None in
  let hook field ~key ~time ~dts ~write =
    match !self with
      | Some t -> on_packet t field ~key ~time ~dts ~write
      | None -> write ()
  in
  let t =
    {
      log;
      format;
      pos;
      interrupt;
      buffer;
      metadata = format.F.metadata;
      current =
        open_container ?interrupt ~on_packet:hook ~log ~format ~pos ~buffer ();
      state = `Built;
      position = 0;
      header = `Pending 0;
      link_start = None;
      returned_link_start = None;
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

let init_mode t ?id3 () =
  match (t.current.muxer, id3) with
    | "", _ -> Error "a format is required"
    | "mpegts", Some false -> Ok `None
    | "mpegts", _ -> Ok `In_stream
    | m, Some false when List.mem m packed_audio ->
        Error "packed audio requires ID3 (RFC 8216 §3.4)"
    | m, _ when List.mem m packed_audio -> Ok `Prepended
    | _, Some true -> Error "the format carries no HLS ID3 metadata"
    | _ -> Ok `None

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

(* encoder.md §8.9: known once every audio and video field produced a
   packet, or once flushed codecs produced all they will. *)
let initial_delay t h ~final =
  match h.offset with
    | Some offset -> `Known offset
    | None ->
        if final then flush_codecs t;
        if
          final
          || t.current.streams_created
             && List.for_all
                  (fun field -> List.mem_assoc field h.first_dts)
                  h.counted
        then `Known (own_delay h)
        else `Partial (own_delay h)

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
    | `Prepended ->
        let offset = Option.value ~default:0 h.offset in
        Some (Hls_id3.make ~version:h.id3_version ~time:(time + offset) m)
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
      counted = [];
      first_dts = [];
      offset = None;
      late = [];
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
    initial_delay = (fun ~final () -> initial_delay t h ~final);
    set_offset = (fun offset -> set_offset t h offset);
    hls_stop = (fun () -> hls_stop t h);
    codec_attrs = (fun () -> codec_attrs t);
    independent_segments = (fun () -> independent_segments t);
    free_split = (fun () -> Hls_split.all_key h.split);
    hls_bitrate = (fun () -> bitrate t);
    hls_video_size = (fun () -> video_size t);
  }

let create ?interrupt ~pos ~name metadata format =
  let t = make ?interrupt ~pos ~name metadata format in
  {
    Encoder.encode_metadata = encode_metadata t;
    header =
      (fun () ->
        match t.header with `Captured h -> h | `Pending _ -> Strings.empty);
    encode = encode t;
    encode_into = Some (encode_into t);
    stop = (fun () -> stop t);
    bitrate = (fun () -> bitrate t);
    restart_offset = (fun () -> t.returned_link_start);
  }

let () =
  Plug.register Encoder.plug "ffmpeg" ~doc:"FFmpeg encoder." (function
    | Encoder.Ffmpeg format ->
        Some (fun ~pos name metadata -> create ~pos ~name metadata format)
    | _ -> None);
  Plug.register Encoder.hls_plug "ffmpeg" ~doc:"FFmpeg HLS segments." (function
    | Encoder.Ffmpeg format when segment_muxer format.F.format ->
        Some
          (fun ~pos ~segment_duration name ->
            create_hls ~pos ~segment_duration ~name format)
    | _ -> None)
