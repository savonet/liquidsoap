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
  mutable wait_start : int option;
}

type state = [ `Built | `Started | `Failed of exn | `Stopped ]

(* encoder.md §8: what an HLS instance accumulates between two calls of the
   HLS output. [reference] is the first video track, whose key packets are the
   split points; without one, splits are taken at frame starts on the grid. *)
type hls = {
  header_format : bool;
  mp4 : bool;
  reference : string option;
  mutable header : Strings.t option;
  pending_header : Buffer.t;
  mutable started : bool;
  mutable pieces : Encoder.hls_piece list;
  mutable split_boundary : int option;
  mutable awaited_key : int option;
  mutable seen_boundary : int option;
  mutable reference_started : bool;
  mutable id3 :
    [ `None
    | `Timed of (Avutil.output, [ `Data ], [ `Packet ]) Av.stream * int
    | `Packed of int option ];
  mutable id3_queue : (unit -> unit) list;
  mutable restart_at : int option;
}

(* [header] is the current container's header (encoder.md §10): [`Pending
   start] until its first packet, [start] being where its bytes begin in
   [buffer]. *)
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
  mutable hls : hls option;
  mutable header : [ `Pending of int | `Captured of Strings.t ];
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

(* Containers that carry header properties in band follow a change of them
   at a source switch (encoder.md §13.3). *)
let carries_header_in_band muxer =
  List.mem muxer ["mpegts"; "adts"; "h264"; "hevc"; "mp3"]

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

let open_container ?interrupt ?(on_packet = fun _ ~key:_ ~time:_ -> ()) ~log
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
        ~stores_header:(not (carries_header_in_band muxer))
        output
    in
    let tracks =
      List.map
        (fun (field, track) ->
          match track with
            | `Drop -> (field, `Drop)
            | `Copy keyframe ->
                ( field,
                  `Copy
                    (Copy_encoder.track copy
                       ~field:(Frame.Fields.register field)
                       ~keyframe ~on_packet:(on_packet field)) )
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
      wait_start = None;
    }
  with exn -> release_and_raise exn

let copy_tracks c =
  List.filter_map (function _, `Copy t -> Some t | _ -> None) c.tracks

let encoded_tracks c =
  List.filter_map (function _, `Encoded t -> Some t | _ -> None) c.tracks

let take t =
  let s = Buffer.contents t.buffer in
  Buffer.clear t.buffer;
  (match t.header with
    | `Pending _ -> t.header <- `Pending 0
    | `Captured _ -> ());
  Strings.of_string s

(* encoder.md §8.3: the box that starts every MP4 media segment. *)
let styp = "\x00\x00\x00\x18stypmsdh\x00\x00\x00\x00msdhmsix"
let push h piece = h.pieces <- piece :: h.pieces

(* The offset of the first top-level [moof] box: what precedes it is the
   MP4 init segment. With [delay_moov] the muxer writes [ftyp] and [moov] with
   the first fragment, not at the first packet. *)
let first_moof data =
  let length = String.length data in
  let rec scan offset =
    if offset + 8 > length then None
    else (
      let size =
        Int32.to_int (String.get_int32_be data offset) land 0xffffffff
      in
      let kind = String.sub data (offset + 4) 4 in
      if kind = "moof" then Some offset
      else (
        let size =
          if size = 1 && offset + 16 <= length then
            Int64.to_int (String.get_int64_be data (offset + 8))
          else size
        in
        if size < 8 then None else scan (offset + size)))
  in
  scan 0

(* encoder.md §8.4: a restart is returned once the new container's init
   segment is known. *)
let push_restart h =
  Option.iter
    (fun restart_time ->
      h.restart_at <- None;
      push h (`Restart { Encoder.restart_time; init = h.header }))
    h.restart_at

let bytes h data =
  match (h.mp4, h.header) with
    | true, None -> (
        Buffer.add_string h.pending_header (Strings.to_string data);
        let pending = Buffer.contents h.pending_header in
        match first_moof pending with
          | Some offset ->
              Buffer.clear h.pending_header;
              h.header <- Some (Strings.of_string (String.sub pending 0 offset));
              push_restart h;
              push h (`Bytes (Strings.of_string styp));
              push h
                (`Bytes
                   (Strings.of_string
                      (String.sub pending offset
                         (String.length pending - offset))))
          | None -> ())
    | _ -> push h (`Bytes data)

let split t h ~time ~forced =
  Av.flush t.current.output;
  (* With [delay_moov], the first flush writes [moov] alone and keeps the
     samples for the next one. *)
  if h.mp4 && h.header = None then Av.flush t.current.output;
  bytes h (take t);
  push h (`Split { Encoder.time; forced });
  if h.mp4 && h.header <> None then push h (`Bytes (Strings.of_string styp))

(* encoder.md §8.2: the hook every stream calls before a packet reaches the
   muxer. The bytes before the first packet are the header. *)
let on_packet t field ~key ~time =
  (match t.header with
    | `Pending start when t.hls = None ->
        Av.flush t.current.output;
        t.header <-
          `Captured
            (Strings.of_string
               (Buffer.sub t.buffer start (Buffer.length t.buffer - start)))
    | _ -> ());
  match t.hls with
    | None -> ()
    | Some h -> (
        if not h.started then (
          h.started <- true;
          if h.header_format && not h.mp4 then (
            h.header <- Some (take t);
            push_restart h));
        (* The reference's first key packet starts the first segment: a flush
           there would make the MP4 muxer write [moov] before that track's
           first timestamp. *)
          match (key && h.reference = Some field, time) with
          | true, _ when not h.reference_started -> h.reference_started <- true
          | true, Some time ->
              (match h.awaited_key with
                | Some awaited when time >= awaited -> h.awaited_key <- None
                | _ -> ());
              split t h ~time ~forced:false
          | _ -> ())

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
      | Copy_encoder.Incompatible { field; header; source } ->
          let message =
            Printf.sprintf
              "Track %s: a source with parameters %s cannot follow the stream \
               header %s in this container"
              (Frame.Fields.string_of_field field)
              source header
          in
          t.log#severe "%s" message;
          fail t (Failure message)
      | Avutil.Error _ as exn -> fail t exn);
  if length > 0 then t.position <- t.position + length

let close_container t =
  let c = t.current in
  (try
     if c.streams_created then (
       List.iter (fun track -> track.Encoded_stream.flush ()) (encoded_tracks c);
       Copy_encoder.flush c.copy ~position:t.position)
   with exn -> t.log#severe "Draining failed: %s" (Printexc.to_string exn));
  try close_output c
  with exn -> t.log#severe "Closing failed: %s" (Printexc.to_string exn)

(* encoder.md §13.1: Ogg chains, into a stream or by reopening the file. *)
let chains t =
  t.current.muxer = "ogg"
  &&
    match t.format.output with
    | `Stream -> true
    | `Url u -> String.length u > 5 && String.sub u 0 5 = "file:"

(* encoder.md §13.2: an Ogg link starts its own timeline; an HLS variant keeps
   its timeline across segments. *)
let restart t =
  close_container t;
  Option.iter
    (fun h ->
      bytes h (take t);
      h.header <- None;
      Buffer.clear h.pending_header;
      h.started <- false;
      h.reference_started <- false;
      h.split_boundary <- None;
      h.awaited_key <- None;
      h.restart_at <- Some t.position)
    t.hls;
  t.current <-
    open_container ?interrupt:t.interrupt ~on_packet:(on_packet t) ~log:t.log
      ~format:t.format ~pos:t.pos ~buffer:t.buffer ~append:(t.hls = None) ();
  t.header <- `Pending (Buffer.length t.buffer);
  if t.hls = None then t.position <- 0;
  t.state <- `Built;
  Av.set_output_metadata t.current.output t.metadata;
  Option.iter
    (fun h ->
      (match h.id3 with
        | `Timed (_, version) ->
            let audio_rate = Lazy.Mutexed.force Frame.audio_rate in
            h.id3 <-
              `Timed
                ( Av.new_data_stream
                    ~time_base:{ Avutil.num = 1; den = audio_rate }
                    ~codec:`Timed_id3 t.current.output,
                  version )
        | _ -> ());
      if not h.header_format then push_restart h)
    t.hls

(* encoder.md §13.3: a source whose header properties the container cannot
   follow restarts it where the target defines a restart; the frame is cut at
   the switch, the part before it written to the old container. *)
let rec encode_frames t frame =
  let switch =
    if t.current.streams_created && (t.hls <> None || chains t) then
      Copy_encoder.switch_position t.current.copy frame
    else None
  in
  match switch with
    | Some position ->
        if position > 0 then encode_frame t (Frame.slice frame position);
        t.log#important "Source parameters changed: restarting the container";
        restart t;
        encode_frames t (Frame.after frame position)
    | None -> encode_frame t frame

let encode t frame =
  match t.state with
    | `Failed exn -> raise exn
    | `Stopped -> failwith "Encoder used after stop"
    | `Built | `Started ->
        encode_frames t frame;
        Option.iter
          (fun h ->
            if h.started && h.id3_queue <> [] then (
              List.iter (fun write -> write ()) (List.rev h.id3_queue);
              h.id3_queue <- []))
          t.hls;
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

let header_formats = ["mp4"; "mov"; "ismv"; "ipod"; "webm"; "matroska"]
let packed_audio = ["adts"; "mp3"; "ac3"; "eac3"]

let reference format =
  List.find_map
    (fun (field, track) ->
      match track with
        | `Encode { F.options = `Video _; _ } -> Some field
        | `Copy _ when F.media_of_field field = Some `Video -> Some field
        | _ -> None)
    format.F.streams

let hls_state t =
  {
    header_format = List.mem t.current.muxer header_formats;
    mp4 = List.mem t.current.muxer ["mp4"; "mov"; "ismv"; "ipod"];
    reference = reference t.format;
    header = None;
    pending_header = Buffer.create 4096;
    started = false;
    pieces = [];
    split_boundary = None;
    awaited_key = None;
    seen_boundary = None;
    reference_started = false;
    id3 = `None;
    id3_queue = [];
    restart_at = None;
  }

(* encoder.md §8.1. *)
let hls_init t h ?id3_enabled ?id3_version () =
  match (t.current.muxer, id3_enabled) with
    | "", _ -> error ?pos:t.pos "A format is required for HLS"
    | "mpegts", Some false -> false
    | "mpegts", _ ->
        let audio_rate = Lazy.Mutexed.force Frame.audio_rate in
        h.id3 <-
          `Timed
            ( Av.new_data_stream
                ~time_base:{ Avutil.num = 1; den = audio_rate }
                ~codec:`Timed_id3 t.current.output,
              Option.value ~default:3 id3_version );
        true
    | muxer, Some false when List.mem muxer packed_audio ->
        error ?pos:t.pos "Packed audio requires ID3 metadata in HLS"
    | muxer, _ when List.mem muxer packed_audio ->
        h.id3 <- `Packed id3_version;
        true
    | _, Some true -> error ?pos:t.pos "This format carries no HLS ID3 metadata"
    | _ -> false

(* encoder.md §8.5. *)
let hls_insert_id3 t h ~frame_position ~sample_position m =
  match h.id3 with
    | `None -> None
    | `Packed id3_version ->
        Some
          (Encoder_utils.mk_hls_id3 ?id3_version ~frame_position
             ~sample_position m)
    | `Timed (stream, version) ->
        let audio_rate = Lazy.Mutexed.force Frame.audio_rate in
        let ticks =
          (frame_position * Lazy.Mutexed.force Frame.size) + sample_position
        in
        let ts =
          Option.value ~default:0
            (Time_base.rescale ~src:(Time_base.main ())
               ~dst:{ Avutil.num = 1; den = audio_rate }
               ticks)
        in
        let write () =
          let packet =
            Avcodec.Packet.create (Utils.id3v2_of_metadata ~version m)
          in
          Avcodec.Packet.set_pts packet (Some (Int64.of_int ts));
          Avcodec.Packet.set_dts packet (Some (Int64.of_int ts));
          try Av.write_packet stream { Avutil.num = 1; den = audio_rate } packet
          with exn ->
            t.log#important "Timed ID3 tag dropped: %s" (Printexc.to_string exn)
        in
        if h.started then write () else h.id3_queue <- write :: h.id3_queue;
        None

(* encoder.md §8.3 and §8.4. *)
let hls_encode t h ~boundary ~force frame =
  List.iter
    (fun (field, track) ->
      match track with
        | `Encoded track ->
            track.Encoded_stream.request_keyframe boundary;
            if h.reference = Some field && h.split_boundary <> Some boundary
            then (
              h.split_boundary <- Some boundary;
              h.awaited_key <- Some boundary)
        | _ -> ())
    t.current.tracks;
  if force && h.started && h.awaited_key = None && h.restart_at = None then
    split t h ~time:t.position ~forced:true
  else (
    match h.seen_boundary with
      | Some previous
        when h.reference = None && h.started && previous <> boundary
             && t.position >= previous ->
          split t h ~time:t.position ~forced:false
      | _ -> ());
  h.seen_boundary <- Some boundary;
  bytes h (encode t frame);
  let pieces = List.rev h.pieces in
  h.pieces <- [];
  pieces

let hls_init_encode t h ~boundary frame : Encoder.hls_init =
  let pieces = hls_encode t h ~boundary ~force:false frame in
  match (h.header_format, h.header) with
    | false, _ -> `Init (None, pieces)
    | true, Some header -> `Init (Some header, pieces)
    | true, None ->
        h.pieces <- List.rev pieces;
        `Pending

(* encoder.md §8.6. *)
let codec_attrs t =
  let attrs =
    List.filter_map
      (function
        | field, `Encoded track ->
            Some (field, track.Encoded_stream.codec_attr ())
        | field, `Copy track -> Some (field, Copy_encoder.codec_attr track)
        | _, `Drop -> None)
      t.current.tracks
  in
  match List.find_opt (fun (_, attr) -> attr = None) attrs with
    | Some (field, _) ->
        t.log#important "No codec string for track %s: CODECS omitted" field;
        None
    | None -> Some (String.concat "," (List.filter_map snd attrs))

(* encoder.md §8.7 item 6: a copied stream that is not intra-only can be cut by
   a forced split. *)
let independent_segments t =
  List.for_all
    (function _, `Copy track -> Copy_encoder.intra_only track | _ -> true)
    t.current.tracks

let bitrate t =
  match
    List.filter_map
      (function
        | _, `Encoded track -> track.Encoded_stream.bitrate ()
        | _, `Copy track -> Copy_encoder.bitrate track
        | _, `Drop -> None)
      t.current.tracks
  with
    | [] -> None
    | rates -> Some (List.fold_left ( + ) 0 rates)

let video_size t =
  match
    List.filter_map
      (function
        | _, `Encoded track -> track.Encoded_stream.video_size ()
        | _, `Copy track -> Copy_encoder.video_size track
        | _, `Drop -> None)
      t.current.tracks
  with
    | [size] -> Some size
    | _ -> None

(* encoder.md §3 step 2. *)
let hls_defaults (format : F.t) =
  let opts =
    if format.format = Some "mp4" && not (List.mem_assoc "movflags" format.opts)
    then F.set format.opts "movflags" (`String "+frag_custom+dash+delay_moov")
    else format.opts
  in
  let interleaved =
    match format.interleaved with `Default -> `False | other -> other
  in
  { format with opts; interleaved }

let create ?(hls = false) ?interrupt ~pos ~name metadata format =
  let format = if hls then hls_defaults format else format in
  let log = Log.make ["ffmpeg"; "encoder"; name] in
  let buffer = Buffer.create 4096 in
  let self = ref None in
  let hook field ~key ~time =
    Option.iter (fun t -> on_packet t field ~key ~time) !self
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
      hls = None;
      header = `Pending 0;
    }
  in
  self := Some t;
  encode_metadata t metadata;
  let hls_value =
    if hls then (
      let h = hls_state t in
      t.hls <- Some h;
      {
        Encoder.init = hls_init t h;
        init_encode =
          (fun ~boundary frame -> hls_init_encode t h ~boundary frame);
        split_encode =
          (fun ~boundary ~force frame -> hls_encode t h ~boundary ~force frame);
        codec_attrs = (fun () -> codec_attrs t);
        independent_segments = (fun () -> independent_segments t);
        insert_id3 = hls_insert_id3 t h;
        bitrate = (fun () -> bitrate t);
        video_size = (fun () -> video_size t);
      })
    else Encoder.dummy_hls (encode t)
  in
  {
    Encoder.encode_metadata = encode_metadata t;
    header =
      (fun () ->
        match t.header with `Captured h -> h | `Pending _ -> Strings.empty);
    hls = hls_value;
    encode = encode t;
    stop = (fun () -> stop t);
    restart_offset = (fun () -> None);
  }

let () =
  Plug.register Encoder.plug "ffmpeg" ~doc:"FFmpeg encoder." (function
    | Encoder.Ffmpeg format ->
        Some
          (fun ?hls ~pos name metadata ->
            create ?hls ~pos ~name metadata format)
    | _ -> None)
