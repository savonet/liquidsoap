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
        ~muxer output
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
  t.returned_link_start <- t.link_start;
  t.link_start <- None;
  (match t.header with
    | `Pending _ -> t.header <- `Pending 0
    | `Captured _ -> ());
  Strings.of_string s

(* The hook every stream calls before a packet reaches the muxer. The bytes
   before the first packet are the header; the bindings would only write it
   with that packet, so it is written here. *)
let on_packet t _field ~key:_ ~time:_ =
  match t.header with
    | `Pending start ->
        Av.write_header t.current.output;
        t.header <-
          `Captured
            (Strings.of_string
               (Buffer.sub t.buffer start (Buffer.length t.buffer - start)))
    | _ -> ()

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
    if t.current.streams_created && chains t then
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

let create ?interrupt ~pos ~name metadata format =
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
      header = `Pending 0;
      link_start = None;
      returned_link_start = None;
    }
  in
  self := Some t;
  encode_metadata t metadata;
  {
    Encoder.encode_metadata = encode_metadata t;
    header =
      (fun () ->
        match t.header with `Captured h -> h | `Pending _ -> Strings.empty);
    encode = encode t;
    stop = (fun () -> stop t);
    restart_offset = (fun () -> t.returned_link_start);
  }

let () =
  Plug.register Encoder.plug "ffmpeg" ~doc:"FFmpeg encoder." (function
    | Encoder.Ffmpeg format ->
        Some (fun ~pos name metadata -> create ~pos ~name metadata format)
    | _ -> None)
