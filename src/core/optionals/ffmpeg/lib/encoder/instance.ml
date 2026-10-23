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

let open_container ?interrupt ~log ~(format : F.t) ~pos ~buffer
    ?(append = false) () =
  let opts =
    Opts.of_list
      (convert_container_opts pos
         (if append then F.set format.opts "truncate" (`Int 0) else format.opts))
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
  let output =
    match format.output with
      | `Stream -> (
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
                     ~keyframe) )
          | `Encode encoded -> (
              try
                ( field,
                  `Encoded
                    (Encoded_stream.create ~log ~pos
                       ~field:(Frame.Fields.register field)
                       encoded output) )
              with exn ->
                (try Av.close output with _ -> ());
                raise exn))
      format.streams
  in
  { output; muxer; copy; tracks; streams_created = false; wait_start = None }

let copy_tracks c =
  List.filter_map (function _, `Copy t -> Some t | _ -> None) c.tracks

let encoded_tracks c =
  List.filter_map (function _, `Encoded t -> Some t | _ -> None) c.tracks

let take t =
  let s = Buffer.contents t.buffer in
  Buffer.clear t.buffer;
  Strings.of_string s

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

let encode t frame =
  match t.state with
    | `Failed exn -> raise exn
    | `Stopped -> failwith "Encoder used after stop"
    | `Built | `Started ->
        let c = t.current in
        if not c.streams_created then create_streams t frame;
        let length = Frame.position frame in
        if c.streams_created && length > 0 then (
          try
            List.iter
              (fun track ->
                track.Encoded_stream.encode ~position:t.position frame)
              (encoded_tracks c);
            List.iter
              (fun track ->
                Copy_encoder.encode track ~position:t.position frame)
              (copy_tracks c);
            Copy_encoder.end_of_frame c.copy ~position:t.position
          with
            | Copy_encoder.Incompatible { field; header; source } ->
                let message =
                  Printf.sprintf
                    "Track %s: a source with parameters %s cannot follow the \
                     stream header %s in this container"
                    (Frame.Fields.string_of_field field)
                    source header
                in
                t.log#severe "%s" message;
                fail t (Failure message)
            | Avutil.Error _ as exn -> fail t exn);
        if length > 0 then t.position <- t.position + length;
        take t

let close_container t =
  let c = t.current in
  (try
     if c.streams_created then (
       List.iter (fun track -> track.Encoded_stream.flush ()) (encoded_tracks c);
       Copy_encoder.flush c.copy ~position:t.position)
   with exn -> t.log#severe "Draining failed: %s" (Printexc.to_string exn));
  try Av.close c.output
  with exn -> t.log#severe "Closing failed: %s" (Printexc.to_string exn)

let restartable t =
  t.current.muxer = "ogg"
  &&
    match t.format.output with
    | `Stream -> true
    | `Url u -> String.length u > 5 && String.sub u 0 5 = "file:"

let encode_metadata t m =
  let m = Frame.Metadata.Export.to_list m in
  t.metadata <-
    m @ List.filter (fun (k, _) -> not (List.mem_assoc k m)) t.format.metadata;
  match t.state with
    | `Failed _ | `Stopped -> ()
    | _ ->
        if not (Av.output_started t.current.output) then
          Av.set_output_metadata t.current.output t.metadata
        else if restartable t then (
          close_container t;
          t.current <-
            open_container ?interrupt:t.interrupt ~log:t.log ~format:t.format
              ~pos:t.pos ~buffer:t.buffer ~append:true ();
          t.position <- 0;
          t.state <- `Built;
          Av.set_output_metadata t.current.output t.metadata)
        else t.log#debug "Metadata update not carried by this container"

let stop t =
  match t.state with
    | `Stopped -> Strings.empty
    | `Failed _ ->
        (try Av.close t.current.output with _ -> ());
        t.state <- `Stopped;
        Strings.empty
    | `Built | `Started ->
        close_container t;
        t.state <- `Stopped;
        take t

let create ?(hls = false) ?interrupt ~pos ~name metadata format =
  ignore hls;
  let log = Log.make ["ffmpeg"; "encoder"; name] in
  let buffer = Buffer.create 4096 in
  let t =
    {
      log;
      format;
      pos;
      interrupt;
      buffer;
      metadata = format.F.metadata;
      current = open_container ?interrupt ~log ~format ~pos ~buffer ();
      state = `Built;
      position = 0;
    }
  in
  encode_metadata t metadata;
  let encode = encode t in
  {
    Encoder.encode_metadata = encode_metadata t;
    header = (fun () -> Strings.empty);
    hls = Encoder.dummy_hls encode;
    encode;
    stop = (fun () -> stop t);
  }

let () =
  Plug.register Encoder.plug "ffmpeg" ~doc:"FFmpeg encoder." (function
    | Encoder.Ffmpeg format ->
        Some
          (fun ?hls ?interrupt ~pos name metadata ->
            create ?hls ?interrupt ~pos ~name metadata format)
    | _ -> None)
