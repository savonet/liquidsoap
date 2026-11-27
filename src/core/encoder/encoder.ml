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

(** Common infrastructure for encoding streams *)

type format = Format_value.t =
  | WAV of Wav_format.t
  | AVI of Avi_format.t
  | NDI of Ndi_format.t
  | Ogg of Ogg_format.t
  | MP3 of Mp3_format.t
  | Shine of Shine_format.t
  | Flac of Flac_format.t
  | Ffmpeg of Liquidsoap_ffmpeg_formats.Encoder_format.t
  | FdkAacEnc of Fdkaac_format.t
  | External of External_encoder_format.t

(** Resolves the deferred values a format's checks need and runs those checks.
    The other deferred values stay unresolved until a writer is built, so that
    the frame's video size can still be detected from the first decoded file. *)
let resolve ~pos format =
  let check_rate ~samplerates = function
    | `Value _ -> ()
    | `Setting setting as rate ->
        let rate = Deferred.force rate in
        if not (List.mem rate samplerates) then
          Encoder_types.raise_error ~pos
            (Printf.sprintf
               "invalid samplerate value %d, read from %s. Possible values: %s"
               rate
               (Deferred.setting_name setting)
               (String.concat ", " (List.map string_of_int samplerates)))
  in
  match format with
    | MP3 m -> (
        check_rate ~samplerates:Mp3_format.samplerates m.Mp3_format.samplerate;
        match m.Mp3_format.samplerate with
          | `Setting _ as rate ->
              Option.iter
                (Encoder_types.raise_error ~pos)
                (Mp3_format.check ~samplerate:(Some (Deferred.force rate)) m)
          | `Value _ -> ())
    | Shine m -> (
        check_rate ~samplerates:Mp3_format.samplerates m.Shine_format.samplerate;
        match m.Shine_format.samplerate with
          | `Setting _ as rate ->
              Option.iter
                (Encoder_types.raise_error ~pos)
                (Mp3_format.bitrate_error ~samplerate:(Deferred.force rate)
                   [m.Shine_format.bitrate])
          | `Value _ -> ())
    | FdkAacEnc m ->
        check_rate ~samplerates:Fdkaac_format.samplerates
          m.Fdkaac_format.samplerate
    | Ogg { Ogg_format.video = Some theora } -> (
        let known rate = Some (Deferred.force rate) in
        match Theora_format.picture_error ~known theora with
          | Some (param, message) ->
              Encoder_types.raise_error ~pos
                (Printf.sprintf
                   "invalid %s value: %s, with the frame's video size %dx%d \
                    read from settings.frame.video"
                   param message
                   (Deferred.force theora.Theora_format.width)
                   (Deferred.force theora.Theora_format.height))
          | None -> ())
    | _ -> ()

(* Formats given to outputs during the script's evaluation, resolved once the
   script has set its frame settings. *)
let pending_resolutions = ref []
let resolutions_done = ref false

(** Resolves [format] at start-up, or now when start-up has passed. *)
let resolve_at_start ~pos format =
  if !resolutions_done then resolve ~pos format
  else pending_resolutions := (pos, format) :: !pending_resolutions

let () =
  Lifecycle.after_script_parse ~name:"encoder formats resolution" (fun () ->
      resolutions_done := true;
      try
        List.iter
          (fun (pos, format) -> resolve ~pos format)
          (List.rev !pending_resolutions);
        pending_resolutions := []
      with Runtime_error.Runtime_error _ as exn -> (
        let bt = Printexc.get_raw_backtrace () in
        (* A configuration error, reported as the script's own errors are. *)
        try Runtime.throw ~lexbuf:None ~bt () exn with Runtime.Error -> exit 1))

let audio_type = Encoder_types.audio_type
let video_format = Encoder_types.video_format
let audio_video_type = Encoder_types.audio_video_type
let video_type = Encoder_types.video_type
let type_of_format f = Format_value.type_of_format ~pos:None f
let string_of_format = Format_value.to_string

let video_size = function
  | Ogg { Ogg_format.video = Some { Theora_format.width; height } } ->
      Some (Deferred.force width, Deferred.force height)
  | Ffmpeg m -> Liquidsoap_ffmpeg_formats.Encoder_format.video_size m
  | _ -> None

(** ISO Base Media File Format, see RFC 6381 section 3.3. *)
let iso_base_file_media_file_format = function
  | MP3 _ | Shine _ -> "mp4a.40.34" (* I have also seen "mp4a.69" and "mp3" *)
  | FdkAacEnc m -> (
      match m.Fdkaac_format.aot with
        | `Mpeg_4 `AAC_LC -> "mp4a.40.2"
        | `Mpeg_4 `HE_AAC -> "mp4a.40.5"
        | `Mpeg_4 `HE_AAC_v2 -> "mp4a.40.29"
        | `Mpeg_4 `AAC_LD -> "mp4a.40.23"
        | `Mpeg_4 `AAC_ELD -> "mp4a.40.39"
        | `Mpeg_2 `AAC_LC -> "mp4a.67"
        | `Mpeg_2 `HE_AAC -> "mp4a.67" (* TODO: check this *)
        | `Mpeg_2 `HE_AAC_v2 -> "mp4a.67" (* TODO: check this *))
  | _ -> raise Not_found

(** Proposed extension for files. *)
let extension = function
  | WAV _ -> "wav"
  | AVI _ -> "avi"
  | Ogg { Ogg_format.audio = Some (Ogg_format.Opus _); video = None } -> "opus"
  | Ogg _ -> "ogg"
  | MP3 _ -> "mp3"
  | Shine _ -> "mp3"
  | Flac _ -> "flac"
  | FdkAacEnc { Fdkaac_format.transmux = `Loas; _ } -> "loas"
  | FdkAacEnc _ -> "aac"
  | Ffmpeg m -> (
      match Liquidsoap_ffmpeg_formats.Encoder_format.extension m with
        | Some ext -> ext
        | None -> raise Not_found)
  | _ -> raise Not_found

(** Mime types *)
let mime = function
  | WAV _ -> "audio/wav"
  | AVI _ -> "video/avi"
  | Ogg _ -> "application/ogg"
  | MP3 _ -> "audio/mpeg"
  | Shine _ -> "audio/mpeg"
  | Flac _ -> "audio/flac"
  | FdkAacEnc _ -> "audio/aac"
  | Ffmpeg m -> Liquidsoap_ffmpeg_formats.Encoder_format.mime m
  | _ -> "application/octet-stream"

(** Bitrate estimation in bits per second. *)
let bitrate = function
  | MP3 w -> Mp3_format.bitrate w
  | Shine w -> Shine_format.bitrate w
  | FdkAacEnc w -> Fdkaac_format.bitrate w
  | _ -> raise Not_found

(** An encoder, once initialized, is something that consumes frames, insert
    metadata and that you eventually close (triggers flushing). Insert metadata
    is really meant for inline metadata, i.e. in most cases, stream sources.
    Otherwise, metadata are passed when creating the encoder. For instance, the
    mp3 encoder may accept metadata initially and write them as id3 tags but
    does not support inline metadata. Also, the ogg encoder supports inline
    metadata but restarts its stream. This is ok, though, because the ogg
    container/streams is meant to be sequentialized but not the mp3 format.
    header contains data that should be sent first to streaming client. *)
type encoder = {
  encode_metadata : Frame.Metadata.Export.t -> unit;
  header : unit -> Strings.t;
  encode : Frame.t -> Strings.t;
  access_points : (unit -> int list) option;
  stop : unit -> Strings.t;
  bitrate : unit -> int option;
  restart_offset : unit -> int option;
}

type hls_id3_mode = [ `None | `In_stream | `Prepended ]

type hls_piece =
  | Bytes of Strings.t
  | Split of { time : int }
  | Restart of { time : int; init : string option }

type hls_ready = { init : string option; pieces : hls_piece list }
type hls_init = Pending | Init of hls_ready

type hls_encoder = {
  init : ?id3:bool -> ?id3_version:int -> unit -> (hls_id3_mode, string) result;
  init_encode : boundary:int -> Frame.t -> hls_init;
  split_encode : boundary:int -> Frame.t -> hls_piece list;
  insert_id3 : time:int -> Frame.Metadata.Export.t -> string option;
  hls_stop : unit -> hls_ready;
  codec_attrs : unit -> string option;
  independent_segments : unit -> bool;
  free_split : unit -> bool;
  hls_bitrate : unit -> int option;
  hls_video_size : unit -> (int * int) option;
}

type hls_factory =
  pos:Pos.t option -> segment_duration:float -> string -> hls_encoder

type stream_start = { header_length : int option; time : int }
type marker = [ `Stream_start of stream_start | `Access_point of int | `End ]

type sink = {
  write : Strings.t -> unit;
  mark : marker -> unit;
  seek : (int -> unit) option;
  read : (int -> int -> string) option;
  size : (unit -> int) option;
  name : string option;
}

type capabilities = {
  seekable : bool;
  continuing : bool;
  live : bool;
  segmented : bool;
  shared : bool;
}

let fresh_stream =
  {
    seekable = false;
    continuing = false;
    live = false;
    segmented = false;
    shared = false;
  }

let live_stream = { fresh_stream with live = true }
let segmented = { fresh_stream with segmented = true }
let shared_stream = { live_stream with shared = true }

type creation = {
  pos : Pos.t option;
  name : string;
  capabilities : capabilities;
  sink : sink;
  initial_metadata : Frame.Metadata.Export.t;
}

type writer = {
  write : Frame.t -> unit;
  metadata : Frame.Metadata.Export.t -> unit;
  finish : [ `Clean | `Abandoned ] -> unit;
  writer_bitrate : unit -> int option;
}

(** A plug-in answers the formats it can write, with what creates their writer.
*)
type plugin = format -> (creation -> writer) option

let plug : plugin Plug.t =
  Plug.create ~doc:"Methods to encode streams." "stream encoding formats"

exception Found of (creation -> writer)

let find_writer format =
  try
    Plug.iter plug (fun _ answer ->
        match answer format with
          | Some create -> raise (Found create)
          | None -> ());
    None
  with Found create -> Some create

let supported format = find_writer format <> None
let () = Format_builders.supported := supported

(* The contract every writer is held to: its calls are serialised, it ends
   once, nothing follows its end, and an in-band offer that is empty or that the
   stream already carries is dropped. *)
let protect ~initial_metadata { write; metadata; finish; writer_bitrate } =
  let lock = Mutex.create () in
  let ended = ref false in
  let carried = ref initial_metadata in
  let running name f =
    Mutex.protect lock (fun () ->
        if !ended then failwith ("Encoder." ^ name ^ ": the writer has ended");
        f ())
  in
  {
    write = (fun frame -> running "write" (fun () -> write frame));
    metadata =
      (fun m ->
        running "metadata" (fun () ->
            if
              (not (Frame.Metadata.Export.is_empty m))
              && not (Frame.Metadata.Export.equal m !carried)
            then (
              carried := m;
              metadata m)));
    finish =
      (fun how ->
        Mutex.protect lock (fun () ->
            if not !ended then (
              ended := true;
              finish how)));
    writer_bitrate = (fun () -> Mutex.protect lock writer_bitrate);
  }

let create_writer format =
  match find_writer format with
    | Some create ->
        fun creation ->
          protect ~initial_metadata:creation.initial_metadata (create creation)
    | None -> raise Not_found

(* The file form of a format that has one: a file written from its first byte,
   whose writer can come back to complete what describes the whole stream. *)
let file_form { seekable; continuing; _ } = seekable && not continuing

(* The ID3v2 tag an MPEG audio file starts with: by default on a file only. *)
let id3v2_tag ~capabilities (id3v2 : int option Encoder_formats.adaptable)
    metadata =
  let version =
    match id3v2 with
      | `Explicit version -> version
      | `Default -> if file_form capabilities then Some 3 else None
  in
  match version with
    | None -> ""
    | Some version ->
        let tag =
          Utils.id3v2_of_metadata ~version
            (Frame.Metadata.to_list
               (Frame.Metadata.Export.to_metadata metadata))
        in
        (* A tag with no frame is only its 10-byte header. *)
        if String.length tag > 10 then tag else ""

(** Why a destination cannot take a format, when it cannot. *)
let refusal { continuing; live; segmented; shared; _ } = function
  | Flac _ when continuing ->
      Some
        "native FLAC cannot extend an existing file: its frames are numbered \
         from the start of their stream. Use %ogg(%flac), which appends a new \
         chained stream"
  | AVI _ when continuing -> Some "%avi cannot extend an existing file"
  | MP3 { Mp3_format.id3v2 = `Explicit (Some _); _ }
  | Shine { Shine_format.id3v2 = `Explicit (Some _); _ }
    when live || continuing || segmented ->
      Some
        "an ID3v2 tag would land in the middle of the stream on this output: \
         remove the id3v2 parameter"
  | FdkAacEnc { Fdkaac_format.transmux = `Adif; _ } when live || continuing ->
      Some
        "transmux=\"adif\" has a single header at the start of its stream: it \
         cannot be read from the middle nor extended"
  | Ffmpeg f
    when shared && not (Liquidsoap_ffmpeg_formats.Encoder_format.streamable f)
    ->
      Some
        "this %ffmpeg format cannot be read from the middle of its stream, \
         where listeners of this output start: use a streamable muxer, or set \
         streamable=true if it is one"
  | External { External_encoder_format.streamable = false; _ } when shared ->
      Some
        "listeners of this output start in the middle of the stream: set \
         streamable=true if the process's output can be read from there"
  | NDI _ -> Some "%ndi can only be used with output.ndi"
  | _ -> None

(* The writer of an encoder returning its bytes. The stream start comes from
   its header and restart offset. Its access points are the offsets it gives
   or, when it gives none, the start of each call's output: encoders return
   whole units. *)
let writer_of_encoder ?(before_end = fun () -> ()) (encoder : encoder)
    (sink : sink) =
  let started = ref false in
  let time = ref 0 in
  let write_bytes bytes =
    if not (Strings.is_empty bytes) then (
      let length = Strings.length bytes in
      let stream_start =
        match (encoder.restart_offset (), !started) with
          | Some offset, _ -> Some offset
          | None, false -> Some 0
          | None, true -> None
      in
      let access_points =
        match encoder.access_points with
          | Some access_points -> access_points ()
          | None -> [0]
      in
      let marks =
        List.sort_uniq
          (fun (offset, _) (offset', _) -> Int.compare offset offset')
          (Option.to_list
             (Option.map (fun offset -> (offset, `Start)) stream_start)
          @ List.filter_map
              (fun offset ->
                if Some offset = stream_start || length <= offset then None
                else Some (offset, `Point))
              access_points)
      in
      let write_slice start stop =
        if start < stop then sink.write (Strings.sub bytes start (stop - start))
      in
      let written =
        List.fold_left
          (fun written (offset, mark) ->
            write_slice written offset;
            (match mark with
              | `Start ->
                  started := true;
                  sink.mark
                    (`Stream_start
                       {
                         header_length =
                           Some (Strings.length (encoder.header ()));
                         time = !time;
                       })
              | `Point -> sink.mark (`Access_point !time));
            offset)
          0 marks
      in
      write_slice written length)
  in
  {
    write =
      (fun frame ->
        let bytes = encoder.encode frame in
        write_bytes bytes;
        time := !time + Frame.position frame);
    metadata = encoder.encode_metadata;
    finish =
      (function
      | `Clean ->
          write_bytes (encoder.stop ());
          before_end ();
          sink.mark `End
      | `Abandoned -> ( try ignore (encoder.stop ()) with _ -> ()));
    writer_bitrate = encoder.bitrate;
  }

(* Serialises the calls of an encoder used without the plug. *)
let protect_encoder
    {
      encode_metadata;
      encode;
      access_points;
      stop;
      header;
      bitrate;
      restart_offset;
    } =
  let m = Mutex.create () in
  let p f = Mutex.protect m f in
  {
    encode_metadata = (fun metadata -> p (fun () -> encode_metadata metadata));
    header = (fun () -> p header);
    encode = (fun frame -> p (fun () -> encode frame));
    access_points =
      Option.map (fun access_points () -> p access_points) access_points;
    stop = (fun () -> p stop);
    bitrate = (fun () -> p bitrate);
    restart_offset = (fun () -> p restart_offset);
  }

(** A plug-in whose writers are encoders returning their bytes. *)
let of_encoders plugin format =
  Option.map
    (fun create { pos; name; sink; initial_metadata; _ } ->
      writer_of_encoder (create ~pos name initial_metadata) sink)
    (plugin format)

let buffered ?pos ~name ~capabilities ~initial_metadata format =
  let create = create_writer format in
  let pending = ref Strings.empty in
  let pending_start = ref None in
  let taken_start = ref None in
  let pending_points = ref [] in
  let taken_points = ref [] in
  let header = ref Strings.empty in
  let header_missing = ref 0 in
  let write bytes =
    if 0 < !header_missing then (
      let length = min !header_missing (Strings.length bytes) in
      header := Strings.append !header (Strings.sub bytes 0 length);
      header_missing := !header_missing - length);
    pending := Strings.append !pending bytes
  in
  let mark = function
    | `Stream_start { header_length; _ } ->
        pending_start := Some (Strings.length !pending);
        pending_points := Strings.length !pending :: !pending_points;
        header := Strings.empty;
        header_missing := Option.value ~default:0 header_length
    | `Access_point _ ->
        pending_points := Strings.length !pending :: !pending_points
    | `End -> ()
  in
  let take () =
    let bytes = !pending in
    pending := Strings.empty;
    taken_start := !pending_start;
    pending_start := None;
    taken_points := List.rev !pending_points;
    pending_points := [];
    bytes
  in
  let writer =
    create
      {
        pos;
        name;
        capabilities;
        sink =
          { write; mark; seek = None; read = None; size = None; name = None };
        initial_metadata;
      }
  in
  {
    encode =
      (fun frame ->
        writer.write frame;
        take ());
    stop =
      (fun () ->
        writer.finish `Clean;
        take ());
    access_points = Some (fun () -> !taken_points);
    encode_metadata = writer.metadata;
    header = (fun () -> !header);
    restart_offset = (fun () -> !taken_start);
    bitrate = writer.writer_bitrate;
  }

(* The HLS ID3 transport of a format, for a variant's [id3] field: in the
   stream for MPEG-TS, before each segment for packed audio (RFC 8216 §3.4),
   none otherwise. *)
let hls_id3_mode ?id3 format =
  let packed_audio = ["adts"; "mp3"; "ac3"; "eac3"] in
  let muxer =
    match format with
      | MP3 _ | Shine _ | FdkAacEnc _ -> Some "mp3"
      | Ffmpeg f -> f.Liquidsoap_ffmpeg_formats.Encoder_format.format
      | _ -> Some ""
  in
  match (muxer, id3) with
    | None, _ -> Error "a format is required"
    | Some "mpegts", Some false -> Ok `None
    | Some "mpegts", _ -> Ok `In_stream
    | Some m, Some false when List.mem m packed_audio ->
        Error "packed audio requires ID3 (RFC 8216 §3.4)"
    | Some m, _ when List.mem m packed_audio -> Ok `Prepended
    | _, Some true -> Error "the format carries no HLS ID3 metadata"
    | _ -> Ok `None

let hls_plug : (format -> hls_factory option) Plug.t =
  Plug.create ~doc:"Methods to encode HLS segments." "hls encoding formats"

exception Found_hls of hls_factory

let protect_hls e =
  let m = Mutex.create () in
  let p f = Mutex.protect m f in
  {
    init =
      (fun ?id3 ?id3_version () -> p (fun () -> e.init ?id3 ?id3_version ()));
    init_encode = (fun ~boundary f -> p (fun () -> e.init_encode ~boundary f));
    split_encode = (fun ~boundary f -> p (fun () -> e.split_encode ~boundary f));
    insert_id3 = (fun ~time m -> p (fun () -> e.insert_id3 ~time m));
    hls_stop = (fun () -> p e.hls_stop);
    codec_attrs = (fun () -> p e.codec_attrs);
    independent_segments = (fun () -> p e.independent_segments);
    free_split = (fun () -> p e.free_split);
    hls_bitrate = (fun () -> p e.hls_bitrate);
    hls_video_size = (fun () -> p e.hls_video_size);
  }

let get_hls_factory fmt =
  try
    Plug.iter hls_plug (fun _ f ->
        match f fmt with
          | Some factory -> raise (Found_hls factory)
          | None -> ());
    raise Not_found
  with Found_hls factory ->
    fun ~pos ~segment_duration name ->
      protect_hls (factory ~pos ~segment_duration name)
