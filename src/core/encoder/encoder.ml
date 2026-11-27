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
    | MP3 m ->
        check_rate ~samplerates:Mp3_format.samplerates m.Mp3_format.samplerate
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

(** The bytes of one encoder output, readable only during the call that hands
    them over: [blit offset destination destination_offset length] copies a
    range of them. *)
type output = { length : int; blit : int -> bytes -> int -> int -> unit }

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
  encode_into : (Frame.t -> (output -> unit) -> unit) option;
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
}

type capabilities = {
  seekable : bool;
  continuing : bool;
  live : bool;
  segmented : bool;
}

let fresh_stream =
  { seekable = false; continuing = false; live = false; segmented = false }

let live_stream = { fresh_stream with live = true }
let segmented = { fresh_stream with segmented = true }

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

(* A writer's calls are serialised. *)
let protect { write; metadata; finish; writer_bitrate } =
  let lock = Mutex.create () in
  {
    write = (fun frame -> Mutex.protect lock (fun () -> write frame));
    metadata = (fun m -> Mutex.protect lock (fun () -> metadata m));
    finish = (fun how -> Mutex.protect lock (fun () -> finish how));
    writer_bitrate = (fun () -> Mutex.protect lock writer_bitrate);
  }

let create_writer format =
  match find_writer format with
    | Some create -> fun creation -> protect (create creation)
    | None -> raise Not_found

(* Formats whose encoders return whole frames on each call: the start of a
   call's output is an access point. *)
let unit_aligned = function
  | MP3 _ | Shine _ | WAV _ -> true
  | FdkAacEnc { Fdkaac_format.transmux = `Adts | `Loas; _ } -> true
  | _ -> false

let writer_of_encoder ~unit_aligned (encoder : encoder) (sink : sink) =
  let started = ref false in
  let time = ref 0 in
  let start_stream () =
    started := true;
    sink.mark
      (`Stream_start
         {
           header_length = Some (Strings.length (encoder.header ()));
           time = !time;
         })
  in
  let write_bytes bytes =
    if not (Strings.is_empty bytes) then (
      match (encoder.restart_offset (), !started) with
        | Some offset, _ ->
            if 0 < offset then sink.write (Strings.sub bytes 0 offset);
            start_stream ();
            sink.write (Strings.drop bytes offset)
        | None, false ->
            start_stream ();
            sink.write bytes
        | None, true ->
            if unit_aligned then sink.mark (`Access_point !time);
            sink.write bytes)
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
          sink.mark `End
      | `Abandoned -> ( try ignore (encoder.stop ()) with _ -> ()));
    writer_bitrate = encoder.bitrate;
  }

(* Serialises the calls of an encoder used without the plug. *)
let protect_encoder
    {
      encode_metadata;
      encode;
      encode_into;
      stop;
      header;
      bitrate;
      restart_offset;
    } =
  let m = Mutex.create () in
  let encode_metadata metadata =
    Mutex.protect m (fun () -> encode_metadata metadata)
  in
  let header () = Mutex.protect m header in
  let encode frame = Mutex.protect m (fun () -> encode frame) in
  let encode_into =
    Option.map
      (fun encode_into frame f ->
        Mutex.protect m (fun () -> encode_into frame f))
      encode_into
  in
  let stop () = Mutex.protect m stop in
  let bitrate () = Mutex.protect m bitrate in
  let restart_offset () = Mutex.protect m restart_offset in
  {
    encode_metadata;
    encode;
    encode_into;
    stop;
    header;
    bitrate;
    restart_offset;
  }

(** A plug-in whose writers are encoders returning their bytes. *)
let of_encoders plugin format =
  Option.map
    (fun create { pos; name; sink; initial_metadata; _ } ->
      writer_of_encoder ~unit_aligned:(unit_aligned format)
        (create ~pos name initial_metadata)
        sink)
    (plugin format)

let buffered ?pos ~name ~capabilities ~initial_metadata format =
  let create = create_writer format in
  let pending = ref Strings.empty in
  let pending_start = ref None in
  let taken_start = ref None in
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
        header := Strings.empty;
        header_missing := Option.value ~default:0 header_length
    | `Access_point _ | `End -> ()
  in
  let take () =
    let bytes = !pending in
    pending := Strings.empty;
    taken_start := !pending_start;
    pending_start := None;
    bytes
  in
  let writer =
    create
      {
        pos;
        name;
        capabilities;
        sink = { write; mark; seek = None; read = None };
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
    encode_into = None;
    encode_metadata = writer.metadata;
    header = (fun () -> !header);
    restart_offset = (fun () -> !taken_start);
    bitrate = writer.writer_bitrate;
  }

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
