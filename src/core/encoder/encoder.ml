(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

type format =
  | WAV of Wav_format.t
  | AVI of Avi_format.t
  | NDI of Ndi_format.t
  | Ogg of Ogg_format.t
  | MP3 of Mp3_format.t
  | Shine of Shine_format.t
  | Flac of Flac_format.t
  | Ffmpeg of Ffmpeg_format.t
  | FdkAacEnc of Fdkaac_format.t
  | External of External_encoder_format.t

let audio_type ~pcm_kind n =
  Frame.Fields.make
    ~audio:
      (Type.make
         (Format_type.descr
            (`Format (Frame_base.format_of_channels ~pcm_kind n))))
    ()

let video_format () = Content.(default_format Video.kind)

let audio_video_type ~pcm_kind n =
  Frame.Fields.add Frame.Fields.video
    (Type.make (Format_type.descr (`Format (video_format ()))))
    (audio_type ~pcm_kind n)

let video_type () =
  Frame.Fields.make
    ~video:(Type.make (Format_type.descr (`Format (video_format ()))))
    ()

let type_of_format f =
  let audio_type = audio_type ~pcm_kind:Content_audio.kind in
  let audio_video_type = audio_video_type ~pcm_kind:Content_audio.kind in
  match f with
    | WAV w -> audio_type w.Wav_format.channels
    | AVI a -> audio_video_type a.Avi_format.channels
    | MP3 m -> audio_type (if m.Mp3_format.stereo then 2 else 1)
    | Shine m -> audio_type m.Shine_format.channels
    | NDI { audio = false; video = false } -> assert false
    | NDI { audio = true; video = false } ->
        audio_type (Lazy.force Frame.audio_channels)
    | NDI { audio = true; video = true } ->
        audio_video_type (Lazy.force Frame.audio_channels)
    | NDI { audio = false; video = true } -> video_type ()
    | Flac m -> audio_type m.Flac_format.channels
    | Ffmpeg m ->
        List.fold_left
          (fun ctype (field, c) ->
            Frame.Fields.add field
              (match c with
                | `Drop -> Type.var ~constraints:[Format_type.track] ()
                | `Copy _ ->
                    Type.make
                      (Format_type.descr
                         (`Format
                           Content.(
                             default_format (kind_of_string "ffmpeg.copy"))))
                | `Encode { Ffmpeg_format.mode = `Raw; options = `Audio _ } ->
                    Type.make
                      (Format_type.descr
                         (`Format
                           Content.(
                             default_format (kind_of_string "ffmpeg.audio.raw"))))
                | `Encode { Ffmpeg_format.mode = `Raw; options = `Video _ } ->
                    Type.make
                      (Format_type.descr
                         (`Format
                           Content.(
                             default_format (kind_of_string "ffmpeg.video.raw"))))
                | `Encode
                    Ffmpeg_format.
                      {
                        mode = `Internal;
                        options = `Audio { pcm_kind; channels };
                      } ->
                    assert (channels > 0);
                    let params =
                      {
                        Content.Audio.channel_layout =
                          Lazy.from_val
                            (Audio_converter.Channel_layout.layout_of_channels
                               channels);
                      }
                    in
                    Type.make
                      (Format_type.descr
                         (`Format (Frame_base.audio_format ~pcm_kind params)))
                | `Encode { Ffmpeg_format.mode = `Internal; options = `Video _ }
                  ->
                    Type.make
                      (Format_type.descr
                         (`Format Content.(default_format Video.kind))))
              ctype)
          Frame.Fields.empty m.streams
    | FdkAacEnc m -> audio_type m.Fdkaac_format.channels
    | Ogg { Ogg_format.audio; video } -> (
        let channels =
          match audio with
            | Some (Ogg_format.Vorbis { Vorbis_format.channels = n; _ })
            | Some (Ogg_format.Opus { Opus_format.channels = n; _ })
            | Some (Ogg_format.Flac { Flac_format.channels = n; _ }) ->
                n
            | Some (Ogg_format.Speex { Speex_format.stereo; _ }) ->
                if stereo then 2 else 1
            | None -> 0
        in
        match (channels, video) with
          | 0, Some _ -> video_type ()
          | n, Some _ -> audio_video_type n
          | n, None -> audio_type n)
    | External e ->
        let channels = e.External_encoder_format.channels in
        if e.External_encoder_format.video <> None then
          audio_video_type channels
        else audio_type channels

let string_of_format = function
  | WAV w -> Wav_format.to_string w
  | AVI w -> Avi_format.to_string w
  | Ogg w -> Ogg_format.to_string w
  | MP3 w -> Mp3_format.to_string w
  | NDI w -> Ndi_format.to_string w
  | Shine w -> Shine_format.to_string w
  | Flac w -> Flac_format.to_string w
  | Ffmpeg w -> Ffmpeg_format.to_string w
  | FdkAacEnc w -> Fdkaac_format.to_string w
  | External w -> External_encoder_format.to_string w

let video_size = function
  | Ogg { Ogg_format.video = Some { Theora_format.width; height } } ->
      Some (Lazy.force width, Lazy.force height)
  | Ffmpeg m -> (
      match
        List.fold_left
          (fun cur (_, stream) ->
            match stream with
              | `Encode Ffmpeg_format.{ options = `Video { width; height } } ->
                  (width, height) :: cur
              | _ -> cur)
          [] m.Ffmpeg_format.streams
      with
        | (width, height) :: [] -> Some (Lazy.force width, Lazy.force height)
        | _ -> None)
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
  | Ffmpeg { Ffmpeg_format.format = Some "libmp3lame" } -> "mp4a.40.34"
  | Ffmpeg { Ffmpeg_format.format = Some "aac" } -> "mp4a.40.2"
  | _ -> raise Not_found

(** Proposed extension for files. *)
let extension = function
  | WAV _ -> "wav"
  | AVI _ -> "avi"
  | Ogg _ -> "ogg"
  | MP3 _ -> "mp3"
  | Shine _ -> "mp3"
  | Flac _ -> "flac"
  | FdkAacEnc _ -> "aac"
  | Ffmpeg { Ffmpeg_format.format = Some "ogg" } -> "ogg"
  | Ffmpeg { Ffmpeg_format.format = Some "opus" } -> "opus"
  | Ffmpeg { Ffmpeg_format.format = Some "mp3" } -> "mp3"
  | Ffmpeg { Ffmpeg_format.format = Some "matroska" } -> "mkv"
  | Ffmpeg { Ffmpeg_format.format = Some "mpegts" } -> "ts"
  | Ffmpeg { Ffmpeg_format.format = Some "ac3" } -> "ac3"
  | Ffmpeg { Ffmpeg_format.format = Some "eac3" } -> "eac3"
  | Ffmpeg
      {
        Ffmpeg_format.format = Some "adts";
        streams = [(_, `Encode { Ffmpeg_format.codec = Some "aac" })];
      } ->
      "aac"
  | Ffmpeg { Ffmpeg_format.format = Some "adts" } -> "adts"
  | Ffmpeg { Ffmpeg_format.format = Some "mp4" } -> "mp4"
  | Ffmpeg { Ffmpeg_format.format = Some "wav" } -> "wav"
  | _ -> raise Not_found

(** Mime types *)
let mime = function
  | WAV _ -> "audio/wav"
  | AVI _ -> "video/avi"
  | Ogg _ -> "application/ogg"
  | MP3 _ -> "audio/mpeg"
  | Shine _ -> "audio/mpeg"
  | Flac _ -> "audio/flex"
  | FdkAacEnc _ -> "audio/aac"
  | Ffmpeg { Ffmpeg_format.format = Some "ogg" } -> "application/ogg"
  | Ffmpeg { Ffmpeg_format.format = Some "opus" } -> "application/ogg"
  | Ffmpeg { Ffmpeg_format.format = Some "mp3" } -> "audio/mpeg"
  | Ffmpeg { Ffmpeg_format.format = Some "matroska" } -> "video/x-matroska"
  | Ffmpeg { Ffmpeg_format.format = Some "ac3" } -> "audio/ac3"
  | Ffmpeg { Ffmpeg_format.format = Some "eac3" } -> "audio/eac3"
  | Ffmpeg
      {
        Ffmpeg_format.format = Some "adts";
        streams = [(_, `Encode { Ffmpeg_format.codec = Some "aac" })];
      } ->
      "audio/aac"
  | Ffmpeg { Ffmpeg_format.format = Some "mp4" } -> "video/mp4"
  | Ffmpeg { Ffmpeg_format.format = Some "wav" } -> "audio/wav"
  | _ -> "application/octet-stream"

(** Bitrate estimation in kbits per second. *)
let bitrate = function
  | MP3 w -> Mp3_format.bitrate w
  | Shine w -> Shine_format.bitrate w
  | FdkAacEnc w -> Fdkaac_format.bitrate w
  (* For Ffmpeg we rely on hls.bitrate *)
  | _ -> raise Not_found

(** Encoders that can output to a file. *)
let file_output = function Ffmpeg _ -> true | _ -> false

let with_file_output ?(append = false) encoder file =
  match encoder with
    | Ffmpeg params ->
        let opts = Hashtbl.copy params.Ffmpeg_format.opts in
        Hashtbl.replace opts "truncate" (`Int (if append then 0 else 1));
        Ffmpeg
          {
            params with
            Ffmpeg_format.output = `Url (Printf.sprintf "file:%s" file);
            opts;
          }
    | _ -> failwith "No file output!"

(** Encoders that can output to a arbitrary url. *)
let url_output = function Ffmpeg _ -> true | _ -> false

let with_url_output encoder file =
  match encoder with
    | Ffmpeg opts -> Ffmpeg { opts with Ffmpeg_format.output = `Url file }
    | _ -> failwith "No file output!"

(** An encoder, once initialized, is something that consumes
    frames, insert metadata and that you eventually close
    (triggers flushing).
    Insert metadata is really meant for inline metadata, i.e.
    in most cases, stream sources. Otherwise, metadata are
    passed when creating the encoder. For instance, the mp3
    encoder may accept metadata initially and write them as
    id3 tags but does not support inline metadata.
    Also, the ogg encoder supports inline metadata but restarts
    its stream. This is ok, though, because the ogg container/streams
    is meant to be sequentialized but not the mp3 format.
    header contains data that should be sent first to streaming
    client. *)

type split_result =
  [ (* Returns (flushed, first_bytes_for_next_segment) *)
    `Ok of
    Strings.t * Strings.t
  | `Nope of Strings.t ]

(* Raised by [init_encode] if more data is needed. *)
exception Not_enough_data

type hls = {
  (* Returns true if id3 is enabled. *)
  init : ?id3_enabled:bool -> ?id3_version:int -> unit -> bool;
  (* Returns (init_segment, first_bytes) *)
  init_encode : Frame.t -> Strings.t option * Strings.t;
  split_encode : Frame.t -> split_result;
  codec_attrs : unit -> string option;
  insert_id3 :
    frame_position:int ->
    sample_position:int ->
    (string * string) list ->
    string option;
  bitrate : unit -> int option;
  (* width x height *)
  video_size : unit -> (int * int) option;
}

let dummy_hls encode =
  {
    init = (fun ?id3_enabled:_ ?id3_version:_ _ -> false);
    init_encode = (fun f -> (None, encode f));
    split_encode = (fun f -> `Ok (Strings.empty, encode f));
    codec_attrs = (fun () -> None);
    insert_id3 = (fun ~frame_position:_ ~sample_position:_ _ -> None);
    bitrate = (fun () -> None);
    video_size = (fun () -> None);
  }

type encoder = {
  insert_metadata : Frame.Metadata.Export.t -> unit;
  header : unit -> Strings.t;
  hls : hls;
  encode : Frame.t -> Strings.t;
  stop : unit -> Strings.t;
}

type factory =
  ?hls:bool -> pos:Pos.t option -> string -> Frame.Metadata.Export.t -> encoder

(** A plugin might or might not accept a given format.
    If it accepts it, it gives a function creating suitable encoders. *)
type plugin = format -> factory option

let plug : plugin Plug.t =
  Plug.create ~doc:"Methods to encode streams." "stream encoding formats"

exception Found of factory

(** Return the first available encoder factory for that format. *)
let get_factory fmt =
  try
    Plug.iter plug (fun _ f ->
        match f fmt with Some factory -> raise (Found factory) | None -> ());
    raise Not_found
  with Found factory ->
    fun ?hls ~pos name m ->
      let { insert_metadata; hls; encode; stop; header } =
        factory ?hls ~pos name m
      in
      (* Protect all functions with a mutex. *)
      let m = Mutex.create () in
      let insert_metadata = Mutex_utils.mutexify m insert_metadata in
      let header = Mutex_utils.mutexify m header in
      let {
        init;
        init_encode;
        split_encode;
        codec_attrs;
        insert_id3;
        bitrate;
        video_size;
      } =
        hls
      in
      let init ?id3_enabled ?id3_version () =
        Mutex_utils.mutexify m (fun () -> init ?id3_enabled ?id3_version ()) ()
      in
      let init_encode frame =
        Mutex_utils.mutexify m (fun () -> init_encode frame) ()
      in
      let split_encode frame =
        Mutex_utils.mutexify m (fun () -> split_encode frame) ()
      in
      let codec_attrs = Mutex_utils.mutexify m codec_attrs in
      let insert_id3 ~frame_position ~sample_position meta =
        Mutex_utils.mutexify m
          (fun () -> insert_id3 ~frame_position ~sample_position meta)
          ()
      in
      let bitrate = Mutex_utils.mutexify m bitrate in
      let video_size = Mutex_utils.mutexify m video_size in
      let hls =
        {
          init;
          init_encode;
          split_encode;
          codec_attrs;
          insert_id3;
          bitrate;
          video_size;
        }
      in
      let encode frame = Mutex_utils.mutexify m (fun () -> encode frame) () in
      let stop = Mutex_utils.mutexify m stop in
      { insert_metadata; hls; encode; stop; header }
