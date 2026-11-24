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

(** A format value: the configuration a [%name(...)] expression describes. *)
type t =
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

let to_string = function
  | WAV w -> Wav_format.to_string w
  | AVI w -> Avi_format.to_string w
  | Ogg w -> Ogg_format.to_string w
  | MP3 w -> Mp3_format.to_string w
  | NDI w -> Ndi_format.to_string w
  | Shine w -> Shine_format.to_string w
  | Flac w -> Flac_format.to_string w
  | Ffmpeg w -> Liquidsoap_ffmpeg_formats.Encoder_format.to_string w
  | FdkAacEnc w -> Fdkaac_format.to_string w
  | External w -> External_encoder_format.to_string w

(** The frame type a format value encodes. *)
let type_of_format ~pos f =
  let audio_type = Encoder_types.audio_type ~pcm_kind:Audio_format.kind in
  let audio_video_type =
    Encoder_types.audio_video_type ~pcm_kind:Audio_format.kind
  in
  let video_type = Encoder_types.video_type in
  match f with
    | WAV w -> audio_type w.Wav_format.channels
    | AVI a -> audio_video_type a.Avi_format.channels
    | MP3 m -> audio_type (if m.Mp3_format.stereo then 2 else 1)
    | Shine m -> audio_type m.Shine_format.channels
    | NDI { audio = false; video = false } -> assert false
    | NDI { audio = true; video = false } ->
        audio_type (!Audio_format.channels ())
    | NDI { audio = true; video = true } ->
        audio_video_type (!Audio_format.channels ())
    | NDI { audio = false; video = true } -> video_type ()
    | Flac m -> audio_type m.Flac_format.channels
    | Ffmpeg m -> Liquidsoap_ffmpeg_formats.Encoder_format.type_of_format m
    | FdkAacEnc m -> audio_type m.Fdkaac_format.channels
    | Ogg { Ogg_format.audio; video } -> (
        let channels =
          match audio with
            | Some (Ogg_format.Vorbis { Vorbis_format.channels = n; _ })
            | Some (Ogg_format.Opus { Opus_format.channels = n; _ })
            | Some (Ogg_format.Flac { Flac_format.channels = n; _ }) ->
                Some n
            | Some (Ogg_format.Speex { Speex_format.stereo; _ }) ->
                Some (if stereo then 2 else 1)
            | None -> None
        in
        match (channels, video) with
          | None, None ->
              Encoder_types.raise_error ~pos
                "%ogg needs a vorbis, opus, speex, flac or theora stream"
          | None, Some _ -> video_type ()
          | Some n, Some _ -> audio_video_type n
          | Some n, None -> audio_type n)
    | External e ->
        let channels = e.External_encoder_format.channels in
        if e.External_encoder_format.video <> None then
          audio_video_type channels
        else audio_type channels
