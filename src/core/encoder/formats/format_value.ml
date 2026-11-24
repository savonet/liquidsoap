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
