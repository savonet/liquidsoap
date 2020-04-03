(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

let mpeg_mime = "audio/mpeg"
let ogg_application_mime = "application/ogg"
let ogg_audio_mime = "audio/ogg"
let ogg_video_mime = "video/ogg"
let wav_mime = "audio/wav"
let avi_mime = "video/avi"
let aac_mime = "audio/aac"
let flac_mime = "audio/x-flac"

let base_proto kind =
  [
    ( "format",
      Lang.string_t,
      Some (Lang.string ""),
      Some
        "Format, e.g. \"audio/ogg\". When empty, the encoder is used to guess."
    );
    ("", Lang.format_t kind, None, Some "Encoding format.");
  ]

module type Icecast_t = sig
  type content

  val format_of_content : string -> content

  type info

  val info_of_encoder : Encoder.format -> info
end

module Icecast_v (M : Icecast_t) = struct
  type encoder_data = {
    factory : string -> Meta_format.export_metadata -> Encoder.encoder;
    format : M.content;
    info : M.info;
  }

  let mpeg = M.format_of_content mpeg_mime
  let ogg_application = M.format_of_content ogg_application_mime
  let ogg_audio = M.format_of_content ogg_audio_mime
  let ogg_video = M.format_of_content ogg_video_mime
  let ogg = ogg_application
  let wav = M.format_of_content wav_mime
  let avi = M.format_of_content avi_mime
  let aac = M.format_of_content aac_mime
  let flac = M.format_of_content flac_mime

  let format_of_encoder = function
    | Encoder.MP3 _ -> Some mpeg
    | Encoder.Shine _ -> Some mpeg
    | Encoder.Ffmpeg _ -> None
    | Encoder.FdkAacEnc _ -> Some aac
    | Encoder.External _ -> None
    | Encoder.GStreamer _ -> None
    | Encoder.Flac _ -> Some flac
    | Encoder.WAV _ -> Some wav
    | Encoder.AVI _ -> Some avi
    | Encoder.Ogg _ -> Some ogg

  let encoder_data p =
    let v = Lang.assoc "" 1 p in
    let enc = Lang.to_format v in
    let info, format = (M.info_of_encoder enc, format_of_encoder enc) in
    let encoder_factory =
      try Encoder.get_factory enc
      with Not_found ->
        raise
          (Lang_errors.Invalid_value (v, "No encoder found for that format"))
    in
    let format =
      let f = Lang.to_string (List.assoc "format" p) in
      if f <> "" then M.format_of_content f
      else (
        match format with
          | Some x -> x
          | None ->
              raise
                (Lang_errors.Invalid_value
                   ( Lang.assoc "" 1 p,
                     "No format (mime) found, please specify one." )) )
    in
    { factory = encoder_factory; format; info }
end
