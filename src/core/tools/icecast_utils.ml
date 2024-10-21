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

let mpeg_mime = "audio/mpeg"
let ogg_application_mime = "application/ogg"
let ogg_audio_mime = "audio/ogg"
let ogg_video_mime = "video/ogg"
let wav_mime = "audio/wav"
let avi_mime = "video/avi"
let aac_mime = "audio/aac"
let flac_mime = "audio/flac"

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

  val info_of_encoder : Encoder.format -> Encoder.encoder -> info
end

let ffmpeg_mime_of_format = function
  | "3dostr" -> Some "application/vnd.pg.format"
  | "3g2" -> Some "video/3gpp2"
  | "3gp" -> Some "video/3gpp"
  | "4xm" -> Some "audio/x-adpcm"
  | "a64" -> Some "application/octet-stream "
  | "aa" -> Some "application/octet-stream"
  | "aac" -> Some "audio/aac"
  | "ac3" -> Some "audio/x-ac3"
  | "acm" -> Some "application/octet-stream"
  | "adts" -> Some "audio/aac"
  | "aiff" -> Some "audio/aiff"
  | "amr" -> Some "audio/amr"
  | "apng" -> Some "image/png"
  | "asf" -> Some "video/x-ms-asf"
  | "asf_stream" -> Some "video/x-ms-asf"
  | "ass" -> Some "text/x-ass"
  | "au" -> Some "audio/basic"
  | "avi" -> Some "video/x-msvideo"
  | "avm2" -> Some "application/x-shockwave-flash"
  | "bin" -> Some "application/octet-stream"
  | "bit" -> Some "audio/bit"
  | "calf" -> Some "audio/x-calf"
  | "dts" -> Some "audio/x-dca"
  | "dvd" -> Some "video/mpeg"
  | "eac3" -> Some "audio/x-eac3"
  | "f4v" -> Some "application/f4v"
  | "flac" -> Some "audio/x-flac"
  | "flv" -> Some "video/x-flv"
  | "g722" -> Some "audio/G722"
  | "g723_1" -> Some "audio/g723"
  | "gif" -> Some "image/gif"
  | "gsm" -> Some "audio/x-gsm"
  | "h261" -> Some "video/x-h261"
  | "h263" -> Some "video/x-h263"
  | "hls" -> Some "application/x-mpegURL"
  | "hls,applehttp" -> Some "application/x-mpegURL"
  | "ico" -> Some "image/vnd.microsoft.icon"
  | "ilbc" -> Some "audio/iLBC"
  | "ipod" -> Some "video/mp4"
  | "ismv" -> Some "video/mp4"
  | "jacosub" -> Some "text/x-jacosub"
  | "jpeg_pipe" -> Some "image/jpeg"
  | "jpegls_pipe" -> Some "image/jpeg"
  | "latm" -> Some "audio/MP4A-LATM"
  | "live_flv" -> Some "video/x-flv"
  | "m4v" -> Some "video/x-m4v"
  | "matroska" -> Some "video/x-matroska"
  | "matroska,webm" -> Some "video/webm"
  | "microdvd" -> Some "text/x-microdvd"
  | "mjpeg" -> Some "video/x-mjpeg"
  | "mjpeg_2000" -> Some "video/x-mjpeg"
  | "mmf" -> Some "application/vnd.smaf"
  | "mov,mp4,m4a,3gp,3g2,mj2" -> Some "video/mp4"
  | "mp2" -> Some "audio/mpeg"
  | "mp3" -> Some "audio/mpeg"
  | "mp4" -> Some "video/mp4"
  | "mpeg" -> Some "video/mpeg"
  | "mpeg1video" -> Some "video/mpeg"
  | "mpeg2video" -> Some "video/mpeg"
  | "mpegts" -> Some "video/MP2T"
  | "mpegtsraw" -> Some "video/MP2T"
  | "mpegvideo" -> Some "video/mpeg"
  | "mpjpeg" -> Some "multipart/x-mixed-replace;boundary=ffserver"
  | "mxf" -> Some "application/mxf"
  | "mxf_d10" -> Some "application/mxf"
  | "mxf_opatom" -> Some "application/mxf"
  | "nut" -> Some "video/x-nut"
  | "oga" -> Some "audio/ogg"
  | "ogg" -> Some "application/ogg"
  | "ogv" -> Some "video/ogg"
  | "oma" -> Some "audio/x-oma"
  | "opus" -> Some "audio/ogg"
  | "rm" -> Some "application/vnd.rn-realmedia"
  | "singlejpeg" -> Some "image/jpeg"
  | "smjpeg" -> Some "image/jpeg"
  | "spx" -> Some "audio/ogg"
  | "srt" -> Some "application/x-subrip"
  | "sup" -> Some "application/x-pgs"
  | "svcd" -> Some "video/mpeg"
  | "swf" -> Some "application/x-shockwave-flash"
  | "tta" -> Some "audio/x-tta"
  | "vcd" -> Some "video/mpeg"
  | "vob" -> Some "video/mpeg"
  | "voc" -> Some "audio/x-voc"
  | "wav" -> Some "audio/x-wav"
  | "webm" -> Some "video/webm"
  | "webm_chunk" -> Some "video/webm"
  | "webm_dash_manifest" -> Some "application/xml"
  | "webp" -> Some "image/webp"
  | "webvtt" -> Some "text/vtt"
  | "wv" -> Some "audio/x-wavpack"
  | _ -> None

module Icecast_v (M : Icecast_t) = struct
  type encoder_data = {
    factory : string -> Frame.Metadata.Export.t -> Encoder.encoder;
    format : M.content;
    info : Encoder.encoder -> M.info;
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
    | Encoder.Ffmpeg e ->
        Option.map M.format_of_content
          (match e.Ffmpeg_format.format with
            | None -> None
            | Some v -> ffmpeg_mime_of_format v)
    | Encoder.FdkAacEnc _ -> Some aac
    | Encoder.External _ -> None
    | Encoder.NDI _ -> None
    | Encoder.Flac _ -> Some flac
    | Encoder.WAV _ -> Some wav
    | Encoder.AVI _ -> Some avi
    | Encoder.Ogg _ -> Some ogg

  let encoder_overrides = function
    | Encoder.Ffmpeg { Ffmpeg_format.format = Some "mp3"; opts } as e ->
        if not (Hashtbl.mem opts "id3v2_version") then
          Hashtbl.replace opts "id3v2_version" (`Int 0);
        if not (Hashtbl.mem opts "write_xing") then
          Hashtbl.replace opts "write_xing" (`Int 0);
        e
    | e -> e

  let encoder_data p =
    let v = Lang.assoc "" 1 p in
    let enc = encoder_overrides (Lang.to_format v) in
    let info, format = (M.info_of_encoder enc, format_of_encoder enc) in
    let encoder_factory =
      try Encoder.get_factory enc
      with Not_found ->
        raise (Error.Invalid_value (v, "No encoder found for that format"))
    in
    let format =
      let f = Lang.to_string (List.assoc "format" p) in
      if f <> "" then M.format_of_content f
      else (
        match format with
          | Some x -> x
          | None ->
              raise
                (Error.Invalid_value
                   ( Lang.assoc "" 1 p,
                     "No format (mime) found, please specify one." )))
    in
    { factory = encoder_factory ~pos:(Value.pos v); format; info }
end
