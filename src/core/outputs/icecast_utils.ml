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

let mpeg_mime = "audio/mpeg"
let ogg_application_mime = "application/ogg"
let ogg_audio_mime = "audio/ogg"
let ogg_video_mime = "video/ogg"
let wav_mime = "audio/wav"
let avi_mime = "video/avi"
let aac_mime = "audio/aac"
let flac_mime = "audio/flac"

(* A listener joining mid-stream must not receive an ID3v2 header or a Xing
   frame: an mp3 %ffmpeg stream gets a copy of its format without them. *)
let streaming_format = function
  | Encoder.Ffmpeg
      ({ Liquidsoap_ffmpeg_formats.Encoder_format.format = Some "mp3"; opts } as
       m) ->
      let default key opts =
        if List.mem_assoc key opts then opts
        else Liquidsoap_ffmpeg_formats.Encoder_format.set opts key (`Int 0)
      in
      Encoder.Ffmpeg
        { m with opts = default "write_xing" (default "id3v2_version" opts) }
  | format -> format

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

  (* The format helper's MIME type, the one table of every output. *)
  let format_of_encoder = function
    | Encoder.External _ | Encoder.NDI _ -> None
    | format -> (
        match Encoder.mime format with
          | "application/octet-stream" -> None
          | mime -> Some (M.format_of_content mime))

  let encoder_data ?(capabilities = Encoder.live_stream) p =
    let v = Lang.assoc "" 1 p in
    let enc = streaming_format (Lang_encoder.output_format ~capabilities v) in
    let info, format = (M.info_of_encoder enc, format_of_encoder enc) in
    let factory name initial_metadata =
      Encoder.buffered ?pos:(Value.pos v) ~name ~capabilities ~initial_metadata
        enc
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
                     "No format (mime) found, please specify one.",
                     [] )))
    in
    { factory; format; info }
end
