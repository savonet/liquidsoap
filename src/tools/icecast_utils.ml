(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2011 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

let mpeg_mime = "audio/mpeg"
let ogg_application_mime = "application/ogg"
let ogg_audio_mime = "audio/ogg"
let ogg_video_mime = "video/ogg"
let wav_mime = "audio/wav"
let aac_mime = "audio/aac"
let aacplus_mime = "audio/aacp"
let flac_mime = "audio/x-flac"

type icecast_protocol = Http | Icy

let base_proto kind = 
    [("protocol", Lang.string_t, (Some (Lang.string "http")),
      Some "Protocol of the streaming server: \
           'http' for Icecast, 'icy' for shoutcast.") ;
      "icy_metadata", Lang.string_t, Some (Lang.string "guess"),
      Some "Send new metadata using the ICY protocol. \
            One of: \"guess\", \"true\", \"false\"";
     "encoding", Lang.string_t, Some (Lang.string ""),
      Some "Encoding used to send metadata. If empty, defaults to UTF-8 \
            for \"http\" protocol and ISO-8859-1 for \"icy\" \
            protocol." ;
     ("format", Lang.string_t, Some (Lang.string ""),
      Some "Format, e.g. \"audio/ogg\". \
      When empty, the encoder is used to guess.") ;
      "", Lang.format_t kind, None, Some "Encoding format."]

module type Icecast_t =
sig
  type protocol

  val protocol_of_icecast_protocol : icecast_protocol -> protocol

  type content

  val format_of_content : string -> content

  type info

  val info_of_encoder : Encoder.format -> info

end

module Icecast_v(M:Icecast_t) = 
struct
  type icy_metadata = Guess | True | False

  let mpeg = M.format_of_content mpeg_mime
  let ogg_application = M.format_of_content ogg_application_mime
  let ogg_audio = M.format_of_content ogg_audio_mime
  let ogg_video = M.format_of_content ogg_video_mime
  let ogg = ogg_application
  let wav = M.format_of_content wav_mime
  let aac = M.format_of_content aac_mime
  let aacplus = M.format_of_content aacplus_mime
  let flac = M.format_of_content flac_mime

  let protocol p =
    let v = List.assoc "protocol" p in
      match Lang.to_string v with
        | "http" -> Http
        | "icy" -> Icy
        | _ ->
            raise (Lang.Invalid_value
                     (v, "Valid values are 'http' (icecast) \
                          and 'icy' (shoutcast)"))
  let format_of_encoder =
    function
      | Encoder.MP3 _ -> Some mpeg
      | Encoder.AACPlus _ -> Some aacplus
      | Encoder.VoAacEnc _ -> Some aac
      | Encoder.External _ -> None
      | Encoder.Flac _ -> Some flac
      | Encoder.WAV _ -> Some wav
      | Encoder.Ogg _ -> Some ogg

  let is_ogg =
    function
      | Encoder.Ogg _ -> true
      | _ -> false

  let encoder_data p =
    let protocol = protocol p in
    let v = Lang.assoc "" 1 p in
    let enc = Lang.to_format v in
    let info,format =
       if (is_ogg enc) && (protocol = Icy) then
         raise (Lang.Invalid_value
               (v, "icy protocol (shoutcast) does not support Ogg")) ;
       M.info_of_encoder enc,format_of_encoder enc
    in
    let encoder_factory =
      try Encoder.get_factory enc with
        | Not_found ->
            raise (Lang.Invalid_value
                     (v, "No encoder found for that format"))
    in
    let format =
      let f = Lang.to_string (List.assoc "format" p) in
        if f <> "" then M.format_of_content f else
          match format with
            | Some x -> x
            | None   -> raise (Lang.Invalid_value (Lang.assoc "" 1 p,
                                                 "No format (mime) found, \
                                                  please specify one."))
    in
    let icy_metadata =
      let v = List.assoc "icy_metadata" p in
      match Lang.to_string v with
        | "guess" -> Guess
        | "true"  -> True
        | "false" -> False
        | _ ->
              raise (Lang.Invalid_value
                       (v, "Valid values are 'guess', \
                            'true' or 'false'"))
    in
    let icy_metadata =
      match format, icy_metadata with
        | _, True -> true
        | _, False -> false
        | x, _ when x = mpeg ||
                    x = wav ||
                    x = aac ||
                    x = aacplus ||
                    x = flac -> true
        | x, _ when x = ogg_application ||
                    x = ogg_audio ||
                    x = ogg_video -> false
        | _, Guess ->
             raise (Lang.Invalid_value
                      (List.assoc "icy_metadata" p,
                       "Could not guess icy_metadata for this format, \
                        please specify either 'true' or 'false'."))
    in
    let out_enc =
      match Lang.to_string (List.assoc "encoding" p) with
        | "" ->
           if protocol = Icy then
             Some "ISO-8859-1"
           else
             None
        | s -> Some s
    in
    let ogg = is_ogg enc in
    (M.protocol_of_icecast_protocol protocol),
    encoder_factory, format, info, icy_metadata, 
    ogg, out_enc

end

