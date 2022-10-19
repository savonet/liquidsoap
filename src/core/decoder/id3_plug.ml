(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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

module Metadata = Metadata.Make (struct
  let enc = function
    | `Auto -> `Auto
    | `UTF_8 -> `UTF8
    | `UTF_16 -> `UTF16
    | `UTF_16LE -> `UTF16LE
    | `UTF_16BE -> `UTF16BE
    | `ISO_8859_1 -> `ISO8859

  let convert e = Camomile_utils.convert (enc e)
end)

let log = Log.make ["decoder"; "id3"]

(** Configuration keys for id3. *)
let mime_types =
  Dtools.Conf.list
    ~p:(Decoder.conf_mime_types#plug "id3")
    "Mime-types used for decoding metadata using native ID3v1 and ID3v2 parser"
    ~d:["audio/mpeg"; "audio/x-wav"]

let conf_id3 =
  Dtools.Conf.void
    ~p:(Decoder.conf_decoder#plug "id3")
    "Native ID3 parser settings"

let file_extensions =
  Dtools.Conf.list
    ~p:(Decoder.conf_file_extensions#plug "id3")
    "File extensions used for decoding metadata using native ID3v1 and ID3v2 \
     parser"
    ~d:["mp3"; "wav"]

let get_tags parse fname =
  try
    if
      not
        (Decoder.test_file ~log ~mimes:mime_types#get
           ~extensions:file_extensions#get fname)
    then raise Metadata.Invalid;
    parse fname
  with
    | Metadata.Invalid -> []
    | e ->
        let bt = Printexc.get_backtrace () in
        Utils.log_exception ~log ~bt
          (Printf.sprintf "Error while decoding file tags: %s"
             (Printexc.to_string e));
        raise Not_found

let () =
  Plug.register Request.mresolvers "ID3" ~doc:"Native decoder for ID3 tags."
    (get_tags Metadata.ID3.parse_file)

let () =
  Plug.register Request.mresolvers "ID3v1" ~doc:"Native decoder for ID3v1 tags."
    (get_tags Metadata.ID3v1.parse_file)

let () =
  Plug.register Request.mresolvers "ID3v2" ~doc:"Native decode for ID3v2 tags."
    (get_tags Metadata.ID3v2.parse_file)
