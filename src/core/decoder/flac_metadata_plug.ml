(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
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

let log = Log.make ["decoder"; "flac"; "metadata"]

let mime_types =
  Dtools.Conf.list
    ~p:(Decoder.conf_mime_types#plug "flac_metadata")
    "Mime-types used for decoding metadata using native FLAC metadata parser."
    ~d:["audio/flac"]

let conf_id3 =
  Dtools.Conf.void
    ~p:(Decoder.conf_decoder#plug "flac_metadata")
    "Native FLAC metadata parser settings."

let file_extensions =
  Dtools.Conf.list
    ~p:(Decoder.conf_file_extensions#plug "flac_metadata")
    "File extensions used for decoding metadata using native FLAC parser."
    ~d:["flac"]

let get_tags ~metadata:_ ~extension ~mime parse fname =
  try
    if
      not
        (Decoder.test_file ~log ~extension ~mime ~mimes:(Some mime_types#get)
           ~extensions:(Some file_extensions#get) fname)
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
  Plug.register Request.mresolvers "flac_native"
    ~doc:"Native FLAC metadata resolver."
    (get_tags Metadata.FLAC.parse_file)
