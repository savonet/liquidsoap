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

let log = Log.make ["decoder"; "id3v2"]

(** Configuration keys for id3v2. *)
let mime_types =
  Dtools.Conf.list
    ~p:(Decoder.conf_mime_types#plug "id3v2")
    "Mime-types used for decoding metadata using native ID3v2 parser"
    ~d:["audio/mpeg"]

let conf_id3v2 =
  Dtools.Conf.void
    ~p:(Decoder.conf_decoder#plug "id3v2")
    "Native ID3v2 parser settings"

let file_extensions =
  Dtools.Conf.list
    ~p:(Decoder.conf_file_extensions#plug "id3v2")
    "File extensions used for decoding metadata using native ID3v2 parser"
    ~d:["mp3"]

let get_tags fname =
  try
    if
      not
        (Decoder.test_file ~mimes:mime_types#get ~extensions:file_extensions#get
           ~log fname)
    then raise Id3v2.Invalid;
    let ic = open_in fname in
    Tutils.finalize
      ~k:(fun () -> close_in ic)
      (fun () -> Id3v2.parse (input ic))
  with
    | Id3v2.Invalid -> []
    | e ->
        log#info "Error while decoding file tags: %s" (Printexc.to_string e);
        log#info "Backtrace:\n%s" (Printexc.get_backtrace ());
        raise Not_found

let () = Request.mresolvers#register "ID3V2" get_tags
