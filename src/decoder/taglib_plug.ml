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

exception Invalid_file

let log = Log.make ["decoder"; "taglib"]

(** Configuration keys for taglib. *)
let mime_types =
  Dtools.Conf.list
    ~p:(Decoder.conf_mime_types#plug "taglib")
    "Mime-types used for decoding metadata using TAGLIB" ~d:["audio/mpeg"]

let conf_taglib =
  Dtools.Conf.void ~p:(Decoder.conf_decoder#plug "taglib") "Taglib settings"

let file_extensions =
  Dtools.Conf.list
    ~p:(Decoder.conf_file_extensions#plug "taglib")
    "File extensions used for decoding metadata using TAGLIB" ~d:["mp3"]

(** We used to force the format. However, now that we check extensions, taglib's
  * automatic format detection should work. *)
let get_tags fname =
  try
    if
      not
        (Decoder.test_file ~mimes:mime_types#get ~extensions:file_extensions#get
           ~log fname)
    then raise Invalid_file;
    let f = Taglib.File.open_file `Autodetect fname in
    Tutils.finalize
      ~k:(fun () -> Taglib.File.close_file f)
      (fun () ->
        let tags =
          try [("year", string_of_int (Taglib.tag_year f))]
          with Not_found -> []
        in
        Hashtbl.fold
          (fun key (values : string list) tags ->
            if values = [] then tags
            else (
              let v = List.hd values in
              if v = "" then tags else (key, v) :: tags ))
          (Taglib.File.properties f) tags)
  with
    | Invalid_file -> []
    | e ->
        log#info "Error while decoding file tags: %s" (Printexc.to_string e);
        log#info "Backtrace:\n%s" (Printexc.get_backtrace ());
        raise Not_found

let () = Request.mresolvers#register "TAGLIB" get_tags
