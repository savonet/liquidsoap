(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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
    "Mime-types used for decoding metadata using TAGLIB"
    ~d:
      [
        "audio/mpeg";
        "audio/ogg";
        "video/ogg";
        "audio/wav";
        "audio/wave";
        "audio/flac";
        "audio/mp4";
        "video/mp4";
      ]

let conf_taglib =
  Dtools.Conf.void ~p:(Decoder.conf_decoder#plug "taglib") "Taglib settings"

let file_extensions =
  Dtools.Conf.list
    ~p:(Decoder.conf_file_extensions#plug "taglib")
    "File extensions used for decoding metadata using TAGLIB"
    ~d:["mp3"; "ogg"; "mkv"; "wav"; "flac"; "mp4"; "m4a"]

let int_tags = [("year", Taglib.tag_year); ("tracknumber", Taglib.tag_track)]
let tag_aliases = [("track", "tracknumber")]

(** We used to force the format. However, now that we check extensions, taglib's
  * automatic format detection should work. *)
let get_tags ~metadata:_ ~extension ~mime fname =
  try
    if
      not
        (Decoder.test_file ~log ~extension ~mime ~mimes:(Some mime_types#get)
           ~extensions:(Some file_extensions#get) fname)
    then raise Invalid_file;
    let f =
      try Taglib.File.open_file `OggOpus fname
      with _ -> Taglib.File.open_file `Autodetect fname
    in
    Fun.protect
      ~finally:(fun () -> Taglib.File.close_file f)
      (fun () ->
        let tags =
          List.fold_left
            (fun cur (lbl, fn) ->
              try
                let v = fn f in
                if v = 0 then cur else (lbl, string_of_int v) :: cur
              with Not_found -> cur)
            [] int_tags
        in
        Hashtbl.fold
          (fun key (values : string list) tags ->
            let key = String.lowercase_ascii key in
            let key = try List.assoc key tag_aliases with _ -> key in
            if List.mem_assoc key tags || values = [] then tags
            else (
              let v = List.hd values in
              match v with
                | "0" when List.mem_assoc key int_tags -> tags
                | "" -> tags
                | _ -> (key, v) :: tags))
          (Taglib.File.properties f) tags)
  with
    | Invalid_file -> []
    | e ->
        let bt = Printexc.get_backtrace () in
        Utils.log_exception ~log ~bt
          (Printf.sprintf "Error while decoding file tags: %s"
             (Printexc.to_string e));
        raise Not_found

let () = Plug.register Request.mresolvers "taglib" ~doc:"" get_tags
