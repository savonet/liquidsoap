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

type t = [ `ISO_8859_1 | `UTF_16 | `UTF_16BE | `UTF_16LE | `UTF_8 ]

exception Unknown_encoding of string

let of_string : string -> t = function
  | "UTF-8" -> `UTF_8
  | "ISO-8859-1" -> `ISO_8859_1
  | "UTF-16" -> `UTF_16
  | e -> raise (Unknown_encoding e)

let to_string : t -> string = function
  | `UTF_8 -> "UTF-8"
  | `UTF_16 -> "UTF-16"
  | `UTF_16BE -> "UTF-16BE"
  | `UTF_16LE -> "UTF-16LE"
  | `ISO_8859_1 -> "ISO-8859-1"

let camomile_dir = Liquidsoap_paths.camomile_dir

module C = CamomileLibrary.CharEncoding.Configure (struct
  let datadir = Filename.concat camomile_dir "database"
  let localedir = Filename.concat camomile_dir "locales"
  let charmapdir = Filename.concat camomile_dir "charmaps"
  let unimapdir = Filename.concat camomile_dir "mappings"
end)

let of_name s = try C.of_name s with Not_found -> raise (Unknown_encoding s)

let conf_tag =
  Dtools.Conf.void
    ~p:(Configure.conf#plug "tag")
    "Settings related to metadata tags"

let conf_encoding =
  Dtools.Conf.list
    ~p:(conf_tag#plug "encodings")
    ~d:["UTF-8"; "ISO-8859-1"; "UTF-16"]
    "List of encodings to try for automatic encoding detection."

let custom_encoding = ref None

let automatic_encoding () =
  match !custom_encoding with
    | Some e -> e
    | None ->
        let encs = conf_encoding#get in
        let e = C.automatic "auto" (List.map of_name encs) C.utf8 in
        custom_encoding := Some e;
        e

let camolog = Log.make ["camomile"]

let recode_string ~in_enc ~out_enc s =
  try
    try C.recode_string ~in_enc ~out_enc s
    with e ->
      let in_enc =
        if in_enc == automatic_encoding () then
          Printf.sprintf "auto(%s)" (String.concat "," conf_encoding#get)
        else C.name_of in_enc
      in
      camolog#important "Failed to convert %S from %s to %s (%s)!" s in_enc
        (C.name_of out_enc) (Printexc.to_string e);
      s
  with
    | Unknown_encoding e ->
        camolog#important "Failed to convert %S: unknown encoding %s" s e;
        s
    | e ->
        camolog#important "Failed to convert %S: unknown error %s" s
          (Printexc.to_string e);
        s

let enc (e : t) =
  match e with
    | `ISO_8859_1 -> C.of_name "ISO-8859-1"
    | `UTF_8 -> C.utf8
    | `UTF_16 -> C.utf16
    | `UTF_16LE -> C.utf16le
    | `UTF_16BE -> C.utf16be

let convert ?source ?(target = `UTF_8) =
  let in_enc =
    match source with Some e -> enc e | None -> automatic_encoding ()
  in
  let out_enc = enc target in
  recode_string ~in_enc ~out_enc
