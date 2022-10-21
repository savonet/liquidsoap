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

let supported_encodings =
  [
    `ISO_8859_1;
    `ISO_8859_10;
    `ISO_8859_11;
    `ISO_8859_13;
    `ISO_8859_14;
    `ISO_8859_15;
    `ISO_8859_16;
    `ISO_8859_2;
    `ISO_8859_3;
    `ISO_8859_4;
    `ISO_8859_5;
    `ISO_8859_6;
    `ISO_8859_7;
    `ISO_8859_8;
    `ISO_8859_9;
    `KOI8_R;
    `KOI8_U;
    `UTF_16;
    `UTF_16BE;
    `UTF_16LE;
    `UTF_8;
  ]

let description = "camomile implementation"
let can_detect = supported_encodings
let can_decode = supported_encodings
let can_encode = supported_encodings

let conf_camomile =
  Dtools.Conf.void
    ~p:(Configure.conf#plug "camomile")
    "Settings related to camomile library (for charset conversion)."

let conf_path =
  let d =
    match Liquidsoap_paths.mode with
      | `Default -> CamomileDefaultConfig__.InstallConfig.share_dir
      | `Posix -> "/usr/share/liquidsoap/camomile"
      | `Standalone ->
          Filename.concat (Liquidsoap_paths.rundir ()) "../camomile"
  in
  Dtools.Conf.string
    ~p:(conf_camomile#plug "path")
    ~d "Directory where camomile files are to be found."

let conf_encoding =
  Dtools.Conf.list
    ~p:(conf_camomile#plug "encodings")
    ~d:["UTF-8"; "ISO-8859-1"; "UTF-16"]
    "List of encodings to try for automatic encoding detection."

module C = CamomileLibrary.CharEncoding.Configure (struct
  let basedir = conf_path#get
  let datadir = Filename.concat basedir "database"
  let localedir = Filename.concat basedir "locales"
  let charmapdir = Filename.concat basedir "charmaps"
  let unimapdir = Filename.concat basedir "mappings"
end)

let of_name s =
  try C.of_name s with Not_found -> raise (Charset_base.Unknown_encoding s)

let custom_encoding = ref None

let automatic_encoding () =
  match !custom_encoding with
    | Some e -> e
    | None ->
        let encs = conf_encoding#get in
        let e = C.automatic "auto" (List.map of_name encs) C.utf8 in
        custom_encoding := Some e;
        e

let recode_string ~in_enc ~out_enc s =
  try C.recode_string ~in_enc ~out_enc s
  with e ->
    let in_enc =
      if in_enc == automatic_encoding () then
        Printf.sprintf "auto(%s)" (String.concat "," conf_encoding#get)
      else C.name_of in_enc
    in
    Charset_base.log#important "Failed to convert %S from %s to %s (%s)!" s
      in_enc (C.name_of out_enc) (Printexc.to_string e);
    s

let enc e =
  match e with
    | `UTF_8 -> C.utf8
    | `UTF_16 -> C.utf16
    | `UTF_16LE -> C.utf16le
    | `UTF_16BE -> C.utf16be
    | x -> C.of_name (Charset_base.to_string x)

let convert ?source ?(target = `UTF_8) =
  let in_enc =
    match source with Some e -> enc e | None -> automatic_encoding ()
  in
  let out_enc = enc target in
  recode_string ~in_enc ~out_enc
