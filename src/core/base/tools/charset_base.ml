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

let conf_charset =
  Dtools.Conf.void
    ~p:(Configure.conf#plug "charset")
    "Settings related to charset conversion."

let conf_path =
  Dtools.Conf.string ~p:(conf_charset#plug "path")
    ~d:(Liquidsoap_paths.camomile_dir ())
    "Directory where charset files are to be found."

let conf_encoding =
  Dtools.Conf.list
    ~p:(conf_charset#plug "encodings")
    ~d:["UTF-8"; "ISO-8859-1"; "UTF-16"]
    "List of encodings to try for automatic encoding detection."

let conf_max_string_length =
  Dtools.Conf.int
    ~p:(conf_charset#plug "max_string_length")
    ~d:1024 "Do not recode strings over that length."

module C = CamomileLib.CharEncoding.Configure (struct
  let basedir = conf_path#get
  let datadir = Filename.concat basedir "database"
  let localedir = Filename.concat basedir "locales"
  let charmapdir = Filename.concat basedir "charmaps"
  let unimapdir = Filename.concat basedir "mappings"
end)

include C

exception Unknown_encoding of string
exception Unsupported_encoding of t

let of_string s =
  try C.of_name (String.uppercase_ascii s)
  with Not_found -> raise (Unknown_encoding s)

let to_string = C.name_of
let custom_encoding = ref None

let automatic_encoding () =
  match !custom_encoding with
    | Some e -> e
    | None ->
        let encs = conf_encoding#get in
        let e = C.automatic "auto" (List.map of_string encs) C.utf8 in
        custom_encoding := Some e;
        e
