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

type t =
  [ `ISO_8859_1
  | `ISO_8859_10
  | `ISO_8859_11
  | `ISO_8859_13
  | `ISO_8859_14
  | `ISO_8859_15
  | `ISO_8859_16
  | `ISO_8859_2
  | `ISO_8859_3
  | `ISO_8859_4
  | `ISO_8859_5
  | `ISO_8859_6
  | `ISO_8859_7
  | `ISO_8859_8
  | `ISO_8859_9
  | `KOI8_R
  | `KOI8_U
  | `UTF_16
  | `UTF_16BE
  | `UTF_16LE
  | `UTF_8
  | `UTF_7 ]

exception Unknown_encoding of string
exception Unsupported_encoding of t
exception Malformed_input of string

let log = Log.make ["charset"]

let all_encodings =
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
    `UTF_7;
  ]

let of_string : string -> t = function
  | "ISO-8859-1" -> `ISO_8859_1
  | "ISO-8859-10" -> `ISO_8859_10
  | "ISO-8859-11" -> `ISO_8859_11
  | "ISO-8859-13" -> `ISO_8859_13
  | "ISO-8859-14" -> `ISO_8859_14
  | "ISO-8859-15" -> `ISO_8859_15
  | "ISO-8859-16" -> `ISO_8859_16
  | "ISO-8859-2" -> `ISO_8859_2
  | "ISO-8859-3" -> `ISO_8859_3
  | "ISO-8859-4" -> `ISO_8859_4
  | "ISO-8859-5" -> `ISO_8859_5
  | "ISO-8859-6" -> `ISO_8859_6
  | "ISO-8859-7" -> `ISO_8859_7
  | "ISO-8859-8" -> `ISO_8859_8
  | "ISO-8859-9" -> `ISO_8859_9
  | "KOI8-R" -> `KOI8_R
  | "KOI8-U" -> `KOI8_U
  | "UTF-7" -> `UTF_7
  | "UTF-8" -> `UTF_8
  | "UTF-16" -> `UTF_16
  | "UTF_16BE" -> `UTF_16BE
  | "UTF_16LE" -> `UTF_16LE
  | e -> raise (Unknown_encoding e)

let to_string : t -> string = function
  | `ISO_8859_1 -> "ISO-8859-1"
  | `ISO_8859_10 -> "ISO-8859-10"
  | `ISO_8859_11 -> "ISO-8859-11"
  | `ISO_8859_13 -> "ISO-8859-13"
  | `ISO_8859_14 -> "ISO-8859-14"
  | `ISO_8859_15 -> "ISO-8859-15"
  | `ISO_8859_16 -> "ISO-8859-16"
  | `ISO_8859_2 -> "ISO-8859-2"
  | `ISO_8859_3 -> "ISO-8859-3"
  | `ISO_8859_4 -> "ISO-8859-4"
  | `ISO_8859_5 -> "ISO-8859-5"
  | `ISO_8859_6 -> "ISO-8859-6"
  | `ISO_8859_7 -> "ISO-8859-7"
  | `ISO_8859_8 -> "ISO-8859-8"
  | `ISO_8859_9 -> "ISO-8859-9"
  | `KOI8_R -> "KOI8-R"
  | `KOI8_U -> "KOI8-U"
  | `UTF_7 -> "UTF-7"
  | `UTF_8 -> "UTF-8"
  | `UTF_16 -> "UTF-16"
  | `UTF_16BE -> "UTF-16BE"
  | `UTF_16LE -> "UTF-16LE"

let () =
  Printexc.register_printer (function
    | Unknown_encoding s -> Some [%string "Unknown encoding: %{s}"]
    | Unsupported_encoding t ->
        let enc = to_string t in
        Some [%string "Unsupported encoding: %{enc}"]
    | Malformed_input s -> Some [%string "Malformed input: %{s}"]
    | _ -> None)
