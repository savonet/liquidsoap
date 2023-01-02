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

type t = [ `ISO_8859_1 | `UTF_16 | `UTF_16BE | `UTF_16LE | `UTF_8 ]

exception Unknown_encoding of string
exception Unsupported_encoding of t

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
