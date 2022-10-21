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

(** A character set. *)
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

(** Description of the implementation. *)
val description : string

(** List of all available encodings. *)
val all_encodings : t list

(** Implementation capabilities. *)
val can_detect : t list

val can_decode : t list
val can_encode : t list

(** Charset from string. *)
val of_string : string -> t

(** String name of charset. *)
val to_string : t -> string

(** Convert between charsets. By default, source charset is automatically
    detected and target charset is UTF8. *)
val convert : ?source:t -> ?target:t -> string -> string
