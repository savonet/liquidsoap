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

(** A character set. *)
type t = [ `ISO_8859_1 | `UTF_16 | `UTF_16BE | `UTF_16LE | `UTF_8 ]

exception Unknown_encoding of string
exception Unsupported_encoding of t

(** Charset from string. *)
val of_string : string -> t

(** String name of charset. *)
val to_string : t -> string

(** Convert between charsets. By default, source charset is automatically
    detected and target charset is UTF8. The function will silently fail unless
    [fail] is set to [true]. *)
val convert : ?fail:bool -> ?source:t -> ?target:t -> string -> string
