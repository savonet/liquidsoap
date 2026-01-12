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

(** Strings with view on a substring. *)

(** A string with view. *)
type t

(** Create from a string. *)
val of_string : string -> t

(** Create from a substring. *)
val of_substring : string -> int -> int -> t

(** Extract a string. *)
val to_string : t -> string

(** Extract valid substring. *)
val to_substring : t -> string * int * int

(** Whether it is empty. *)
val is_empty : t -> bool

(** Length. *)
val length : t -> int

(** Extract a substring with view. *)
val sub : t -> int -> int -> t

(** Blit to a bytes. *)
val blit : t -> bytes -> int -> unit
