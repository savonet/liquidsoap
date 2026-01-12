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

(** Operations on positions (in source files). *)

type t

type pos = {
  fname : string;
  lstart : int;
  lstop : int;
  cstart : int;
  cstop : int;
}

val pack_offset : int
val pack : pos -> t
val unpack : t -> pos
val of_lexing_pos : Lexing.position * Lexing.position -> t
val to_string : ?prefix:string -> t -> string
val string_of_pos : ?prefix:string -> t -> string

module Option : sig
  type base = t
  type t = base option

  val to_string : ?prefix:string -> t -> string
end

module List : sig
  type base = t
  type t = base list

  val to_pos : t -> base
  val to_string : ?newlines:bool -> ?prefix:string -> t -> string
end
