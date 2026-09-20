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

(** A typing environment without closures, marshalable without
    [Marshal.Closures] and therefore readable by js_of_ocaml. Custom types
    become constructors without parameters, named after the custom type, and
    only the language's own constraints survive {!restore}. *)
type t

(** What a dump carries besides the environment: the types of what the language
    leaves to whoever links it, for a reader that has none. *)
type core_types = { source_methods : Type.t option; clock : Type.t option }

val no_core_types : core_types

type restored = {
  restored_env : (string * Type.scheme) list;
  restored_core_types : core_types;
}

val strip : ?core_types:core_types -> (string * Type.scheme) list -> t
val restore : t -> restored

(** The dump format's version: a reader accepts the dumps written with the same
    one, whichever liquidsoap wrote them. *)
val abi_version : int

(** Raises [Invalid_argument] if a closure is left. [version] is the liquidsoap
    that wrote the dump, which {!written_by} reads back. *)
val to_string : version:string -> t -> string

(** Raises [Failure] unless the dump was written in this {!abi_version}. *)
val of_string : string -> t

val written_by : string -> string option
