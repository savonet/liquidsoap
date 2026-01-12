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

(** {1 Builtins} *)

(** Whether a given builtin was declared. *)
val has_builtin : string -> bool

(** Retrieve a builtin definition. *)
val get_builtin : string -> (Type.scheme * Value.t) option

(** Add a builtin value. *)
val add_builtin :
  ?override:bool ->
  ?register:bool ->
  ?doc:Doc.Value.t Lazy.t ->
  string list ->
  Type.scheme * Value.t ->
  unit

(** Declare a module. *)
val add_module : string list -> unit

(** {1 Environments} *)

(** Initial typing environment (with builtins). *)
val default_typing_environment : unit -> (string * Type.scheme) list

(** Initial environment (with builtins). *)
val default_environment : unit -> (string * Value.t) list

(** Clear all environments. *)
val clear_toplevel_environments : unit -> unit
