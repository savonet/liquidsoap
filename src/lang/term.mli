(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

include module type of Runtime_term

exception Internal_error of (Pos.t list * string)
exception Unsupported_encoder of (Pos.t option * string)

val conf_debug : bool ref
val conf_debug_errors : bool ref
val debug : bool Lazy.t
val profile : bool ref
val ref_t : ?pos:Pos.t -> Type.t -> Type.t

module Custom = Term_base.Custom

type encoder_params =
  [ `Anonymous of string | `Encoder of encoder | `Labelled of string * t ] list

and encoder = string * encoder_params

val unit : ast
val is_ground : t -> bool
val string_of_pat : pattern -> string
val to_string : t -> string

val make :
  ?pos:Pos.t ->
  ?t:Type.t ->
  ?flags:Flags.flags ->
  ?methods:t Methods.t ->
  ast ->
  t

val free_vars : ?bound:Vars.elt list -> t -> Vars.t
val free_fun_vars : (t, Type.t) func -> Vars.t
val can_ignore : Type.t -> bool
val fresh : handler:Type.Fresh.mapper -> t -> t

exception Unbound of Pos.Option.t * string
exception Ignored of t
exception No_label of t * string * bool * t
exception Duplicate_label of Pos.Option.t * string
exception Missing_arguments of Pos.Option.t * (string * Type.t) list
exception Unused_variable of (string * Pos.t)
exception Deprecated of (string * Pos.t)

val check_unused :
  throw:(bt:Printexc.raw_backtrace -> exn -> unit) -> lib:bool -> t -> unit

module type Custom = sig
  type content

  val t : Type.t
  val to_custom : content -> Custom.t
  val of_custom : Custom.t -> content
  val is_custom : Custom.t -> bool
  val to_term : content -> t
  val of_term : t -> content
end

module type CustomDef = sig
  type content

  val name : string
  val to_string : content -> string
  val to_json : pos:Pos.t list -> content -> Json.t
  val compare : content -> content -> int
end

module MkCustom (Def : CustomDef) : Custom with type content = Def.content
