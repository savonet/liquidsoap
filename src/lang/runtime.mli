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

(** {1 Main script evaluation} *)

exception Error

type typing_env = { term : Term.t; env : Typing.env }

type eval_config = {
  fetch_cache : bool;
  save_cache : bool;
  trim : bool;
  typing_env : (unit -> typing_env) option;
  eval : [ `True | `False | `Toplevel ];
}

type eval_mode = [ `Parse_only | `Eval of eval_config ]

(** Report lexbuf related errors. *)
val report : Sedlexing.lexbuf -> (throw:(exn -> unit) -> unit -> unit) -> unit

(** Typecheck a term and return it. Might return a cached value! *)
val type_term :
  name:string ->
  throw:(exn -> unit) ->
  config:eval_config ->
  lib:bool ->
  parsed_term:Parsed_term.t ->
  Term.t ->
  Term.t

(** Evaluate a term. *)
val eval_term : name:string -> config:eval_config -> Term.t -> unit

(** Raise errors for warnings. *)
val strict : bool ref

(** Return the list of external libraries. *)
val libs : ?error_on_no_stdlib:bool -> ?deprecated:bool -> unit -> string list

(** Load the external libraries. *)
val load_libs : unit -> unit

(* Wrapper for format language errors. Re-raises [Error]
   after printing language errors. *)
val throw : ?formatter:Format.formatter -> Sedlexing.lexbuf -> exn -> unit

val program :
  (unit -> Parser.token * Lexing.position * Lexing.position) -> Parsed_term.t

val mk_expr :
  ?fname:string ->
  ((unit -> Parser.token * Lexing.position * Lexing.position) -> Parsed_term.t) ->
  Sedlexing.lexbuf ->
  Term.t

(** Interactive loop: read from command line, eval, print and loop. *)
val interactive : unit -> unit

(** Parse a string. *)
val parse : string -> Term.t

(** Evaluate a string. The result is checked to have the given type. *)
val eval : ignored:bool -> ty:Type.t -> string -> Value.t

val error_header : formatter:Format.formatter -> int -> Pos.Option.t -> unit
