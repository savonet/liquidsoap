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

type stdlib = { full_term : Term.t; checked_term : Term.t; env : Typing.env }
type append_stdlib = unit -> stdlib

(** Typecheck a term and return it. Might return a cached value! *)
val type_term :
  ?name:string ->
  ?stdlib:append_stdlib ->
  ?term:Term.t ->
  ?ty:Type.t ->
  ?cache_dirtype:Cache.dirtype ->
  cache:bool ->
  trim:bool ->
  lib:bool ->
  Parsed_term.t ->
  Term.t

(** Evaluate a term. *)
val eval_term : ?name:string -> toplevel:bool -> Term.t -> Value.t

(** Raise errors for warnings. *)
val strict : bool ref

(** Raise raw errors. *)
val raw_errors : bool ref

(** Return the list of external libraries. *)
val libs :
  ?error_on_no_stdlib:bool ->
  ?deprecated:bool ->
  stdlib:string ->
  unit ->
  string list

(** Load the external libraries. *)
val load_libs : stdlib:string -> unit -> unit

(* Wrapper for format language errors. Re-raises [Error]
   after printing language errors. *)
val throw :
  ?formatter:Format.formatter ->
  lexbuf:Sedlexing.lexbuf option ->
  bt:Printexc.raw_backtrace ->
  unit ->
  exn ->
  unit

val program :
  (unit -> Parser.token * Lexing.position * Lexing.position) -> Parsed_term.t

(** Interactive loop: read from command line, eval, print and loop. *)
val interactive : unit -> unit

(** Parse a string. *)
val parse : string -> Parsed_term.t * Term.t

val error_header : formatter:Format.formatter -> int -> Pos.Option.t -> unit

(** Report language errors. *)
val report :
  ?default:(unit -> 'a) ->
  lexbuf:Sedlexing.lexbuf option ->
  (throw:(bt:Printexc.raw_backtrace -> exn -> unit) -> unit -> 'a) ->
  'a
