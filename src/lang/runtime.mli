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

(** {1 Main script evaluation} *)

exception Error

(** Raise errors for warnings. *)
val strict : bool ref

(** Load the external libraries. *)
val load_libs :
  ?error_on_no_stdlib:bool ->
  ?parse_only:bool ->
  ?deprecated:bool ->
  ?stdlib:string ->
  unit ->
  unit

(* Wrapper for format language errors. Re-raises [Error]
   after printing language errors. *)
val throw : ?formatter:Format.formatter -> Sedlexing.lexbuf -> exn -> unit

val mk_expr :
  ?print_ast:bool ->
  ?fname:string ->
  pwd:string ->
  (Parser.token, Parsed_term.t) MenhirLib.Convert.traditional ->
  Sedlexing.lexbuf ->
  Term.t

(** Evaluate a script from an [in_channel]. *)
val from_in_channel :
  ?print_ast:bool -> ?parse_only:bool -> lib:bool -> in_channel -> unit

(** Evaluate a script from a file. *)
val from_file :
  ?print_ast:bool -> ?parse_only:bool -> lib:bool -> string -> unit

(** Evaluate a script from a string. *)
val from_string :
  ?print_ast:bool -> ?parse_only:bool -> lib:bool -> string -> unit

(** Interactive loop: read from command line, eval, print and loop. *)
val interactive : unit -> unit

(** Parse a string. *)
val parse : string -> Term.t

(** Evaluate a string. The result is checked to have the given type. *)
val eval : ignored:bool -> ty:Type.t -> string -> Value.t

val error_header : formatter:Format.formatter -> int -> Pos.Option.t -> unit
