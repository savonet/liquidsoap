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

(** Helper functions for the parser. *)

module Vars = Parsed_term.Vars

type arglist = Parsed_term.fun_arg list
type pos = Parsed_term.pos

type lexer_let_decoration =
  [ `Eval
  | `Json_parse
  | `None
  | `Recursive
  | `Replaces
  | `Yaml_parse
  | `Xml_parse
  | `Sqlite_row
  | `Sqlite_query ]

type let_opt_el = string * Parsed_term.t

val clear_comments : unit -> unit
val get_pending_comments : unit -> (pos * string list) list
val append_comment : pos:pos -> string -> unit
val attach_comments : Parsed_term.t -> unit
val mk_stmt : pos:pos -> Parsed_term.statement_ast -> Parsed_term.statement
val mk_block : pos:pos -> Parsed_term.statement list -> Parsed_term.block

(** A block where an expression is expected. A single-expression block is
    returned unwrapped, which is the common case (`if a then`, `def f() = e
    end`) and keeps the AST free of redundant `Block` nodes. *)
val expr_of_block : pos:pos -> Parsed_term.block -> Parsed_term.t

val block_expr : pos:pos -> Parsed_term.statement list -> Parsed_term.t

(** Reinterpret a bare binding's left-hand side, parsed as an expression, as a
    pattern plus an optional type annotation. Raises [Term.Parse_error] if it is
    not a valid target. *)
val binding_target :
  Parsed_term.t -> Parsed_term.pattern * Parsed_term.type_annotation option

val let_args :
  kind:Parsed_term.binding_kind ->
  decoration:Parsed_term.let_decoration ->
  pat:Parsed_term.pattern ->
  ?arglist:arglist ->
  def:Parsed_term.t ->
  ?cast:Parsed_term.type_annotation ->
  unit ->
  Parsed_term._let

val let_decoration_of_lexer_let_decoration :
  lexer_let_decoration -> Parsed_term.let_decoration

val mk_json_assoc_object_ty :
  pos:pos ->
  Parsed_term.type_annotation * string * string * string ->
  Parsed_term.type_annotation

val mk_source_ty :
  pos:pos ->
  string ->
  Parsed_term.source_annotation ->
  Parsed_term.type_annotation

val mk_named_ty :
  pos:pos ->
  string ->
  Parsed_term.type_annotation option ->
  Parsed_term.type_annotation

val mk :
  ?comments:(pos * Parsed_term.comment) list ->
  ?annotations:Parsed_term.term_annotation list ->
  pos:pos ->
  Parsed_term.parsed_ast ->
  Parsed_term.t

val mk_try :
  ?handler:Parsed_term._try_handler ->
  ?finally_block:Parsed_term.block ->
  body_block:Parsed_term.block ->
  pos:pos ->
  unit ->
  Parsed_term.t

val mk_fun : pos:pos -> arglist -> Parsed_term.t -> Parsed_term.t

val mk_encoder :
  pos:pos -> string -> Parsed_term.encoder_params -> Parsed_term.t

val args_of_json_parse : pos:pos -> (string * 'a) list -> (string * 'a) list
val render_string : pos:pos -> char * string -> string
