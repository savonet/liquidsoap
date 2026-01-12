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

module Term = Parsed_term
module Vars = Term_base.Vars

type arglist = Term.fun_arg list
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

type explicit_binding = [ `Def of Term._let | `Let of Term._let ]
type binding = [ explicit_binding | `Binding of Term._let ]
type let_opt_el = string * Term.t

val clear_comments : unit -> unit
val append_comment : pos:pos -> string -> unit
val attach_comments : Term.t -> unit

val mk_let :
  pos:pos ->
  [< `Binding of Term._let | `Def of Term._let | `Let of Term._let ] ->
  Term.t ->
  Term.t

val let_args :
  decoration:Term.let_decoration ->
  pat:Term.pattern ->
  ?arglist:arglist ->
  def:Term.t ->
  ?cast:Term.type_annotation ->
  unit ->
  Term._let

val let_decoration_of_lexer_let_decoration :
  lexer_let_decoration -> Term.let_decoration

val mk_json_assoc_object_ty :
  pos:pos ->
  Parsed_term.type_annotation * string * string * string ->
  Term.type_annotation

val mk :
  ?comments:(pos * Parsed_term.comment) list ->
  ?annotations:Parsed_term.term_annotation list ->
  pos:pos ->
  Term.parsed_ast ->
  Term.t

val mk_try :
  ?ensure:Term.t ->
  ?handler:Term.t ->
  ?errors_list:Term.t ->
  variable:string ->
  body:Term.t ->
  pos:pos ->
  unit ->
  Term.t

val mk_fun : pos:pos -> arglist -> Term.t -> Term.t
val mk_encoder : pos:pos -> string -> Term.encoder_params -> Term.t
val args_of_json_parse : pos:pos -> (string * 'a) list -> (string * 'a) list
val render_string_ref : (pos:pos -> char * string -> string) ref
val render_string : pos:pos -> char * string -> string
