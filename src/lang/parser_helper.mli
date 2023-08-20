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

(** Helper functions for the parser. *)

module Term = Parsed_term
module Vars = Term_base.Vars

type arglist = Term.fun_arg list

type lexer_let_decoration =
  [ `Eval | `Json_parse | `None | `Recursive | `Replaces | `Yaml_parse ]

type explicit_binding = [ `Def of Term._let | `Let of Term._let ]
type binding = [ explicit_binding | `Binding of Term._let ]

type encoder_param =
  string * [ `Term of Term.t | `Encoder of string * encoder_opt ]

and encoder_opt = encoder_param list

type let_opt_el = string * Term.t
type record = pos:Lexing.position * Lexing.position -> Term.t -> Term.t
type ty_content_arg = string * string
type ty_content_args = ty_content_arg list
type ty_content = string * ty_content_args
type meth_pattern_el = string * Term.pattern option

val clear_comments : unit -> unit
val append_comment : pos:Pos.t -> string -> unit
val attach_comments : pos:Pos.t -> Term.t -> unit
val mk_ty : ?pos:Pos.t -> Parsed_term.type_annotation -> Type.t

val mk_let :
  pos:Pos.t ->
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
  pos:Pos.t ->
  Parsed_term.type_annotation * string * string * string ->
  Term.type_annotation

val mk :
  ?pos:Pos.t ->
  ?t:Type_base.t ->
  ?methods:Term.t Term.Methods.typ ->
  Term.parsed_ast ->
  Term.t

val mk_fun : pos:Pos.t -> arglist -> Term.t -> Term.t

val mk_encoder :
  pos:Pos.t -> string -> Term.t Term_base.ast_encoder_params -> Term.t

val args_of_json_parse : pos:Pos.t -> (string * 'a) list -> (string * 'a) list
val render_string_ref : (pos:Pos.t -> char * string -> string) ref
val render_string : pos:Pos.t -> char * string -> string
