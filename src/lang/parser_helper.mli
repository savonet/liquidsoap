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

type arglist = (Type.t, Term.t) Term_base.func_argument list

type lexer_let_decoration =
  [ `Eval | `Json_parse | `None | `Recursive | `Replaces | `Yaml_parse ]

type let_decoration =
  [ `Eval
  | `Json_parse of (string * Term.t) list
  | `None
  | `Recursive
  | `Replaces
  | `Yaml_parse ]

type app_list_elem = (string * Term.t) list

type binding = {
  doc : Doc.Value.t option;
  decoration : let_decoration;
  pat : Term.pattern;
  arglist : arglist option;
  def : Term.t;
  cast : Type.t option;
}

type encoder_param =
  string * [ `Term of Term.t | `Encoder of string * encoder_opt ]

and encoder_opt = encoder_param list

type inner_list_item = [ `Ellipsis of Term.t | `Expr of Term.t ]
type inner_list = [ `App of Term.t | `List of Term.t list ]
type let_opt_el = string * Term.t
type record = pos:Lexing.position * Lexing.position -> Term.t -> Term.t
type ty_content_arg = string * string
type ty_content_args = ty_content_arg list
type ty_content = string * ty_content_args
type varlist = [ `List of Term.t list | `App of Term.t ]
type meth_pattern_el = string * Term.pattern option

val mk_ty : ?pos:Pos.t -> Parsed_term.type_annotation -> Type.t

val let_args :
  ?doc:Doc.Value.t ->
  decoration:let_decoration ->
  pat:Term.pattern ->
  ?arglist:arglist ->
  def:Term.t ->
  ?cast:Parsed_term.type_annotation ->
  unit ->
  binding

val let_decoration_of_lexer_let_decoration :
  [< `Eval | `Json_parse | `None | `Recursive | `Replaces | `Yaml_parse ] ->
  [> `Eval
  | `Json_parse of 'a list
  | `None
  | `Recursive
  | `Replaces
  | `Yaml_parse ]

val mk_json_assoc_object_ty :
  pos:Pos.t ->
  Parsed_term.type_annotation * string * string * string ->
  Term.type_annotation

val mk_time_pred : pos:Pos.t -> int * int * int -> Term.t
val during : pos:Pos.t -> int option list -> int * int * int
val between : pos:Pos.t -> int option list -> int option list -> int * int * int
val mk_let : pos:Pos.t -> binding -> Term.t -> Term.t

val mk :
  ?pos:Pos.t ->
  ?t:Type_base.t ->
  ?methods:Term.t Term.Methods.typ ->
  Term.parsed_ast ->
  Term.t

val append_list :
  pos:Pos.t ->
  [< `Ellipsis of Term.t | `Expr of Term.t ] ->
  [< `App of Term.t | `List of Term.t list ] ->
  [> `App of Term.t | `List of Term.t list ]

val mk_list : pos:Pos.t -> [< `App of Term.t | `List of Term.t list ] -> Term.t

val mk_coalesce :
  pos:Pos.t -> default:Term.t -> Term.parsed_ast Term.term -> Term.t

val mk_fun :
  pos:Pos.t -> (Type.t, Term.t) Term.func_argument list -> Term.t -> Term.t

val mk_invoke :
  ?default:Term.t ->
  pos:Pos.t ->
  Term.parsed_ast Term.term ->
  [< `App of string * (string * Term.t) list | `String of string ] ->
  Term.t

val mk_encoder :
  pos:Pos.t -> string -> Term.t Term_base.ast_encoder_params -> Term.t

val args_of :
  only:string list ->
  except:string list ->
  pos:Pos.t ->
  string ->
  (Type_base.t, Term.t) Term.func_argument list

val app_of :
  only:string list ->
  except:string list ->
  pos:Pos.t ->
  string ->
  (string * Term.t) list

val args_of_json_parse : pos:Pos.t -> (string * 'a) list -> (string * 'a) list
