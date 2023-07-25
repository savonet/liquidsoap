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

type arglist = Term.t Term_base.func_argument list

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

val let_args :
  ?doc:Doc.Value.t ->
  decoration:let_decoration ->
  pat:Term.pattern ->
  ?arglist:arglist ->
  def:Term.t ->
  ?cast:Type.t ->
  unit ->
  binding

type encoder_param =
  string * [ `Encoder of string * encoder_opt | `Term of Term.t ]

and encoder_opt = encoder_param list

type inner_list_item = [ `Ellipsis of Term.t | `Expr of Term.t ]
type inner_list = [ `App of Term.t | `List of Term.t list ]
type let_opt_el = string * Term.t
type record = pos:Lexing.position * Lexing.position -> Term.t -> Term.t
type ty_content_arg = string * string
type ty_content_args = ty_content_arg list
type ty_content = string * ty_content_args
type varlist = [ `App of Term.t | `List of Term.t list ]
type meth_pattern_el = string * Term.pattern option

type meth_ty_opt = {
  meth_ty_name : string;
  meth_ty_typ : Type.t;
  meth_ty_optional : bool;
  meth_ty_json_name : string option;
}

val let_decoration_of_lexer_let_decoration :
  [< `Eval | `Json_parse | `None | `Recursive | `Replaces | `Yaml_parse ] ->
  [> `Eval
  | `Json_parse of 'a list
  | `None
  | `Recursive
  | `Replaces
  | `Yaml_parse ]

val string_of_let_decoration :
  [< `Eval | `Json_parse of 'a | `None | `Recursive | `Replaces | `Yaml_parse ] ->
  string

val args_of_json_parse : pos:Pos.t -> (string * 'a) list -> (string * 'a) list

val gen_args_of :
  only:string list ->
  except:string list ->
  pos:Pos.t ->
  (pos:Pos.t -> Type.t -> (string * string * Value.t option) list -> 'a) ->
  string ->
  'a

val args_of :
  only:string list -> except:string list -> pos:Pos.t -> string -> arglist

val app_of :
  only:string list ->
  except:string list ->
  pos:Pos.t ->
  string ->
  (string * Term.t) list

val mk :
  ?pos:Pos.t ->
  ?t:Type.t ->
  ?methods:Term.t Term.Methods.t ->
  Term.parsed_ast ->
  Term.t

val append_list :
  pos:Pos.t ->
  [< `Ellipsis of Term.t | `Expr of Term.t ] ->
  [< `App of Term.t | `List of Term.t list ] ->
  [> `App of Term.t | `List of Term.t list ]

val mk_list : pos:Pos.t -> [< `App of Term.t | `List of Term.t list ] -> Term.t
val mk_fun : pos:Pos.t -> arglist -> Term.t -> Term.t

val mk_app_invoke_default :
  pos:Pos.t -> args:(string * 'a) list -> Term.t -> Term.t

val mk_any : pos:Pos.t -> unit -> Term.t

val mk_invoke_default :
  pos:Pos.t ->
  optional:bool ->
  name:string ->
  Term.t ->
  Term.t Term_base.invoke ->
  Term.t * Term.t

val update_invoke_default :
  pos:Pos.t -> optional:bool -> Term.t -> string -> Term.t -> Term.t

val mk_invoke :
  ?default:Term.t ->
  pos:Pos.t ->
  Term.t ->
  [< `App of string * (string * Term.t) list | `String of string ] ->
  Term.t

val mk_coalesce : pos:Pos.t -> default:Term.t -> Term.t -> Term.t

val mk_let_json_parse :
  pos:Pos.t ->
  (string * Term.t) list * Term.pattern * Term.t * Type.t option ->
  Term.t ->
  Term.t

val mk_let_yaml_parse :
  pos:Pos.t -> Term.pattern * Term.t * Type.t option -> Term.t -> Term.t

val mk_rec_fun :
  pos:Pos.t -> [> `PVar of string list ] -> arglist -> Term.t -> Term.t

val mk_eval :
  pos:Pos.t ->
  Term_base.doc option * Term.pattern * Term.t * Term.t * Type.t option ->
  Term.t

val mk_let : pos:Pos.t -> binding -> Term.t -> Term.t
val mk_encoder : pos:Pos.t -> string -> Term.encoder_params -> Term.t
val time_units : int array
val date : pos:Pos.t -> int option list -> int
val last_index : 'a option list -> int
val precision : 'a option list -> int
val duration : 'a option list -> int
val between : pos:Pos.t -> int option list -> int option list -> int * int * int
val during : pos:Pos.t -> int option list -> int * int * int
val mk_time_pred : pos:Pos.t -> int * int * int -> Term.t

val mk_source_ty :
  pos:Pos.t ->
  string ->
  (string * (string * (string * string) list)) list ->
  extensible:bool ->
  Type.t

val mk_json_assoc_object_ty :
  pos:Pos.t -> Type.t * string * string * string -> Type.t

val mk_ty : pos:Pos.t -> string -> Type.t
val mk_invoke_ty : pos:Pos.t -> Type.t -> string -> Type.t
