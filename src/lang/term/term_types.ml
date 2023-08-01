(** Sets of variables. *)
module Vars = Set.Make (String)

module Methods = struct
  include Methods

  type 'a typ = (string, 'a) t
  type 'a t = 'a typ
end

type ground = ..

type pattern =
  [ `PVar of string list  (** a field *)
  | `PTuple of pattern list  (** a tuple *)
  | `PList of pattern list * string option * pattern list  (** a list *)
  | `PMeth of pattern option * (string * pattern option) list
    (** a value with methods *) ]

(** Documentation for declarations: general documentation, parameters, methods. *)
type 'a term = { mutable t : Type.t; term : 'a; methods : 'a term Methods.t }

type 'a ast_encoder_params =
  (string * [ `Encoder of 'a ast_encoder | `Term of 'a ]) list

and 'a ast_encoder = string * 'a ast_encoder_params

type ('a, 'b) invoke = { invoked : 'a; default : 'a option; meth : 'b }

(* ~l1:x1 .. ?li:(xi=defi) .. *)
type ('a, 'b) func_argument = {
  label : string;
  as_variable : string option;
  default : 'a option;
  typ : 'b;
}

type ('a, 'b) func = {
  mutable free_vars : Vars.t option;
  name : string option;
  arguments : ('a, 'b) func_argument list;
  body : 'a;
}

type 'a app = 'a * (string * 'a) list

type 'a ast =
  [ `Ground of ground
  | `Encoder of 'a ast_encoder
  | `Tuple of 'a list
  | `Null
  | `Open of 'a * 'a
  | `Var of string
  | `Seq of 'a * 'a ]

type t = runtime_ast term

and runtime_ast =
  [ `Let of let_t
  | `List of t list
  | `Cast of t * Type.t
  | `App of t * (string * t) list
  | `Invoke of (t, string) invoke
  | `Fun of (t, Type.t) func
  | t ast ]

and let_t = {
  doc : Doc.Value.t option;
  replace : bool;
  pat : pattern;
  mutable gen : Type.var list;
  def : t;
  body : t;
}
