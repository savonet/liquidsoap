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
  | `PMeth of pattern option * (string * meth_term_default) list
    (** a value with methods *) ]

and meth_term_default = [ `Nullable | `Pattern of pattern | `None ]

type 'a term = { mutable t : Type.t; term : 'a; methods : 'a term Methods.t }

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
  | `Invoke of invoke
  | `Encoder of encoder
  | `Fun of (t, Type.t) func
  | t ast ]

and invoke = { invoked : t; invoke_default : t option; meth : string }

and encoder_params =
  [ `Anonymous of string | `Encoder of encoder | `Labelled of string * t ] list

and encoder = string * encoder_params

and let_t = {
  doc : Doc.Value.t option;
  replace : bool;
  pat : pattern;
  mutable gen : Type.var list;
  def : t;
  body : t;
}
