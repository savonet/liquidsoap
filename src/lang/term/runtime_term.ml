(** Sets of variables. *)
module Vars = Set.Make (String)

module Methods = struct
  include Methods

  type 'a typ = (string, 'a) t
  type 'a t = 'a typ
end

type custom = ..

type custom_handler = {
  to_string : custom -> string;
  to_json : pos:Pos.t list -> custom -> Json.t;
  compare : custom -> custom -> int;
  typ : (module Type.Custom.Implementation);
}

type custom_term = { value : custom; handler : custom_handler }

type pattern =
  [ `PVar of string list  (** a field *)
  | `PTuple of pattern list  (** a tuple *)
  | `PList of pattern list * string option * pattern list  (** a list *)
  | `PMeth of pattern option * (string * meth_term_default) list
    (** a value with methods *) ]

and meth_term_default = [ `Nullable | `Pattern of pattern | `None ]

type flags = int

let octal_int = 0b1
let hex_int = 0b10

type 'a term = {
  mutable t : Type.t;
  term : 'a;
  flags : flags;
  methods : 'a term Methods.t;
  id : int;
}

let has_flag { flags } flag = flags land flag <> 0

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

type 'a common_ast =
  [ `Custom of custom_term
  | `Tuple of 'a list
  | `Null
  | `Open of 'a * 'a
  | `Var of string
  | `Seq of 'a * 'a ]

type 'a invoke = { invoked : 'a; invoke_default : 'a option; meth : string }

type 'a encoder_params =
  [ `Anonymous of string | `Encoder of 'a encoder | `Labelled of string * 'a ]
  list

and 'a encoder = string * 'a encoder_params

type 'a let_t = {
  doc : Doc.Value.t option;
  replace : bool;
  pat : pattern;
  mutable gen : Type.var list;
  def : 'a;
  body : 'a;
}

type 'a runtime_ast =
  [ `Int of int
  | `Float of float
  | `String of string
  | `Bool of bool
  | `Let of 'a let_t
  | `List of 'a list
  | `Cast of 'a * Type.t
  | `App of 'a * (string * 'a) list
  | `Invoke of 'a invoke
  | `Encoder of 'a encoder
  | `Fun of ('a, Type.t) func ]

type t = ast term
and ast = [ t common_ast | t runtime_ast ]
