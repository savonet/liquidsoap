open Term_hash

(** Sets of variables. *)
module Vars = Set.Make (String)

module Methods = struct
  include Methods

  type nonrec 'a t = (string, 'a) t [@@deriving hash]
end

type custom [@@deriving hash]

type custom_handler = {
  name : string;
  to_string : custom -> string; [@hash.ignore]
  to_json : pos:Pos.t list -> custom -> Json.t; [@hash.ignore]
  compare : custom -> custom -> int; [@hash.ignore]
  typ : Type.t; [@hash.ignore]
}
[@@deriving hash]

type custom_term = { value : custom; handler : custom_handler }
[@@deriving hash]

type 'a term = {
  t : Type.t;
  term : 'a;
  flags : Flags.flags;
  methods : 'a term Methods.t;
}

let has_flag { flags } flag = Flags.has flags flag

(* ~l1:x1 .. ?li:(xi=defi) .. *)
type ('a, 'b) func_argument = {
  label : string;
  as_variable : string option;
  default : 'a option;
  typ : 'b; [@hash.ignore]
  pos : Pos.t option; [@hash.ignore]
}
[@@deriving hash]

type ('a, 'b) func = {
  mutable free_vars : Vars.t option;
  name : string option;
  arguments : ('a, 'b) func_argument list;
  body : 'a;
}

type 'a app = 'a * (string * 'a) list
type ('a, 'b) cast = { cast : 'a; typ : 'b } [@@deriving hash]

type ('a, 'b) common_ast =
  [ `Custom of custom_term
  | `Tuple of 'a list
  | `Null
  | `Cast of ('a, 'b) cast
  | `Open of 'a * 'a
  | `Var of string
  | `Seq of 'a * 'a ]
[@@deriving hash]

type 'a invoke = { invoked : 'a; invoke_default : 'a option; meth : string }

type 'a encoder_params =
  [ `Anonymous of string | `Encoder of 'a encoder | `Labelled of string * 'a ]
  list

and 'a encoder = string * 'a encoder_params

type pattern = [ `PVar of string list | `PTuple of string list ]

type 'a let_t = {
  doc : Doc.Value.t option;
  replace : bool;
  pat : pattern;
  mutable gen : Type.var list;
  def : 'a;
  body : 'a;
}

type cached_env = { var_name : int; var_id : int; env : Typing.env }

type 'a runtime_ast =
  [ `Int of int
  | `Cache_env of cached_env ref
  | `Float of float
  | `String of string
  | `Bool of bool
  | `Let of 'a let_t
  | `List of 'a list
  | `App of 'a * (string * 'a) list
  | `Invoke of 'a invoke
  | `Hide of 'a * string list
  | `Encoder of 'a encoder
  | `Fun of ('a, Type.t) func ]

type t = ast term
and ast = [ (t, Type.t) common_ast | t runtime_ast ]
