open Term_hash

(** Sets of variables. *)
module Vars = Set.Make (String)

module Methods = struct
  include Methods

  type 'a typ = (string, 'a) t
  type 'a t = 'a typ
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

type flags = int

let octal_int = 0b1
let hex_int = 0b10

type 'a term = {
  mutable t : Type.t;
  term : 'a;
  flags : flags;
  mutable methods : 'a term Methods.t;
}

let has_flag { flags } flag = flags land flag <> 0

(* ~l1:x1 .. ?li:(xi=defi) .. *)
type ('a, 'b) func_argument = {
  label : string;
  as_variable : string option;
  default : 'a option;
  typ : 'b;
}
[@@deriving hash]

type ('a, 'b) func = {
  mutable free_vars : Vars.t option;
  name : string option;
  arguments : ('a, 'b) func_argument list;
  body : 'a;
}

type 'a app = 'a * (string * 'a) list

type ('a, 'b) cast = { cast : 'a; mutable typ : 'b [@compare.ignore] }
[@@deriving hash]

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

type value

type 'a runtime_ast =
  [ `Int of int
  | `Float of float
  | `String of string
  | `Bool of bool
  | `Let of 'a let_t
  | `List of 'a list
  | `Value of value
  | `App of 'a * (string * 'a) list
  | `Invoke of 'a invoke
  | `Hide of 'a * string list
  | `Encoder of 'a encoder
  | `Fun of ('a, Type.t) func ]

type t = ast term
and ast = [ (t, Type.t) common_ast | t runtime_ast ]

let rec map_encoder fn (lbl, params) = (lbl, map_encoder_params fn params)

and map_encoder_params fn =
  List.map (function
    | `Anonymous _ as v -> v
    | `Encoder enc -> `Encoder (map_encoder fn enc)
    | `Labelled (lbl, arg) -> `Labelled (lbl, fn arg))

let rec map_ast map_term = function
  | `Custom _ as ast -> ast
  | `Tuple l -> `Tuple (List.map map_term l)
  | `Null -> `Null
  | `Int _ as ast -> ast
  | `Float _ as ast -> ast
  | `Bool _ as ast -> ast
  | `String _ as ast -> ast
  | `Value _ as ast -> ast
  | `Open (t, t') -> `Open (map_term t, map_term t')
  | `Hide (t, l) -> `Hide (map_term t, l)
  | `Var _ as ast -> ast
  | `Seq (t, t') -> `Seq (map_term t, map_term t')
  | `Let ({ def; body } as _let) ->
      `Let { _let with def = map_term def; body = map_term body }
  | `List l -> `List (List.map map_term l)
  | `Cast ({ cast } as c) -> `Cast { c with cast = map_term cast }
  | `App (t, l) ->
      `App (map_term t, List.map (fun (lbl, t) -> (lbl, map_term t)) l)
  | `Invoke ({ invoked; invoke_default } as invoke) ->
      `Invoke
        {
          invoke with
          invoked = map_term invoked;
          invoke_default = Option.map map_term invoke_default;
        }
  | `Encoder enc -> `Encoder (map_encoder map_term enc)
  | `Fun ({ body; arguments } as func) ->
      `Fun
        {
          func with
          body = map_term body;
          arguments =
            List.map
              (fun ({ default } as arg) ->
                { arg with default = Option.map map_term default })
              arguments;
        }

and map_term fn tm =
  {
    tm with
    term = map_ast (map_term fn) tm.term;
    methods = Methods.map (map_term fn) tm.methods;
  }
