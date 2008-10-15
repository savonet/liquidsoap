type pos = (Lexing.position * Lexing.position)
val print_single_pos : pos -> string
val print_pos : ?prefix:string -> pos -> string

type ground = Unit | Bool | Int | String | Float | Source | Request
val print_ground : ground -> string

type constr = Num | Ord | Getter of ground | Dtools
type constraints = constr list
val print_constr : constr -> string

type t = { pos : pos option; mutable level : int; mutable descr : descr; }
and descr =
    Ground of ground
  | List of t
  | Product of t * t
  | Arrow of (bool * string * t) list * t
  | EVar of int * constraints
  | UVar of int * constraints
  | Link of t
and ty_range =
    String_r of string
  | Int_r of int * int
  | Float_r of float * float
val make : ?pos:pos option -> ?level:int -> descr -> t
val dummy : t
val print : t -> string

exception Occur_check of t*t
val occur_check : t -> t -> unit

exception Unsatisfied_constraint of constr*t
val bind : t -> t -> unit
val deref : t -> t
val instantiate : level:int -> t -> t
val generalize : level:int -> t -> unit

type trace_item = Item of t*t | Flip
exception Error of trace_item list
val ( <: ) : t -> t -> unit
val ( >: ) : t -> t -> unit

val fresh_evar : level:int -> pos:pos option -> t
