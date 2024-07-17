
(** The scripting language. *)

(** {1 Types, values and prototypes} *)

(** Types of objects that can be given as parameters. *)
type parameter_type =
  | Bool_t
  | Int_t
  | String_t
  | Float_t
  | Source_t
  | List_t    of parameter_type
  | Product_t of parameter_type*parameter_type

(** Objects given as parameters. *)
type parameter =
  | Bool    of bool
  | Int     of int
  | String  of string
  | Float   of float
  | Source  of Types.source
  | List    of parameter list
  | Product of parameter*parameter

type default_value = parameter option
type documentation = string option
type prototype  = (string*parameter_type*default_value*documentation) list

(** Candidate instantiation of parameters. *)
type parameters = (string*parameter) list

(** Compiled prototype, needed for type checking. *)
type internal
val internal_of_prototype : prototype -> internal
val check : internal -> parameters -> (string,parameter) Hashtbl.t

(** {1 Exceptions} *)

(** Exception for the user, when type-checking is not enough for validation. *)
exception Invalid_value of string*string

(** Exception for unbound symbols, detected in the parser. *)
exception Unbound of string

(** Exceptions raised by the prototype checker: *)

exception Multiple_definitions of string
exception Missing_definition of string
exception Wrong_type of (string*string*parameter_type)
exception Wrong_label of string

(** {1 Operator registering} *)

(** The operators plug. *)
val operators : (parameters -> Types.source) Plug.plug

(** How to make a documentation for your operator plugins. *)
val to_doc : string -> prototype -> Doc.item

(** {1 Conversion utilities} *)

val to_int : parameter -> int
val to_string : parameter -> string
val to_float : parameter -> float
val to_source : parameter -> Types.source
val to_list : parameter -> parameter list
val to_string_list : parameter -> string list
val to_int_list : parameter -> int list
val to_source_list : parameter -> Types.source list

val parameter_to_string : parameter -> string
val type_to_string : parameter_type -> string

