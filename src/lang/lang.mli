(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

(** Values and types of the liquidsoap language. *)

(** The type of a value. *)
type t = Type.t

type module_name
type scheme = Type.scheme
type regexp = Builtins_regexp.regexp

(** {2 Values} *)

module Custom = Value.Custom
module Methods = Term.Methods

type in_value = Value.in_value
type env = Value.env
type value = Value.t

val demeth : value -> value
val split_meths : value -> (string * value) list * value
val error_module : module_name

(** {2 Computation} *)

(** Multiapply a value to arguments. The argument [t] is the type of the result
    of the application. *)
val apply : ?pos:Pos.t list -> value -> env -> value

(** {3 Helpers for source builtins} *)

type proto = (string * t * value option * string option) list
type 'a meth = { name : string; scheme : scheme; descr : string; value : 'a }

(** Add a builtin to the language, high-level version for functions. *)
val add_builtin :
  category:Doc.Value.category ->
  descr:string ->
  ?flags:Doc.Value.flag list ->
  ?meth:value meth list ->
  ?examples:string list ->
  ?base:module_name ->
  string ->
  proto ->
  t ->
  (env -> value) ->
  module_name

(** Add a builtin value to the language *)
val add_builtin_value :
  category:Doc.Value.category ->
  descr:string ->
  ?flags:Doc.Value.flag list ->
  ?base:module_name ->
  string ->
  value ->
  t ->
  module_name

(** Add a builtin to the language, more rudimentary version. *)
val add_builtin_base :
  category:Doc.Value.category ->
  descr:string ->
  ?flags:Doc.Value.flag list ->
  ?base:module_name ->
  string ->
  Value.in_value ->
  t ->
  module_name

(** Declare a new module. *)
val add_module : ?base:module_name -> string -> module_name

val module_name : module_name -> string

(** {2 Manipulation of values} *)

val to_unit : value -> unit
val to_bool : value -> bool
val to_bool_getter : value -> unit -> bool
val to_string : value -> string
val to_string_getter : value -> unit -> string
val to_regexp : value -> regexp
val to_float : value -> float
val to_float_getter : value -> unit -> float
val to_error : value -> Runtime_error.runtime_error
val to_int : value -> int
val to_int_getter : value -> unit -> int
val to_num : value -> [ `Int of int | `Float of float ]
val to_list : value -> value list
val to_option : value -> value option
val to_valued_option : (value -> 'a) -> value -> 'a option
val to_default_option : default:'a -> (value -> 'a) -> value -> 'a
val to_product : value -> value * value
val to_tuple : value -> value list
val to_ref : value -> (unit -> value) * (value -> unit)

val to_valued_ref :
  (value -> 'a) -> ('a -> value) -> value -> (unit -> 'a) * ('a -> unit)

val to_string_list : value -> string list
val to_int_list : value -> int list
val to_fun : value -> (string * value) list -> value
val to_getter : value -> unit -> value

(** [assoc x n l] returns the [n]-th [y] such that [(x,y)] is in the list [l].
    This is useful for retrieving arguments of a function. *)
val assoc : 'a -> int -> ('a * 'b) list -> 'b

val int_t : t
val unit_t : t
val float_t : t
val bool_t : t
val string_t : t
val regexp_t : t
val product_t : t -> t -> t
val of_product_t : t -> t * t
val tuple_t : t list -> t
val of_tuple_t : t -> t list
val record_t : (string * t) list -> t
val optional_record_t : (string * t) list -> t
val method_t : t -> (string * scheme * string) list -> t
val optional_method_t : t -> (string * scheme * string) list -> t
val list_t : t -> t
val of_list_t : t -> t
val nullable_t : t -> t
val ref_t : t -> t
val error_t : t
val error_meths_t : t

(** [fun_t args r] is the type of a function taking [args] as parameters and
    returning values of type [r]. The elements of [r] are of the form [(b,l,t)]
    where [b] indicates if the argument is optional, [l] is the label of the
    argument ([""] means no label) and [t] is the type of the argument. *)
val fun_t : (bool * string * t) list -> t -> t

val univ_t : ?constraints:Type.constr list -> unit -> t

(** A getter on an arbitrary type. *)
val getter_t : t -> t

val unit : value
val int : int -> value
val octal_int : int -> value
val hex_int : int -> value
val bool : bool -> value
val float : float -> value
val string : string -> value
val regexp : regexp -> value
val list : value list -> value
val null : value
val error : Runtime_error.runtime_error -> value
val product : value -> value -> value
val tuple : value list -> value
val meth : value -> (string * value) list -> value
val record : (string * value) list -> value
val reference : (unit -> value) -> (value -> unit) -> value

(** Build a function from an OCaml function. Items in the prototype indicate the
    label and optional values. Second string value is used when renaming
    argument name, e.g. `fun (foo=_, ...) -> ` *)
val val_fun : (string * string * value option) list -> (env -> value) -> value

(** Build a function from a term. *)
val term_fun : (string * string * value option) list -> Term.t -> value

(** Build a constant function. It is slightly less opaque and allows the
    printing of the closure when the constant is ground. *)
val val_cst_fun : (string * value option) list -> value -> value

(** Extract position from the environment. Used inside function execution. *)
val pos : env -> Pos.t list

(** Raise an error. *)
val raise_error :
  ?bt:Printexc.raw_backtrace ->
  ?message:string ->
  pos:Pos.List.t ->
  string ->
  'a

(** Convert an exception into a runtime error. *)
val runtime_error_of_exception :
  bt:Printexc.raw_backtrace -> kind:string -> exn -> Runtime_error.runtime_error

(** Re-raise an error as a runtime error. *)
val raise_as_runtime : bt:Printexc.raw_backtrace -> kind:string -> exn -> 'a

(** Return the process' environment. *)
val environment : unit -> (string * string) list

(** Return an unescaped string description of a regexp, i.e. ^foo/bla$ *)
val descr_of_regexp : regexp -> string

(** Return a string description of a regexp value i.e. r/^foo\/bla$/g *)
val string_of_regexp : regexp -> string

type stdlib = [ `Disabled | `If_present | `Force | `Override of string ]

(** Type a term, possibly returning the cached term instead. *)
val type_term :
  ?name:string ->
  ?cache:bool ->
  ?trim:bool ->
  ?deprecated:bool ->
  ?ty:t ->
  stdlib:stdlib ->
  parsed_term:Parsed_term.t ->
  Term.t ->
  Term.t

(** Evaluate a term. *)
val eval :
  ?toplevel:bool ->
  ?typecheck:bool ->
  ?cache:bool ->
  ?deprecated:bool ->
  ?ty:t ->
  ?name:string ->
  stdlib:stdlib ->
  string ->
  value
