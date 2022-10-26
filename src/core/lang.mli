(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

(** Values and types of the liquidsoap language. *)

val log : Log.t

(** The type of a value. *)
type t = Liquidsoap_lang.Type.t

type scheme = Liquidsoap_lang.Type.scheme
type regexp

(** {2 Values} *)

(** A typed value. *)
module Ground : sig
  type t = Liquidsoap_lang.Term.Ground.t = ..
  type t += Bool of bool | Int of int | String of string | Float of float

  type content = Liquidsoap_lang.Term.Ground.content = {
    descr : t -> string;
    to_json : pos:Liquidsoap_lang.Pos.t list -> t -> Json.t;
    compare : t -> t -> int;
    typ : (module Liquidsoap_lang.Type.Ground.Custom);
  }

  val register : (t -> bool) -> content -> unit
  val to_string : t -> string
end

type value = Liquidsoap_lang.Value.t = {
  pos : Liquidsoap_lang.Pos.Option.t;
  value : in_value;
}

and env = (string * value) list
and lazy_env = (string * value Lazy.t) list

and in_value = Liquidsoap_lang.Value.in_value =
  | Ground of Ground.t
  | List of value list
  | Tuple of value list
  | Null
  | Meth of string * value * value
  | Ref of value Atomic.t
  | Fun of
      (string * string * value option) list * lazy_env * Liquidsoap_lang.Term.t
  (* A function with given arguments (argument label, argument variable, default
     value), closure and value. *)
  | FFI of (string * string * value option) list * (env -> value)

val demeth : value -> value

(** Iter a function over all sources contained in a value. This only applies to
    statically referenced objects, i.e. it does not explore inside reference
    cells. [on_reference] is used when we encounter a reference cell that may
    contain a source. If not passed, we display a warning log. *)
val iter_sources :
  ?on_reference:(unit -> unit) -> (Source.source -> unit) -> value -> unit

(** {2 Computation} *)

val apply_fun : (?pos:Liquidsoap_lang.Pos.t -> value -> env -> value) ref

(** Multiapply a value to arguments. The argument [t] is the type of the result
   of the application. *)
val apply : value -> env -> value

(** {3 Helpers for registering protocols} *)

val add_protocol :
  syntax:string ->
  doc:string ->
  static:bool ->
  string ->
  Request.resolver ->
  unit

(** {3 Helpers for source builtins} *)

type proto = (string * t * value option * string option) list

(** Add an builtin to the language, high-level version for functions. *)
val add_builtin :
  category:Doc.Value.category ->
  descr:string ->
  ?flags:Doc.Value.flag list ->
  ?meth:(string * Liquidsoap_lang.Type.scheme * string * value) list ->
  ?examples:string list ->
  string ->
  proto ->
  t ->
  (env -> value) ->
  unit

(** Add an builtin to the language, more rudimentary version. *)
val add_builtin_base :
  category:Doc.Value.category ->
  descr:string ->
  ?flags:Doc.Value.flag list ->
  string ->
  in_value ->
  t ->
  unit

(** Declare a new module. *)
val add_module : string -> unit

type 'a operator_method = string * scheme * string * ('a -> value)

(** Add an operator to the language and to the documentation. *)
val add_operator :
  category:Doc.Value.source ->
  descr:string ->
  ?flags:Doc.Value.flag list ->
  ?meth:(< Source.source ; .. > as 'a) operator_method list ->
  string ->
  proto ->
  return_t:t ->
  (env -> 'a) ->
  unit

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
val to_source : value -> Source.source
val to_format : value -> Encoder.format
val to_int : value -> int
val to_int_getter : value -> unit -> int
val to_num : value -> [ `Int of int | `Float of float ]
val to_list : value -> value list
val to_option : value -> value option
val to_valued_option : (value -> 'a) -> value -> 'a option
val to_default_option : default:'a -> (value -> 'a) -> value -> 'a
val to_product : value -> value * value
val to_tuple : value -> value list
val to_ref : value -> value Atomic.t
val to_metadata_list : value -> (string * string) list
val to_metadata : value -> Frame.metadata
val to_string_list : value -> string list
val to_int_list : value -> int list
val to_source_list : value -> Source.source list
val to_fun : value -> (string * value) list -> value
val to_getter : value -> unit -> value
val to_http_transport : value -> Http.transport

(** [assoc x n l] returns the [n]-th [y] such that [(x,y)] is in the list [l].
  * This is useful for retrieving arguments of a function. *)
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
val method_t : t -> (string * scheme * string) list -> t
val list_t : t -> t
val of_list_t : t -> t
val nullable_t : t -> t
val ref_t : t -> t
val error_t : t
val source_t : ?methods:bool -> t -> t
val of_source_t : t -> t
val format_t : t -> t

(* [frame_t base_type fields] returns a frame with [base_type] as
   its base type and [fields] as explicit fields. Equivalent to:
   [base_type.{fields}] *)
val frame_t : t -> t Frame.Fields.t -> t

(* Return a generic frame type with the internal media constraint
   applied. Equivalent to: ['a where 'a is an internal media type] *)
val internal_t : unit -> t

(** [fun_t args r] is the type of a function taking [args] as parameters
  * and returning values of type [r].
  * The elements of [r] are of the form [(b,l,t)] where [b] indicates if
  * the argument is optional, [l] is the label of the argument ([""] means no
  * label) and [t] is the type of the argument. *)
val fun_t : (bool * string * t) list -> t -> t

val univ_t : ?constraints:Liquidsoap_lang.Type.constr list -> unit -> t

(** A shortcut for lists of pairs of strings. *)
val metadata_t : t

(** A getter on an arbitrary type. *)
val getter_t : t -> t

(** Abstract http transport *)
val http_transport_t : t

val unit : value
val int : int -> value
val bool : bool -> value
val float : float -> value
val string : string -> value
val regexp : regexp -> value
val list : value list -> value
val null : value
val error : Runtime_error.runtime_error -> value
val source : Source.source -> value
val product : value -> value -> value
val tuple : value list -> value
val meth : value -> (string * value) list -> value
val record : (string * value) list -> value
val reference : value Atomic.t -> value
val http_transport : Http.transport -> value

(** Build a function from an OCaml function. Items in the prototype indicate
    the label and optional values. *)
val val_fun : (string * string * value option) list -> (env -> value) -> value

(** Build a constant function.
  * It is slightly less opaque and allows the printing of the closure
  * when the constant is ground. *)
val val_cst_fun : (string * value option) list -> value -> value

(** Extract position from the environment. Used inside function execution. *)
val pos : env -> Liquidsoap_lang.Pos.t list

(** Convert a metadata packet to a list associating strings to strings. *)
val metadata : Frame.metadata -> value

(** Raise an error. *)
val raise_error :
  ?bt:Printexc.raw_backtrace ->
  ?message:string ->
  pos:Liquidsoap_lang.Pos.List.t ->
  string ->
  'a

(** Re-raise an error as a runtime error. *)
val raise_as_runtime : bt:Printexc.raw_backtrace -> kind:string -> exn -> 'a

(** Return the process' environment. *)
val environment : unit -> (string * string) list

(** Return an unescaped string description of a regexp, i.e. ^foo/bla$ *)
val descr_of_regexp : regexp -> string

(** Return a string description of a regexp value i.e. r/^foo\/bla$/g *)
val string_of_regexp : regexp -> string

module Regexp : sig
  include Liquidsoap_lang.Regexp.T with type t := regexp

  type sub = Liquidsoap_lang.Regexp.sub = {
    matches : string option list;
    groups : (string * string) list;
  }
end
