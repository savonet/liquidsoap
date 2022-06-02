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

(** The type of a value. *)
type t = Type.t

type scheme = Type.scheme

(** {2 Values} *)

(** A typed value. *)
module Ground : sig
  type t = Term.Ground.t = ..
  type t += Bool of bool | Int of int | String of string | Float of float

  type content = Term.Ground.content = {
    descr : t -> string;
    to_json : t -> Json.t;
    compare : t -> t -> int;
    typ : Type.ground;
  }

  val register : (t -> bool) -> content -> unit
  val to_string : t -> string
end

type value = Value.t = { pos : Pos.Option.t; value : in_value }

and env = (string * value) list

and lazy_env = (string * value Lazy.t) list

and in_value = Value.in_value =
  | Ground of Ground.t
  | List of value list
  | Tuple of value list
  | Null
  | Meth of string * value * value
  | Ref of value ref
  | Fun of (string * string * value option) list * lazy_env * Term.t
  (* A function with given arguments (argument label, argument variable, default
     value), closure and value. *)
  | FFI of (string * string * value option) list * (env -> value)

val demeth : value -> value

(** {2 Computation} *)

val apply_fun : (?pos:Pos.t -> value -> env -> value) ref

(** Multiapply a value to arguments. The argument [t] is the type of the result
   of the application. *)
val apply : value -> env -> value

(** {3 Helpers for source builtins} *)

type proto = (string * t * value option * string option) list

(** Add an builtin to the language, high-level version for functions. *)
val add_builtin :
  category:Documentation.category ->
  descr:string ->
  ?flags:Documentation.flag list ->
  ?meth:(string * Type.scheme * string * value) list ->
  ?examples:string list ->
  string ->
  proto ->
  t ->
  (env -> value) ->
  unit

(** Add an builtin to the language, more rudimentary version. *)
val add_builtin_base :
  category:Documentation.category ->
  descr:string ->
  ?flags:Documentation.flag list ->
  string ->
  in_value ->
  t ->
  unit

(** Declare a new module. *)
val add_module : string -> unit

val empty : Frame.content_kind
val any : Frame.content_kind

(** Any internal stream type. *)
val internal : Frame.content_kind

(* Conversion to format *)
val kind_type_of_kind_format : Frame.content_kind -> t

(** {2 Manipulation of values} *)

val to_unit : value -> unit
val to_bool : value -> bool
val to_bool_getter : value -> unit -> bool
val to_string : value -> string
val to_string_getter : value -> unit -> string
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
val to_ref : value -> value ref
val to_metadata_list : value -> (string * string) list
val to_metadata : value -> Frame.metadata
val to_string_list : value -> string list
val to_int_list : value -> int list
val to_fun : value -> (string * value) list -> value
val to_getter : value -> unit -> value

(** [assoc x n l] returns the [n]-th [y] such that [(x,y)] is in the list [l].
  * This is useful for retrieving arguments of a function. *)
val assoc : 'a -> int -> ('a * 'b) list -> 'b

val int_t : t
val unit_t : t
val float_t : t
val bool_t : t
val string_t : t
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
val source_t : t -> t
val of_source_t : t -> t
val format_t : t -> t
val kind_t : Frame.kind -> t
val kind_none_t : t
val frame_kind_t : audio:t -> video:t -> midi:t -> t
val of_frame_kind_t : t -> t Frame.fields

(** [fun_t args r] is the type of a function taking [args] as parameters
  * and returning values of type [r].
  * The elements of [r] are of the form [(b,l,t)] where [b] indicates if
  * the argument is optional, [l] is the label of the argument ([""] means no
  * label) and [t] is the type of the argument. *)
val fun_t : (bool * string * t) list -> t -> t

val univ_t : ?constraints:Type.constraints -> unit -> t

(** A shortcut for lists of pairs of strings. *)
val metadata_t : t

(** A getter on an arbitrary type. *)
val getter_t : t -> t

val unit : value
val int : int -> value
val bool : bool -> value
val float : float -> value
val string : string -> value
val list : value list -> value
val null : value
val error : Runtime_error.runtime_error -> value
val product : value -> value -> value
val tuple : value list -> value
val meth : value -> (string * value) list -> value
val record : (string * value) list -> value
val reference : value ref -> value

(** Build a function from an OCaml function. Items in the prototype indicate
    the label and optional values. *)
val val_fun : (string * string * value option) list -> (env -> value) -> value

(** Build a constant function.
  * It is slightly less opaque and allows the printing of the closure
  * when the constant is ground. *)
val val_cst_fun : (string * value option) list -> value -> value

(** Convert a metadata packet to a list associating strings to strings. *)
val metadata : Frame.metadata -> value

(** Raise an error. *)
val raise_error :
  ?bt:Printexc.raw_backtrace ->
  ?pos:Pos.List.t ->
  ?message:string ->
  string ->
  'a

(** Re-raise an error as a runtime error. *)
val raise_as_runtime : bt:Printexc.raw_backtrace -> kind:string -> exn -> 'a
