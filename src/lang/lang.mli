(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2007 Savonet team

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

(** {2 Kinds} *)

(** The type of a value. *)
type kind

(** Get a string representation of a kind. *)
val print_kind : kind -> string

(** {2 Values} *)

(** A typed value. *)
type value = { mutable t : kind ; value : in_value }
and  in_value =
  | Unit
  | Bool    of bool
  | Int     of int
  | String  of string
  | Float   of float
  | Source  of Source.source (** A source. *)
  | Request of Request.t option
  | List    of value list
  | Product of value * value (** A couple. *)
  | Let     of (Doc.item*(string*string)list) * string * value * value
  | Var     of string (** A variable. *)
  | Seq     of value * value
      (** Sequential composition of two values (the first one should be of type [Unit]. *)
  | App     of value * (string * value) list
      (** Multiapplication of a function to arguments. *)
  | Fun     of (string*string*kind*value option) list * value
      (** fun ~l1:x1 .. ?li:(xi=defi) .. -> body is represented by
        * [Fun ((l1,x1,None)..(li,xi,Some defi)..),body] *)
  | FFI     of (string*value option) list *
               (string*value) list *
               ((string*value) list -> in_value)
               (** An FFI (foreign function call) to a caml function. The first
                 * list is the list of non-instantiated arguments and the second
                 * one of already instantiated arguments. Finally, the function
                 * will be called with the list of its arguments as argument. *)

(** Get a string representation of a value. *)
val print_value : value -> string
(** Iter a function over all sources contained in a value. *)
val iter_sources : (Source.source -> unit) -> value -> unit

(** {2 Computation} *)

(** Multiapply a value to arguments. *)
val apply : value -> (string * value) list -> value
(** Evaluate (= beta-reduce) a value. *)
val eval : value -> value

(** {2 Type-checking/inference} *)

(** Check that a value is typable. *)
val check : value -> unit

(** {2 Language builtins} *)

val builtins : value Plug.plug

(** {3 Helpers for source builtins} *)

type parameters = (string * value) list
(** Type of the prototype of an operator. *)
type operator_proto = (string * kind * value option * string option) list
(** Category of an operator. *)
type category =
  | Input (** Input. *)
  | Output (** Output. *)
  | TrackProcessing (** Operations on tracks (e.g. mixing, etc.). *)
  | SoundProcessing (** Operations on sound (e.g. compression, etc.). *)
  | Visualization (** Visializations of the sound. *)
type doc_flag =
  | Hidden (** Don't list the plugin in the documentation. *)
  | Deprecated (** The plugin should not be used. *)
  | Experimental (** The plugin should not considered as stable. *)
(** Get a string representation of a [doc_flag]. *)
val string_of_flag : doc_flag -> string
(** Add an operator to the language and to the documentation. *)
val add_operator :
  category:category ->
  descr:string ->
  ?flags:doc_flag list ->
  string ->
  (string * kind * value option * string option) list ->
  ((string * value) list -> Source.source) -> unit

(** {2 Bindings handling, toplevel definitions} *)

val bindings : (string * value) list ref
val init_bindings : unit -> unit
val eval_toplevel : value -> value

(** {2 Manipulation of values} *)

val to_bool : value -> bool
val to_string : value -> string
val to_float : value -> float
val to_float_getter : value -> unit -> float
val to_source : value -> Source.source
val to_request : value -> Request.t option
val to_int : value -> int
val to_list : value -> value list
val to_product : value -> value * value
val to_string_list : value -> string list
val to_int_list : value -> int list
val to_source_list : value -> Source.source list

(** [assoc x n l] returns the [n]-th [y] such that [(x,y)] is in the list [l].
  * This is useful for retreiving arguments of a function. *)
val assoc : 'a -> int -> ('a * 'b) list -> 'b

val int_t      : kind
val unit_t     : kind
val float_t    : kind
val bool_t     : kind
val string_t   : kind
val source_t   : kind
val request_t  : kind
val list_t     : kind -> kind
val product_t  : kind -> kind -> kind
(** [fun_t args r] is the type of a function taking [args] as parameters
  * and returning values of type [r].
  * The elements of [r] are of the form [(b,l,t)] where [b] indicates if
  * the argument is optional, [l] is the label of the argument ([""] means no
  * label) and [t] is the type of the argument. *)
val fun_t      : (bool * string * kind) list -> kind -> kind
val univ_t     : ?constraints:Lang_types.constraints -> int -> kind
val metadata_t : kind
(** A float getter. The argument is the number of the universal type parameter
  * (should be >= 1). *)
val float_getter_t : int -> kind

val unit : value
val int : int -> value
val bool : bool -> value
val float : float -> value
val string : string -> value
val list : value list -> value
val source : Source.source -> value
val request : Request.t option -> value
val product : value -> value -> value
val val_fun : (string * string * kind * value option) list -> value -> value
val ffi :
  (string * value option) list ->
  ((string * value) list -> in_value) -> value
val var : string -> value
val app : value -> (string * value) list -> value
val metadata : Frame.metadata -> value

(** {2 Errors raised by other modules} *)

exception Unbound of string
exception Invalid_value of value * string

(** {2 Misc} *)

val debug : bool

(** {2 Main script evaluation} *)

(** Load the external libraries. *)
val load_libs       : unit -> unit
(** Evaluate a script from an [in_channel]. *)
val from_in_channel : in_channel -> unit
(** Evaluate a script from a file. *)
val from_file       : string -> unit
(** Evaluate a script from a string. *)
val from_string     : string -> unit
