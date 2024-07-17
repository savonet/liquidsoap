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

(** {1 Kinds} *)

type kind = in_kind ref
and in_kind =
    Unit_t
  | Bool_t
  | Int_t
  | String_t
  | Float_t
  | Source_t
  | Request_t
  | List_t of kind
  | Product_t of kind * kind
  | Fun_t of (bool * string * kind) list * kind
  | Unknown of int
  | Ref of kind

val dummy_kind : in_kind ref
val fresh_kindvar : unit -> in_kind ref
val print_kind : ?par:bool -> kind -> string

(** {1 Locations} *)

type pos = Lexing.position * Lexing.position

val dummy_pos : Lexing.position * Lexing.position
val print_single_pos : Lexing.position -> string
val print_pos : Lexing.position * Lexing.position -> string

(** {1 Values} *)

type value = { pos : pos; kind : kind; value : in_value; }
and in_value =
    Unit
  | Bool of bool
  | Int of int
  | String of string
  | Float of float
  | Source of Source.source
  | Request of Request.t option
  | List of value list
  | Product of value * value
  | Let of string * value * value
  | Var of string
  | Seq of value * value
  | App of value * (string * value) list
  | Fun of (string * string * kind * value option) list * value
  | FFI of (string * value option) list * (string * value) list *
      ((string * value) list -> in_value)

val dummy_value : value

val print_value : value -> string
val iter_sources : (Source.source -> unit) -> value -> unit

(** {1 Computation} *)

val apply : ?pos:pos -> ?kind:kind -> value -> (string * value) list -> value
val eval : value -> value

(** {1 Type-checking/inference} *)

exception Unification_failed of pos * kind * kind
exception In_unification_failed
exception Wrong_label of pos * string
exception In_wrong_label of string
exception No_common_lbl

val unify : value -> kind -> unit
val check : value -> unit

(** {1 Language builtins} *)

val builtins : value Plug.plug

(** {2 Helpers for source builtins} *)

type parameters = (string * value) list
type operator_proto = (string * kind * value option * string option) list
type category =
    Input
  | Output
  | TrackProcessing
  | SoundProcessing
  | Visualization
type doc_flag =
  | Hidden (** don't list the plugin in the documentation *)
  | Deprecated
  | Experimental
val string_of_flag : doc_flag -> string
val add_operator :
  category:category ->
  descr:string ->
  ?flags:doc_flag list ->
  string ->
  (string * kind * value option * string option) list ->
  ((string * value) list -> Source.source) -> unit

(** {1 Bindings handling, toplevel definitions} *)

val bindings : (string * value) list ref
val init_bindings : unit -> unit
val eval_toplevel : value -> value

(** {1 Manipulation of values} *)

val to_bool : value -> bool
val to_string : value -> string
val to_float : value -> float
val to_source : value -> Source.source
val to_request : value -> Request.t option
val to_int : value -> int
val to_list : value -> value list
val to_product : value -> value * value
val to_string_list : value -> string list
val to_int_list : value -> int list
val to_source_list : value -> Source.source list

val assoc : 'a -> int -> ('a * 'b) list -> 'b

val int_t : in_kind ref
val unit_t : in_kind ref
val float_t : in_kind ref
val bool_t : in_kind ref
val string_t : in_kind ref
val source_t : in_kind ref
val request_t : in_kind ref
val list_t : kind -> in_kind ref
val product_t : kind -> kind -> in_kind ref
val fun_t : (bool * string * kind) list -> kind -> in_kind ref

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

(** {1 Errors raised by other modules} *)

exception Unbound of string
exception Invalid_value of value * string

(** {1 Misc} *)

val debug : bool
