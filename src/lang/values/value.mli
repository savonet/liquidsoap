(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

(** Runtime values: what a liquidsoap script evaluates to.

    Every constructor carries its source position (for error messages), the
    methods attached to it, and for most of them a set of {!Flags.flags}
    recording how it was written in the source. Values are compared and printed
    through {!compare} and {!to_string} rather than structurally, because
    functions and custom values need their own notion of both. *)

module Custom = Term.Custom
module Methods = Runtime_term.Methods

type env = (string * t) list

(** Methods computed on demand rather than stored, used by sources whose method
    set depends on their content type. *)
and dynamic_methods = {
  hidden_methods : string list;
  methods : string -> t option;
}

and t =
  | Int of {
      pos : Pos.Option.t;
      value : int;
      methods : t Methods.t;
      mutable flags : Flags.flags;
    }
  | Float of { pos : Pos.Option.t; value : float; methods : t Methods.t }
  | String of { pos : Pos.Option.t; value : string; methods : t Methods.t }
  | Bool of { pos : Pos.Option.t; value : bool; methods : t Methods.t }
  | Null of { pos : Pos.Option.t; methods : t Methods.t }
  | Custom of {
      pos : Pos.Option.t;
      value : Custom.t;
      methods : t Methods.t;
      dynamic_methods : dynamic_methods option;
      mutable flags : Flags.flags;
    }
  | List of {
      pos : Pos.Option.t;
      value : t list;
      methods : t Methods.t;
      mutable flags : Flags.flags;
    }
  | Tuple of {
      pos : Pos.Option.t;
      value : t list;
      methods : t Methods.t;
      mutable flags : Flags.flags;
    }
  | Fun of {
      id : int;
      pos : Pos.Option.t;
      fun_args : (string * string * t option) list;
      fun_env : env;
      fun_body : Term.t;
      methods : t Methods.t;
      mutable flags : Flags.flags;
    }  (** A function defined in a script. *)
  | FFI of {
      id : int;
      pos : Pos.Option.t;
      ffi_args : (string * string * t option) list;
      mutable ffi_fn : env -> t;
      methods : t Methods.t;
      mutable flags : Flags.flags;
    }  (** A builtin, i.e. a function implemented in OCaml. *)

val hash_fold_t : Term_hash.state -> t -> Term_hash.state

type fun_v = {
  fun_args : (string * string * t option) list;
  fun_env : env;
  fun_body : Term.t;
}

type ffi = { ffi_args : (string * string * t option) list; ffi_fn : env -> t }

(** A value without its position, methods or flags: what {!make} takes. *)
type in_value =
  [ `Bool of bool
  | `Custom of Custom.t
  | `FFI of ffi
  | `Float of float
  | `Fun of fun_v
  | `Int of int
  | `List of t list
  | `Null
  | `String of string
  | `Tuple of t list ]

val make :
  ?pos:Pos.t -> ?methods:t Methods.t -> ?flags:Flags.flags -> in_value -> t

val methods : t -> t Methods.t
val map_methods : t -> (t Methods.t -> t Methods.t) -> t
val pos : t -> Pos.Option.t
val set_pos : t -> Pos.Option.t -> t
val has_flag : t -> Flags.flag -> bool
val add_flag : t -> Flags.flag -> unit
val remove_flag : t -> Flags.flag -> unit
val unit : [> `Tuple of 'a list ]
val is_unit : t -> bool

(** Render an integer the way it was written, i.e. honouring the hexadecimal and
    octal flags. *)
val string_of_int_value : flags:Flags.flags -> int -> string

val to_string : t -> string

(** Retrieve a method. Raises [Not_found] if it is absent. *)
val invoke : t -> string -> t

(** [invokes v ["a"; "b"]] is [v.a.b]. *)
val invokes : t -> string list -> t

(** Drop all methods. *)
val demeth : t -> t

(** [remeth v v'] is [v] with the methods of [v']. *)
val remeth : t -> t -> t

val split_meths : t -> (string * t) list * t

(** Comparison used by the language's `==` and by list and sort operations.
    Functions are compared by identity. *)
val compare : t -> t -> int

(** An OCaml type carried by liquidsoap values, e.g. a source or a request. *)
module type Custom = sig
  type content

  val t : Type.t
  val to_custom : content -> Term.Custom.t
  val of_custom : Term.Custom.t -> content
  val is_custom : Term.Custom.t -> bool
  val to_term : content -> Term.t
  val of_term : Term.t -> content
  val to_value : ?pos:Pos.t -> content -> t
  val of_value : t -> content
  val is_value : t -> bool
end

module type CustomDef = Term.CustomDef

module MkCustomFromTerm (Term : Term.Custom) :
  Custom with type content = Term.content

module MkCustom (Def : CustomDef) : Custom with type content = Def.content

(** A liquidsoap type, as a value. Used by the parsers (`json.parse` and
    friends) which need the expected type at runtime. *)
module RuntimeType : Custom with type content = Type.t
