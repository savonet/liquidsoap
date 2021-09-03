(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2021 Savonet team

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

val debug : bool ref

(** {1 Positions} *)

type pos = Lexing.position * Lexing.position

val print_single_pos : Lexing.position -> string
val print_pos : ?prefix:string -> pos -> string
val print_pos_opt : ?prefix:string -> pos option -> string
val print_pos_list : ?prefix:string -> pos list -> string

(** {1 Ground types} *)

type variance = Covariant | Contravariant | Invariant
type ground = ..

type ground +=
  | Bool
  | Int
  | String
  | Float
  | Request
  | Format of Frame_content.format

val register_ground_printer : (ground -> string option) -> unit
val print_ground : ground -> string

(** {1 Types} *)

type constr = Num | Ord | Dtools | InternalMedia
type constraints = constr list

val print_constr : constr -> string

type t = { pos : pos option; mutable level : int; mutable descr : descr }

and constructed = { name : string; params : (variance * t) list }

and descr =
  | Constr of constructed
  | Ground of ground
  | Getter of t
  | List of t
  | Tuple of t list
  | Nullable of t
  | Meth of string * scheme * string * t
  | Arrow of (bool * string * t) list * t
  | Cons of string
  | Union of t * t
  | EVar of var
  | Link of t

and var = int * constraints

and scheme = var list * t

val unit : descr
val make : ?pos:pos option -> ?level:int -> descr -> t
val dummy : t
val union : ?pos:pos option -> ?level:int -> t list -> t

(** Remove links in a type: this function should always be called before
    matching on types. *)
val deref : t -> t

val fresh : constraints:constraints -> level:int -> pos:pos option -> t
val fresh_evar : level:int -> pos:pos option -> t
val filter_vars : (t -> bool) -> t -> var list

(** Add a method to a type. *)
val meth :
  ?pos:pos option -> ?level:int -> string -> scheme -> ?doc:string -> t -> t

(** Add a submethod to a type. *)
val meths : ?pos:pos option -> ?level:int -> string list -> scheme -> t -> t

(** Remove all methods in a type. *)
val demeth : t -> t

val split_meths : t -> (string * (scheme * string)) list * t

(** Put the methods of the first type around the second type. *)
val remeth : t -> t -> t

(** Type of a method in a type. *)
val invoke : t -> string -> scheme

(** Type of a submethod in a type. *)
val invokes : t -> string list -> scheme

(** {1 Assignation} *)

exception Occur_check of t * t

(** [occur_check x t] ensures that a variable [x] does not occur in [t]. Raises
    [Occur_check] if it is the case. *)
val occur_check : t -> t -> unit

exception Unsatisfied_constraint of constr * t

(** [bind x t] assigns a value [t] to a variable [x]. *)
val bind : t -> t -> unit

(** {1 Representation of types} *)

type repr =
  [ `Constr of string * (variance * repr) list
  | `Ground of ground
  | `List of repr
  | `Tuple of repr list
  | `Nullable of repr
  | `Meth of string * ((string * constraints) list * repr) * repr
  | `Arrow of (bool * string * repr) list * repr
  | `Getter of repr
  | `Cons of string
  | `Union of repr * repr
  | `EVar of string * constraints (* existential variable *)
  | `UVar of string * constraints (* universal variable *)
  | `Ellipsis (* omitted sub-term *)
  | `Range_Ellipsis (* omitted sub-terms (in a list, e.g. list of args) *) ]

val repr : ?filter_out:(t -> bool) -> ?generalized:var list -> t -> repr
val print_repr : Format.formatter -> repr -> unit

(** {1 Typing errors} *)

type explanation = bool * t * t * repr * repr

exception Type_error of explanation

val print_type_error : (string -> unit) -> explanation -> unit

(** {1 Printing and documentation} *)

val pp_type : Format.formatter -> t -> unit
val pp_type_generalized : var list -> Format.formatter -> t -> unit
val print : ?generalized:var list -> t -> string
val print_scheme : scheme -> string
val doc_of_type : generalized:var list -> t -> Doc.item
val doc_of_meths : (string * (scheme * string)) list -> Doc.item
