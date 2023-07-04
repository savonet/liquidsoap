(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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

exception Internal_error of (Pos.t list * string)
exception Parse_error of (Pos.t * string)
exception Unsupported_encoder of (Pos.t option * string)

val conf_debug : bool ref
val conf_debug_errors : bool ref
val debug : bool Lazy.t
val profile : bool ref
val ref_t : ?pos:Pos.t -> Type.t -> Type.t

module Vars : sig
  type elt = String.t
  type t = Set.Make(String).t

  val empty : t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val add : elt -> t -> t
  val singleton : elt -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val disjoint : t -> t -> bool
  val diff : t -> t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val subset : t -> t -> bool
  val iter : (elt -> unit) -> t -> unit
  val map : (elt -> elt) -> t -> t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all : (elt -> bool) -> t -> bool
  val exists : (elt -> bool) -> t -> bool
  val filter : (elt -> bool) -> t -> t
  val filter_map : (elt -> elt option) -> t -> t
  val partition : (elt -> bool) -> t -> t * t
  val cardinal : t -> int
  val elements : t -> elt list
  val min_elt : t -> elt
  val min_elt_opt : t -> elt option
  val max_elt : t -> elt
  val max_elt_opt : t -> elt option
  val choose : t -> elt
  val choose_opt : t -> elt option
  val split : elt -> t -> t * bool * t
  val find : elt -> t -> elt
  val find_opt : elt -> t -> elt option
  val find_first : (elt -> bool) -> t -> elt
  val find_first_opt : (elt -> bool) -> t -> elt option
  val find_last : (elt -> bool) -> t -> elt
  val find_last_opt : (elt -> bool) -> t -> elt option
  val of_list : elt list -> t
  val to_seq_from : elt -> t -> elt Seq.t
  val to_seq : t -> elt Seq.t
  val to_rev_seq : t -> elt Seq.t
  val add_seq : elt Seq.t -> t -> t
  val of_seq : elt Seq.t -> t
end

module Ground : sig
  type t = ..

  type content = {
    descr : t -> string;
    to_json : pos:Pos.t list -> t -> Json.t;
    compare : t -> t -> int;
    typ : (module Type.Ground.Custom);
  }

  val handlers : (Type_base.custom, content * (t -> bool)) Hashtbl.t
  val register : (t -> bool) -> content -> unit

  exception Found of content

  val find : t -> content
  val to_string : t -> string
  val to_json : t -> pos:Pos.t list -> Json.t
  val to_descr : t -> Type_base.descr
  val to_type : t -> Type_base.custom
  val compare : t -> t -> int

  type t += Bool of bool | Int of int | String of string | Float of float
end

module type GroundDef = sig
  type content

  val descr : content -> string
  val to_json : pos:Pos.t list -> content -> Json.t
  val compare : content -> content -> int
  val typ : (module Type.Ground.Custom)
end

module MkGround : functor (D : GroundDef) -> sig
  type Ground.t += Ground of D.content
end

module Methods : sig
  type key = string
  type +!'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val mem : key -> 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  val singleton : key -> 'a -> 'a t
  val remove : key -> 'a t -> 'a t

  val merge :
    (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t

  val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val for_all : (key -> 'a -> bool) -> 'a t -> bool
  val exists : (key -> 'a -> bool) -> 'a t -> bool
  val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
  val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
  val cardinal : 'a t -> int
  val bindings : 'a t -> (key * 'a) list
  val min_binding : 'a t -> key * 'a
  val min_binding_opt : 'a t -> (key * 'a) option
  val max_binding : 'a t -> key * 'a
  val max_binding_opt : 'a t -> (key * 'a) option
  val choose : 'a t -> key * 'a
  val choose_opt : 'a t -> (key * 'a) option
  val split : key -> 'a t -> 'a t * 'a option * 'a t
  val find : key -> 'a t -> 'a
  val find_opt : key -> 'a t -> 'a option
  val find_first : (key -> bool) -> 'a t -> key * 'a
  val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
  val find_last : (key -> bool) -> 'a t -> key * 'a
  val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val to_seq : 'a t -> (key * 'a) Seq.t
  val to_rev_seq : 'a t -> (key * 'a) Seq.t
  val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
  val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
  val of_seq : (key * 'a) Seq.t -> 'a t
end

type t = private { t : Type.t; term : in_term; methods : t Methods.t }
and doc = Doc.Value.t

and let_t = {
  doc : doc option;
  replace : bool;
  pat : pattern;
  mutable gen : Type.var list;
  def : t;
  body : t;
}

and encoder_params = (string * [ `Encoder of encoder | `Term of t ]) list
and encoder = string * encoder_params
and invoke = { invoked : t; default : t option; meth : string }

and in_term =
  | Ground of Ground.t
  | Encoder of encoder
  | List of t list
  | Tuple of t list
  | Null
  | Cast of t * Type.t
  | Invoke of invoke
  | Open of t * t
  | Let of let_t
  | Var of string
  | Seq of t * t
  | App of t * (string * t) list
  | Fun of Vars.t * (string * string * Type.t * t option) list * t
  | RFun of string * Vars.t * (string * string * Type.t * t option) list * t

and pattern =
  | PVar of string list
  | PTuple of pattern list
  | PList of (pattern list * string option * pattern list)
  | PMeth of (pattern option * (string * pattern option) list)

type term = t

val unit : in_term
val is_ground : t -> bool
val string_of_pat : pattern -> string
val to_string : t -> string
val make : ?pos:Pos.t -> ?t:Type.t -> ?methods:term Methods.t -> in_term -> t
val free_vars_pat : pattern -> Vars.t
val bound_vars_pat : pattern -> Vars.t
val free_vars : ?bound:Vars.elt list -> t -> Vars.t
val can_ignore : Type.t -> bool

exception Unbound of Pos.Option.t * string
exception Ignored of t
exception No_label of t * string * bool * t
exception Duplicate_label of Pos.Option.t * string
exception Missing_arguments of Pos.Option.t * (string * Type.t) list
exception Unused_variable of (string * Pos.t)

val check_unused : throw:(exn -> unit) -> lib:bool -> t -> unit

module type Abstract = sig
  type content

  val t : Type.t
  val to_ground : content -> Ground.t
  val of_ground : Ground.t -> content
  val is_ground : Ground.t -> bool
  val to_term : content -> t
  val of_term : t -> content
  val is_term : t -> bool
end

module type AbstractDef = sig
  type content

  val name : string
  val to_json : pos:Pos.t list -> content -> Json.t
  val descr : content -> string
  val compare : content -> content -> int
end

module MkAbstract : functor (Def : AbstractDef) -> sig
  module T : Type.Ground.Custom

  type Ground.t += Value of Def.content
  type content = Def.content

  val t : Type.t
  val of_ground : Ground.t -> Def.content
  val to_ground : Def.content -> Ground.t
  val is_ground : Ground.t -> bool
  val of_term : t -> Def.content
  val to_term : Def.content -> t
  val is_term : t -> bool
end
