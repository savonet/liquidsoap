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

open Type_base

type custom = Type_base.custom

module type Specs = sig
  type content

  val name : string
  val copy_with : (t -> t) -> content -> content
  val occur_check : (t -> unit) -> content -> unit

  val filter_vars :
    (var list -> t -> var list) -> var list -> content -> var list

  val repr : (var list -> t -> Repr.t) -> var list -> content -> Repr.t
  val subtype : (t -> t -> unit) -> content -> content -> unit
  val sup : (t -> t -> t) -> content -> content -> content

  (** How a payload survives a dump, which carries strings and no more. [parse]
      answers [None] for a printed form it cannot rebuild. *)
  val serialize : content -> string

  val parse : string -> content option
end

(** What a dumped custom type is here: its name and the printed form of its
    payload, rebuilt by whoever implements that name. *)
val of_dump : string -> string -> Type_base.custom_handler option

module type Implementation = sig
  type content

  val handler : content -> Type_base.custom_instance
  val to_content : custom -> content

  (** What a custom type of this one's name carries here. *)
  val payload : Type_base.custom_instance -> content
end

module Make (S : Specs) : Implementation with type content = S.content
