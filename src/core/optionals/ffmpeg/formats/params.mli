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

(** What a process knows of the parameters of the three FFmpeg content kinds.

    {!Kinds.Make} takes one implementation of {!S} and registers the kinds over
    it. Two implementations exist: libav's, linked by a process that streams,
    and the stand-in of [dummy/], linked by a process that only typechecks. Both
    accept exactly the same annotations. [Kind] is the parameters of one kind.
*)

module type Kind = sig
  type t

  (** Every property unset. *)
  val default : t

  (** The printed parameter string: [key=value] pairs joined by [,], in the
      order of the kind's parameter table; empty when nothing is set. *)
  val to_string : t -> string

  (** [parse_param label value] is a value holding only that parameter, [None]
      when the label is unknown or the value rejected. *)
  val parse_param : string -> string -> t option

  (** Holds exactly when [merge] succeeds. *)
  val compatible : t -> t -> bool

  (** Fills every unset property from the side that has it.

      @raise Failure naming the first property that differs. *)
  val merge : t -> t -> t

  (** The form a typing dump carries, read back by [parse_serialized]. *)
  val serialize : t -> string

  val parse_serialized : string -> t option
end

module type S = sig
  (** Names the implementation in the error raised when a second one registers.
  *)
  val name : string

  module Copy : Kind
  module Audio : Kind
  module Video : Kind
end
