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

module type Kind = sig
  type t

  val default : t
  val to_string : t -> string
  val parse_param : string -> string -> t option
  val compatible : t -> t -> bool
  val merge : t -> t -> t
  val serialize : t -> string
  val parse_serialized : string -> t option
end

module type S = sig
  val name : string

  module Copy : Kind
  module Audio : Kind
  module Video : Kind
end
