(*****************************************************************************
  
  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team
  
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
  
(** Mutable string buffers where the main operation is to add strings at the end. *)
  
type t

include Strings_base.S with type buffer := t and type return := unit
  
val empty : unit -> t
  
(** Convert from a immutable strings. *)
val of_strings : Strings.t -> t
  
(** Convert to a immutable strings. *)
val to_strings : t -> Strings.t

(** Append strings to the buffer. *)
val append_strings : t -> Strings.t -> unit

(** Empty the buffer and return its content. *)
val flush : t -> Strings.t
