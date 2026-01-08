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

type ('a, 'b) t

val create : int -> ('a, 'b) t
val replace : ('a, 'b) t -> 'a -> 'b -> unit
val mem : ('a, 'b) t -> 'a -> bool
val find : ('a, 'b) t -> 'a -> 'b
val find_opt : ('a, 'b) t -> 'a -> 'b option
val remove : ('a, 'b) t -> 'a -> unit
val length : ('a, 'b) t -> int
