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

(* The channel layouts liquidsoap knows about, and the mapping to and from a
   channel count. Kept here rather than in Audio_converter because frames and
   frame types need the mapping and must not depend on the converters, which
   themselves work on frame content. *)

exception Unsupported

type t = [ `Mono | `Stereo | `Five_point_one ]

let channels_of_layout = function
  | `Mono -> 1
  | `Stereo -> 2
  | `Five_point_one -> 6

let layout_of_channels = function
  | 1 -> `Mono
  | 2 -> `Stereo
  | 6 -> `Five_point_one
  | _ -> raise Unsupported
