(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2009 Savonet team

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

type t

val create : unit -> t
val clear : t -> unit
val length : t -> int
val remaining : t -> int
val add_metadata : t -> Frame.metadata -> unit
val add_break : t -> unit
val advance : t -> int -> unit
val remove : t -> int -> unit
val feed : t -> Frame.content -> int -> int -> unit
val feed_from_frame : t -> Frame.t -> unit
val feed_from_pcm : sample_freq:int -> t -> float array array -> unit
val fill : t -> Frame.t -> unit
