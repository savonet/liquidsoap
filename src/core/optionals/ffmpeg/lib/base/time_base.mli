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

(** Liquidsoap's time bases and the layer's only time-base conversion (common.md
    §4, §5). The time bases are read once the script is parsed. *)

type t = Avutil.rational

(** [1/main_rate]: positions and lengths of content. *)
val main : unit -> t

(** [1/audio rate]. *)
val audio : unit -> t

(** [1/video rate]. *)
val video : unit -> t

(** A time base read from media is valid when both components are positive. *)
val is_valid : t -> bool

(** [rescale ~src ~dst t] is [t × src / dst] rounded to nearest, halves away
    from zero, exactly; [None] when the result is outside the native integer
    range.

    @raise Invalid_argument when [src] or [dst] is not valid. *)
val rescale : src:t -> dst:t -> int -> int option

(** [rescale_int64] reads a timestamp the bindings give. *)
val rescale_int64 : src:t -> dst:t -> Int64.t -> int option

(** [to_main ~src t] is {!rescale} into {!main}. *)
val to_main : src:t -> int -> int option

val to_string : t -> string
