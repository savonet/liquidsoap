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

(** The source-id records of inline encoders (inline-codecs.md §5): encoders
    reading the same input source at the same time share one id and one
    timeline. A transform build calls {!join}, every input frame it converts
    calls {!advance}, an input discontinuity calls {!close}, and the end of the
    transform calls {!leave}. *)

type t

(** The record of [input], identified physically: the open one when it exists,
    else a fresh one with elapsed time 0. *)
val join : Source.source -> t

val id : t -> Source_id.t

(** Input time elapsed since the record's origin, in main ticks. *)
val elapsed : t -> int

(** [advance t ~until] sets the elapsed time to [until] when it is later: every
    transform of the record sees the same input frames, so each reports its own
    end and the latest wins. *)
val advance : t -> until:int -> unit

val close : t -> unit
val is_open : t -> bool
val leave : t -> unit
