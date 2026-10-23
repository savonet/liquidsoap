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

(** Option tables handed to libav, and the one error that reports what libav
    left unconsumed (encoder.md §4, decoder.md §4.3). *)

val of_list : Liquidsoap_ffmpeg_formats.Encoder_format.opts -> Avutil.opts

(** [Unrecognized options: <scope>: k=v, …; <scope>: …], scopes with no leftover
    left out; [None] when nothing is left. *)
val unused_message : (string * Avutil.opts) list -> string option

val string_of_value : Avutil.value -> string
