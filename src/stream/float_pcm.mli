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

(** Create a buffer with a given number of channels and a given length. *)
val create_buffer : int -> int -> float array array

type pcm = float array array

(** {2 Conversions}
  * Lengths are in samples. *)

val to_s16le    : pcm -> int -> int -> string       -> int -> int
val to_s16le_ni : pcm -> int -> int -> string array -> int -> int

(** [from_s16le buf ofs sbuf ofs len] converts and copies [len] samples
  * from the S16LE pcm buffer [sbuf] to [buf]. *)
val from_s16le : pcm -> int -> string -> int -> int -> unit
val from_s16le_ni : pcm -> int -> string array -> int -> int -> unit

(** Multiply samplerate by the given ratio. *)
val native_resample : float -> float array -> int -> int -> float array

(** [resample_s16le src src_off len signed samplesize big_endian
      ratio dst dst_off]: resample from s16le *)
val resample_s16le : string -> int -> int ->
              bool -> int -> bool -> float -> pcm -> int -> int


(** Blit pcm *)
val blit : float array -> int -> float array -> int -> int -> unit

val copy : float array -> float array

(** {2 Sound processing} *)

val blankify : pcm -> int -> int -> unit
val multiply : pcm -> int -> int -> float -> unit

(** Add two buffers and put the result in the first one. *)
val add : pcm -> int -> pcm -> int -> int -> unit
val substract : pcm -> int -> pcm -> int -> int -> unit
val rms : pcm -> int -> int -> float array
