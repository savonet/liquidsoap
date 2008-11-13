(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

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

val sub : float array array -> int -> int -> float array array

val to_s16le : float array array -> int -> int -> string -> int -> int

val to_s16le_ni : float array array -> int -> int -> string array -> int -> int

(** Length in samples. *)
val from_s16le : float array array -> int -> string -> int -> int -> unit

val from_s16le_ni : float array array -> int -> string array -> int -> int -> unit

(** Multiply samplerate by the given ratio. *)
val native_resample : float -> float array -> int -> int -> float array

module Generator :
  sig
    type t
    val create : ?out_freq:int -> ?out_chans:int -> unit -> t
    val length : t -> int
    val clear : t -> unit
    (** Add data to the Generator. The arrays are not copied,
      * and thus should never be modified afterwards. *)
    val feed : t -> ?sample_freq:int -> float array array -> unit
    (** Append metadata at [position] + current length.
      * This function should be called before [feed] since [feed]
      * increases the length. See [AFrame.feed_frame]. *)
    val add_metadata : t -> int*Frame.metadata -> unit
    (** Take without removing [len] metadata starting from position 0 *)
    val peek_metadata : t -> int -> (int*Frame.metadata) list
    (** Append break at [position] + current length.
      * This function should be called before [feed] since [feed]
      * increases the length. See [AFrame.feed_frame]. *)
    val add_break : t -> int -> unit
    (** Take without removing [len] breaks starting from position 0 *)
    val peek_breaks : t -> int -> int list
    val is_empty : t -> bool
    val remove : t -> ?initial:bool -> int -> unit
    (* Returns remaining ticks before next break, or 
     * buffer length if no break *)
    val remaining : t -> int
    (** Fill a frame with as much audio data as possible,
      * or [size] if given.
      * Also advances metadata and breaks. *)
    val fill : t -> ?size:int -> float array array -> int -> int
  end

module Generator_from_raw :
  sig
    type t
    (** Create a generator for the given input/output parameters,
      * designed for filling float_pcm buffers of [samples] samples. *)
    val create :
      channels:int ->
      samplesize:int ->
      signed:bool ->
      big_endian:bool -> in_freq:float -> samples:int -> out_freq:float -> t
    val clear : t -> unit
    val feed : t -> string -> unit
    (** Add a metadata at position [len] plus current length *)
    val add_metadata : t -> int*Frame.metadata -> unit
    (** Take without removing metadata from position 0 to [len] *)
    val peek_metadata : t -> int -> (int*Frame.metadata) list
    (** Append break at [position] + current length.
      * This function should be called before [feed] since [feed]
      * increases the length. See [AFrame.feed_frame]. *)
    val add_break : t -> int -> unit
    (** Take without removing breaks from position 0 to [len] *)
    val peek_breaks : t -> int -> int list
    (** Number of available output samples. *)
    val length : t -> int
    val is_empty : t -> bool
    (* Returns remaining ticks before next break, or
     * buffer length if no break *)
    val remaining : t -> int
    val remove : t -> ?initial:bool -> int -> unit
    (** Fill a frame with as much audio data as possible.
      * or [size] if given.
      *  Also advances metadata and breaks. *)
    val fill : t -> ?size:int -> float array array -> int -> int
  end

module To_s16le :
sig
  type t
  (** [create ~in_channels ~in_samplerate ~out_channels ~out_samplerate n]
    * returns a converter for chunks of at most [n] samples. *)
  val create : in_channels:int -> in_samplerate:int ->
               out_channels:int -> out_samplerate:int -> int -> t
  (** To avoid copy all conversions are done to that buffer. *)
  val get_output_buffer : t -> string
  (** Takes offset/length in the given array in samples,
    * returns the amount of written data in bytes. *)
  val convert : t -> float array array -> int -> int -> int
end

val blankify : float array array -> int -> int -> unit
val multiply : float array array -> int -> int -> float -> unit
val add : float array array -> int -> float array array -> int -> int -> unit
val substract : float array array -> int -> float array array -> int -> int -> unit
val rms : float array array -> int -> int -> float array
