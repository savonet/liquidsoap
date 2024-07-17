(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2006 Savonet team

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
(**
  Function for manipulating wav audio buffers.

  @author Samuel Mimram and David Baelde
*)

(* $Id: mixer.mli 2838 2007-02-14 10:23:23Z dbaelde $ *)

(** Specifications of the format of wav data. *)
type format =
    {
      channels : int; (** number of channels (defaut: 2) *)
      sample_freq : int; (** sampling frequency in Hertz (default: 44100) *)
      sample_size : int; (** size of a sample in bits (default: 16) *)
      big_endian : bool; (** are the integers stored in big endian? (default: false) *)
      signed: bool (** are the integer signed (default: yes but for 8 bitrates) *)
    }

(** A buffer [Generator] is somewhere to store audio data of unknown size,
  * and then fill [Buffer.t]s with that data. *)
module Generator :
sig
  type t

  (** Create a new generator of audio buffers. *)
  val create : unit -> t

  (** Is the audio buffer generator empty? *)
  val is_empty : t -> bool

  (** Amount of data in the generator, in bytes. *)
  val length : t -> int

  (** Should the generator be feeded? 
    * This function should be called every time before calling [Buffer.fill],
    * because a generator which needs to be feeded cannot fill a buffer
    * completely. *)
  val should_be_feeded : t -> bool

  (** Invalid data format. *)
  exception Invalid_format

  (** Feed a generator giving it data
    * in the format specified at the creation of the generator.
    * @raise Invalid_format if the data format isn't handled. *)
  val feed : t -> format -> string -> unit

  (** Removes data at the beginning. *)
  val remove : t -> int -> unit
end

(** A [Buffer.t], or audio frame, is a fixed size amount of audio, coming
  * with metadatas. It is the kind of values that is handled by
  * the [Source.source]s. *)
module Buffer : 
sig
  type t

  (** {2 Basic manipulation} *)

  (** Get a silent audio buffer. *)
  val create : unit -> t

  (** Audio format of audio buffers (which are in pcm wav format). *)
  val format : format

  (** Size in bytes of a buffer. Length in seconds. *)
  val size : int
  val length : float

  (** Number of bytes already filled. *)
  val position : t -> int

  val breaks : t -> int list
  val set_breaks : t -> int list -> unit
  val add_break : t -> int -> unit

  (** Is the audio buffer not full? *)
  val is_partial : t -> bool

  (** Free the buffer, that is set [position] to [0]. *)
  val free : t -> unit

  (** Get a string containing the audio data of the buffer. *)
  val to_string : t -> string

  (** Fill a audio buffer with data from a generator. If there's too much
    * data in the generator it will be left there. If there's not enought,
    * nothing will be left, the buffer will still be partial, but no error
    * is raised. *)
  val fill : t -> Generator.t -> unit

  exception No_chunk
  val get_chunk : t -> t -> unit

  (** {2 Audio processing}
    * All these functions only change the buffer,
    * but do not modify breaks or metadatas. *)

  (** An argument was not correct. *)
  exception Invalid_argument

  (** [blankify a offset len] fills [offset .. offset+len] with blank. *)
  val blankify : t -> int -> int -> unit

  (** [change_volume buf off len coeff] changes the volume of [len] bytes
    * the buffer [buf] starting at position [off]. *)
  val change_volume : t -> int -> int -> float -> unit

  (** [buf1 off1 buf2 off2 len] adds [len] bytes of the buffer [buf2],
    * starting at position [off2], to the buffer [buf1], position [buf1]. *)
  val add : t -> int -> t -> int -> int -> unit

  (** Type of an audio filter. *)
  type filter_type = Low_pass | High_pass | Band_pass | Notch

  (** [simple_filter buf off len freq q filter] filters [len] bytes of the buffer [buf] starting at position [off] using a filter of type [filter] with [freq] as cutoff frequency and [q] as resonance / bandwidth parameter ([q] should verify 0 < [q] <= 1, the nearer of 1 if the most resonant). *)
  val simple_filter : t -> int -> int -> int -> float -> filter_type -> unit

  (** [sine buf off len freq phi] return final [phi]. (TODO) *)
  val sine : t -> int -> int -> int -> float -> float

  val rms : t -> float

  (** {2 Metadatas handling} *)

  exception No_metadata

  type metadata = (string,string) Hashtbl.t

  val free_metadata : t -> unit
  val set_metadata : t -> int -> metadata -> unit
  val get_metadata : t -> int -> metadata option
  val get_all_metadata : t -> (int*metadata) list
  val set_all_metadata : t -> (int*metadata) list -> unit

end

