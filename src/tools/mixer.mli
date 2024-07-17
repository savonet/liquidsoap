(**
  Function for manipulating wav audio buffers.

  @author Samuel Mimram and David Baelde
*)

(* $Id: mixer.mli,v 1.35 2005/04/12 20:26:55 dbaelde Exp $ *)

exception Invalid_format (** Invalid data format. *)
exception Partial_buffer (** The buffer is not full so no processing can be done on it. *)
exception Invalid_argument (** An argument was not correct. *)

(** Specifications of the format of wav data. *)
type format =
    {
      channels : int; (** number of channels (defaut: 2) *)
      sample_freq : int; (** sampling frequency in Hertz (default: 44100) *)
      sample_size : int; (** size of a sample in bits (default: 16) *)
      big_endian : bool; (** are the integers stored in big endian? (default: false) *)
      signed: bool (** are the integer signed (default: yes but for 8 bitrates) *)
    }

(** Type of an audio filter. *)
type filter_type = Low_pass | High_pass | Band_pass | Notch

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
    * This function should be called every time before calling [feed],
    * because a generator which needs to be feeded cannot fill a buffer
    * completely. *)
  val should_be_feeded : t -> bool

  (** Feed a generator giving it data
    * in the format specified at the creation of the generator.
    * @raise Invalid_format if the data format isn't handled. *)
  val feed : t -> format -> string -> unit
end

(** A [Buffer.t], or audio frame, is a fixed size amount of audio, coming
  * with metadatas. It is the kind of values that is handled by
  * the [Types.sources]. *)
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
  val already : t -> int
  val set_already : t -> int -> unit

  (** Is the audio buffer not full? *)
  val is_partial : t -> bool

  (** Free the buffer, that is set [already] to [0]. *)
  val free : t -> unit

  (** Get a string containing the audio data of the buffer. *)
  val to_string : t -> string

  (** Fill a audio buffer with data from a generator. If there's too much
    * data in the generator it will be left there. If there's not enought,
    * nothing will be left, the buffer will still be partial, but no error
    * is raised. *)
  val fill : t -> Generator.t -> unit

  (** {2 Audio processing} *)

  (** [blankify a] fills [a.already...size] with blanks. 
    * It does not change [a.already]. *)
  val blankify : t -> unit

  (** [change_volume buf off len coeff] changes the volume of [len] bytes
    * the buffer [buf] starting at position [off]. *)
  val change_volume : t -> int -> int -> float -> unit

  (** [buf1 off1 buf2 off2 len] adds [len] bytes of the buffer [buf2],
    * starting at position [off2], to the buffer [buf1], position [buf1]. *)
  val add : t -> int -> t -> int -> int -> unit

  (** [simple_filter buf off len freq q filter] filters [len] bytes of the buffer [buf] starting at position [off] using a filter of type [filter] with [freq] as cutoff frequency and [q] as resonance / bandwidth parameter ([q] should verify 0 < [q] <= 1, the nearer of 1 if the most resonant). *)
  val simple_filter : t -> int -> int -> int -> float -> filter_type -> unit

  (** [sine buf off len freq phi] return final [phi]. (TODO) *)
  val sine : t -> int -> int -> int -> float -> float

  (** {2 Metadatas handling} *)

  exception No_metadata

  type metadata = (string,string) Hashtbl.t

  val free_metadatas : t -> unit
  val push_metadata : t -> metadata -> unit
  val pop_metadata : t -> metadata
  val copy_metadatas : t -> metadata Queue.t
  val get_metadata : t -> metadata option

end

