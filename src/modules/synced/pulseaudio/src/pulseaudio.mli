(** An error occurred. *)
exception Error of int

(** Get the description of an error. *)
val string_of_error : int -> string

type sample_format =
  | Sample_format_s16le
  | Sample_format_s16be
  | Sample_format_float32le
  | Sample_format_float32be

type sample = {
  sample_format : sample_format;
  sample_rate : int;
  sample_chans : int;
}

type map

(** Direction of the stream. *)
type dir =
  | Dir_nodirection  (** Invalid direction. *)
  | Dir_playback  (** Playback stream. *)
  | Dir_record  (** Record stream. *)
  | Dir_upload  (** Sample upload stream. *)

(** Attributes of the buffer. *)
type buffer_attr = {
  max_length : int;  (** Maximum length of the buffer. *)
  target_length : int;  (** Target length of the buffer (playback only). *)
  prebuffering : int;  (** Pre-buffering (playback only). *)
  min_request : int;  (** Minimum request (playback only). *)
  fragment_size : int;  (** Fragment size (recording only). *)
}

(** Simple pulseaudio interface. *)
module Simple : sig
  (** Connections to a server. *)
  type t

  val create :
    ?server:string ->
    client_name:string ->
    dir:dir ->
    ?dev:string ->
    stream_name:string ->
    sample:sample ->
    ?map:map ->
    ?attr:buffer_attr ->
    unit ->
    t

  (** Close and free a connection to a server. *)
  val free : t -> unit

  val read : t -> float array array -> int -> int -> unit
  val read_floatarray : t -> floatarray array -> int -> int -> unit

  val read_ba :
    t ->
    (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    unit

  val write : t -> float array array -> int -> int -> unit
  val write_floatarray : t -> floatarray array -> int -> int -> unit

  (** Write data stored in a bigarray. Samples are interleaved. *)
  val write_ba :
    t ->
    (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    unit

  (** Wait until all data already written is played by the daemon. *)
  val drain : t -> unit

  (** Flush the playback buffer. *)
  val flush : t -> unit

  (** Return the playback latency. *)
  val latency : t -> int
end
