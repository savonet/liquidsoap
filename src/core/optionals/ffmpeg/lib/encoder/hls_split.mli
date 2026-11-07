(** The split points of one [%ffmpeg] HLS instance (encoder.md §8.2, §8.4):
    candidates taken before packets, held until every other stream that is not
    all-key shows a key packet, forced splits, and container starts whose init
    segment is carved out of the bytes.

    The instance writes every container byte into {!buffer}. Its packet hook
    calls {!packet}, which calls [flush] to make the muxer write out what
    precedes a candidate. {!take} returns what is decided, in byte order. *)

type t

(** [fields] are the participating streams, in track order.
    [init_length buffer offset] measures the init segment of a container
    starting at [offset]: [None] while it is incomplete, [Some 0] for a format
    without one. *)
val create :
  fields:string list -> init_length:(Buffer.t -> int -> int option) -> unit -> t

val buffer : t -> Buffer.t

(** Marks a stream whose packets are not all key packets, before it writes any:
    its first packet after a candidate then decides it. *)
val inter_coded : t -> string -> unit

(** Every participating stream has written only key packets. *)
val all_key : t -> bool

(** Adds a request: a media time at which a split point is wanted. It stays
    pending until a candidate is taken at it. *)
val request : t -> int -> unit

val packet :
  t -> flush:(unit -> unit) -> field:string -> key:bool -> time:int -> unit

(** A split at the current position, made without its condition. *)
val force : t -> flush:(unit -> unit) -> time:int -> unit

(** A container starts at the current position; [first] for the instance's first
    one, whose init segment {!take} returns apart. *)
val container_start : t -> first:bool -> time:int -> unit

(** A container start whose init segment is not yet known is pending. *)
val restart_pending : t -> bool

(** [init] is [Some i] in the call that resolves the first container start. *)
type taken = { init : string option option; pieces : Encoder.hls_piece list }

val take : t -> taken
