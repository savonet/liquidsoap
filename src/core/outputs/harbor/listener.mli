(** A listener: its record, its socket and its closed flag, common to both
    encoder modes.

    The request creates the {!info} before the admission predicate, then the
    listener with {!create} once admitted. Any work may {!mark_closed} it; only
    its delivery group writes to it and closes its socket. *)

type t

(** What the listener record shows; [peer] is [ip:port], for logs. *)
type info = {
  id : int;
  ip : string;
  peer : string;
  uri : string;
  protocol : string;
  headers : (string * string) list;
  connected_at : float;
}

(** [wake] is called by the winner of {!mark_closed}, so the listener's group
    removes it promptly. *)
val create : log:Log.t -> wake:(unit -> unit) -> info -> Harbor.socket -> t

val info : t -> info
val log : t -> Log.t
val socket : t -> Harbor.socket

(** Monotonic time, in seconds. *)
val now : unit -> float

(** Offers bytes to the socket in one non-blocking write: the count accepted,
    [0] when none was, [-1] on an error, logged. Accepted bytes count as
    progress and towards [bytes_sent]. *)
val write : t -> bytes -> int -> int -> int

val last_progress : t -> float

(** Set by the group at publication. *)
val set_last_progress : t -> float -> unit

(** Idempotent; exactly one caller wins, records the disconnection time, logs
    and wakes the group. *)
val mark_closed : t -> unit

val is_closed : t -> bool

(** {!mark_closed} for a listener too far behind the stream, logged unless it is
    closed already. *)
val close_for_overflow : t -> unit

(** Errors are ignored. Called once, by the socket's owner. *)
val close_socket : t -> unit

val record_t : Lang.t

(** The listener record, [duration] and [bytes_sent] read now. *)
val to_value : t -> Lang.value

(** The record of a request not yet admitted. *)
val info_to_value : info -> Lang.value
