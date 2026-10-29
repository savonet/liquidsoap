(** The shared stream store: bytes at absolute offsets, one writer, any number
    of concurrent readers.

    The streaming clock is the only caller of {!append}. Delivery passes call
    {!read} at any time; a read never waits and reports any range a concurrent
    append may have overwritten. Every append keeps the [retain] bytes before
    its first offset readable, so a reader at most [retain] bytes behind the
    newest output start always reads valid bytes. *)

type t

val create : retain:int -> unit -> t

(** Offset of the next byte to be appended. *)
val tail : t -> int

(** Grows the store when [retain] plus the appended length does not fit, and
    shrinks it back once a large append has left that window. *)
val append : t -> bytes -> int -> int -> unit

(** [read t ~offset ~length dst dst_offset] copies [length] bytes from [offset],
    and returns [false] when some copied byte may not be the one appended at its
    offset. *)
val read : t -> offset:int -> length:int -> bytes -> int -> bool

(** Current capacity, in bytes. *)
val capacity : t -> int
