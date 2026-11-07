(** The process-wide readiness loop (spec/poll.md), on a thread of its own so
    that its wait holds no worker of the scheduler's pool (KC17).

    A listener calls [register] once its socket listens and [remove] before
    closing it. Handlers run on the loop's thread, one at a time, under its
    lock, so [remove] returns only once the socket's handler is done; callers of
    [remove] hold no lock a handler takes. *)

(** Makes the socket non-blocking and calls the handler each time it is
    readable, until [remove]. *)
val register : Srt.socket -> (Srt.socket -> unit) -> unit

val remove : Srt.socket -> unit
