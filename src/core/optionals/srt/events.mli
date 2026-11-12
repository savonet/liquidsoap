(** An operator's events (spec/concurrency.md §2), run one at a time and in
    order by one scheduler task. Script callbacks fire inside these events, and
    the scheduler prepares the task's thread for script code.

    The connection creates one queue per operator, then [post]s each event, or
    [run]s one whose result it needs. *)

type t

val create : string -> t

(** Whether the calling thread is running this queue's events. *)
val on_runner : t -> bool

val post : t -> (unit -> unit) -> unit

(** Runs the job after the events already queued and returns its result, or
    raises its exception. On the queue's own thread, runs it at once. *)
val run : t -> (unit -> 'a) -> 'a
