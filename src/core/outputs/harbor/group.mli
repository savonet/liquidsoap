(** Delivery groups: a set of listeners served by one delivery pass at a time.

    The operator creates groups at start, {!publish}es admitted listeners into
    them and {!wake}s the ones with members on every streaming cycle; at stop,
    {!stop} runs each group's last pass and closes its members. A pass is a
    [`Non_blocking] scheduler task: it flushes members owed bytes, times out
    stalled ones, removes the closed ones, closes their sockets and queues their
    teardown as a [`Threaded] task. *)

type t

(** What a group needs of a listener: the mode answers whether it is owed bytes
    and flushes it with one write, using [scratch] as staging space; [teardown]
    runs after its socket is closed. *)
type member = {
  listener : Listener.t;
  pending : unit -> bool;
  flush : scratch:bytes -> unit;
  teardown : unit -> unit;
}

val create : log:Log.t -> timeout:float -> unit -> t
val has_members : t -> bool

(** Schedules one pass unless one is already due; never blocks. *)
val wake : t -> unit

(** Adds the member and wakes the group. [false] when the group is stopping: the
    caller still owns the listener's socket. *)
val publish : t -> member -> bool

(** Waits for a running pass, runs a last one with no retry, then marks every
    member closed, closes their sockets and queues their teardown. Later
    publications fail. *)
val stop : t -> unit
