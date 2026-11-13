(** Process-wide SRT state (spec/library.md): settings, the one-time libsrt
    start and cleanup, shutdown, address resolution, failure classification, the
    base socket setup and stream sync sources.

    Operators call [init] when built and [register_shutdown] to be released at
    shutdown; connections then call [resolve], [create_socket] and
    [is_permanent]. Every thread that calls libsrt outside the scheduler's tasks
    starts through [spawn], so that cleanup waits for it. *)

val log : Log.t
val conf_prefer_address : string Dtools.Conf.t
val conf_timeout : float Dtools.Conf.t

(** Starts libsrt at application start and cleans it up at final cleanup. Later
    calls do nothing. *)
val init : unit -> unit

type shutdown_hook

(** Registers what releases an operator at core shutdown. The registry holds the
    hook weakly: the operator keeps it alive, and a collected operator is
    forgotten. *)
val register_shutdown : (unit -> unit) -> shutdown_hook

val shutting_down : bool Atomic.t

(** Rounded up, so that a positive duration never becomes libsrt's "do not
    wait". *)
val ms_of_seconds : float -> int

val is_ipv6 : Unix.sockaddr -> bool

(** Every address of the host, in resolution order. *)
val resolve :
  prefer_address:[ `System | `Ipv4 | `Ipv6 ] ->
  string ->
  int ->
  Unix.sockaddr list

(** Failures waiting cannot fix (spec/connection.md §4). *)
val is_permanent : exn -> bool

val close_quietly : Srt.socket -> unit

(** Steps 1–5 of spec/connection.md §3. *)
val create_socket : payload_size:int -> messageapi:bool -> unit -> Srt.socket

(** One sync source per stream name, for the life of the process: an identity
    that changed between two ticks would look like a second sync source. *)
val sync_source : string -> Clock.Sync_source.t

(** Runs the function on a thread of its own, placed by the scheduler, so that
    blocking libsrt calls hold no worker of its pool. Cleanup waits for the
    thread to return, as cleaning libsrt up under a call in flight corrupts the
    process's memory. *)
val spawn : (unit -> unit) -> unit

(** [spawn], returning what waits for the thread to return. *)
val spawn_joinable : (unit -> unit) -> unit -> unit

(** Calls the function from a scheduler task once the delay has elapsed. It must
    not block: it posts its work elsewhere. *)
val after : float -> (unit -> unit) -> unit
