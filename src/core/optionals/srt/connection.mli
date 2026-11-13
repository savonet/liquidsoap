(** The role lifecycle both connection roles share (spec/connection.md);
    [Caller] and [Listener] (spec/caller.md, listener.md) inherit it and supply
    [connect_role] and [retry]. An operator builds one through
    [Common.connection] from its parameters and its kind's reactions, calls
    [start] and [stop] from its own start and stop, and [connect], [disconnect]
    and [sockets] from its script methods.

    Role state changes under one lock, tagged with a generation that every late
    result is checked against; every event and every kind reaction runs
    afterwards in the operator's [Events] queue. *)

type prefer_address = [ `System | `Ipv4 | `Ipv6 ]

type listen_callback =
  hs_version:int ->
  peeraddr:string ->
  streamid:string option ->
  Srt.socket ->
  bool

type params = {
  mode : [ `Caller | `Listener ];
  host : string;
  port : int;
  bind_address : string;
  prefer_address : prefer_address;
  ipv6only : bool option;
  listen_callback : listen_callback option;
  streamid : string option;
  passphrase : string option;
  pbkeylen : int option;
  enforced_encryption : bool option;
  polling_delay : float;
  connection_timeout : float option;
  payload_size : int;
  messageapi : bool;
}

(** What a kind sees of a peer: [fail] reports a failed exchange, once or many
    times, from any thread. *)
type peer_handle = {
  socket : Srt.socket;
  address : Unix.sockaddr;
  fail : unit -> unit;
}

(** A kind's part (spec/connection.md §5 and §8): [client_limit] bounds a
    listener's peers; [setup_socket] sets the options it needs for exchanging
    data; [connected] is its reaction to a new peer and [live] runs once the
    peer is live; [ending] runs before the socket closes and calls its
    continuation once the socket may close; [ended] runs after. [shutdown_wait]
    bounds the wait for a clean end at shutdown. *)
type 'reaction kind = {
  client_limit : int option;
  setup_socket : Srt.socket -> unit;
  connected : peer_handle -> 'reaction;
  live : 'reaction -> unit;
  ending : 'reaction -> clean:bool -> (unit -> unit) -> unit;
  ended : 'reaction -> unit;
  shutdown_wait : float;
}

(** [announced]: [on_connect] fired, so [on_disconnect] is owed. [created]: the
    socket received its creation event, so its close event is owed. *)
type 'reaction peer = {
  handle : peer_handle;
  mutable created : bool;
  mutable reaction : 'reaction option;
  mutable announced : bool;
  mutable live_since : float;
  live : bool Atomic.t;
  ended : bool Atomic.t;
}

(** The socket of an attempt in flight, closed by a release so that a connect
    blocked on it returns. *)
type attempt = { mutable attempt_socket : Srt.socket option }

type 'reaction state =
  | Idle
  | Attempting of attempt
  | Waiting
  | Connected of 'reaction peer
  | Listening of { listener : Srt.socket; mutable peers : 'reaction peer list }

val is_attempting : 'reaction state -> bool
val is_waiting : 'reaction state -> bool

class virtual ['reaction] connection :
  log:(unit -> Log.t) ->
  name:string ->
  params:params ->
  kind:'reaction kind ->
  unit ->
object
  val m : Mutex.t

  (** Under [m]. *)
  val mutable state : 'reaction state

  val events : Events.t

  (** Sockets created and not yet closed, which a release at shutdown waits for.
  *)
  val open_sockets : int Atomic.t

  method register_on_connect : (unit -> unit) -> unit -> unit
  method register_on_disconnect : (unit -> unit) -> unit -> unit
  method register_on_socket : (string -> Srt.socket -> unit) -> unit -> unit

  (** Whether the connection is wanted (spec/common.md §5). *)
  method wanted : bool

  (** The live peers, with their IP address. *)
  method sockets : (string * Srt.socket) list

  method start : unit
  method stop : unit
  method connect : unit
  method disconnect : unit

  (** Starts the role at a new generation. *)
  method virtual private connect_role : int -> unit

  (** Tries again after the delay if the role is still at that generation. *)
  method virtual private retry : int -> float -> unit

  (** Under [m]: whether the generation is still the current one. *)
  method private current : int -> bool

  method private new_socket : Srt.socket

  (** Fires the close event when the socket received its creation event. *)
  method private close_socket : created:bool -> Srt.socket -> unit

  method private set_encryption : Srt.socket -> unit
  method private socket_event : string -> Srt.socket -> unit

  method private make_peer :
    created:bool -> Srt.socket -> Unix.sockaddr -> 'reaction peer

  (** In the event queue: the kind's reaction, [on_connect], then live. *)
  method private peer_connected : 'reaction peer -> unit

  (** Ends the peer once, from any thread. *)
  method private end_peer : 'reaction peer -> clean:bool -> unit
end
