(** The dedicated encoder mode: one encoder per listener, created when it
    connects, so every listener's stream starts from an encoder's first byte.

    The operator calls {!start} and {!stop} under its start/stop serialisation,
    {!send} and {!metadata} on the streaming clock, and {!join} from a request.
    Each listener's encoder is used by the clock (encode, in-stream metadata),
    the request (creation, header) and stop or teardown (stop), one at a time;
    its queue is appended to by the clock and stop and drained by delivery
    passes. *)

type t

val create :
  factory:(string -> Frame.Metadata.Export.t -> Encoder.encoder) ->
  buffer:int ->
  metaint:int ->
  unit ->
  t

val start :
  t -> name:string -> last:(Frame.Metadata.Export.t * Icy.event) option -> unit

val running : t -> bool

(** Stops every listener's encoder and queues its final bytes. *)
val stop : t -> unit

(** Encodes the piece with every listener's encoder and queues the result. *)
val send : t -> Frame.t -> unit

val metadata : t -> Frame.Metadata.Export.t -> Icy.event -> unit

(** Creates the listener's encoder and queues [head], [preroll], then its
    header: [None] when the mode is not running. The encoder gets no piece until
    the preroll is sent. *)
val join :
  t ->
  head:string ->
  icy:bool ->
  preroll:string option ->
  Listener.t ->
  (pending:(unit -> bool) ->
  flush:(scratch:bytes -> unit) ->
  teardown:(unit -> unit) ->
  'a) ->
  'a option
