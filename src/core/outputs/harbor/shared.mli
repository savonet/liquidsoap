(** The shared encoder mode: one encoder, a stream store every listener reads at
    its own offset, and bursts for new listeners.

    The operator creates one value per output, then calls {!start} and {!stop}
    under its start/stop serialisation, {!encode}, {!send} and {!metadata} on
    the streaming clock, and {!join} from a request to admit a listener. *)

type t

val create :
  factory:(string -> Frame.Metadata.Export.t -> Encoder.encoder) ->
  buffer:int ->
  burst:int option ->
  metaint:int ->
  unit ->
  t

(** Creates the encoder, named [name], and starts a new epoch: no byte of a
    previous encoder is a join point afterwards. *)
val start :
  t -> name:string -> last:(Frame.Metadata.Export.t * Icy.event) option -> unit

val running : t -> bool

(** Stops the encoder and appends its final bytes as its last output. *)
val stop : t -> unit

(** Encoded bytes, with the encoder's restart offset in them. *)
type encoded

val encode : t -> Frame.t -> encoded

(** Publishes the output's start, and its stream start if any, then appends the
    bytes. *)
val send : t -> encoded -> unit

(** Records [event] in effect from the current tail and passes [metadata] to the
    encoder. *)
val metadata : t -> Frame.Metadata.Export.t -> Icy.event -> unit

(** The listener state of a new listener, its initial bytes queued after [head]:
    [None] when no encoder is running. *)
val join :
  t ->
  head:string ->
  icy:bool ->
  Listener.t ->
  (pending:(unit -> bool) ->
  flush:(scratch:bytes -> unit) ->
  teardown:(unit -> unit) ->
  'a) ->
  'a option
