
(** The core loop, reading from the scheduler and controlling output. *)

val start : Types.source -> unit

(** Set [shutdown] to false to stop the loop *)
val shutdown : bool ref

val uptime : unit -> float
val skip : unit -> unit

(** Get the last 10 metadatas *)
val get_metadatas : unit -> Request.metadata Queue.t
