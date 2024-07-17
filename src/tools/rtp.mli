
(** Minimal interface over libortp. *)
type t
type mode = Recv | Send

(** [new_session addr port] *)
val new_session : mode -> string -> int -> t
val recv : t -> Mixer.Buffer.t -> bool
val send : t -> Mixer.Buffer.t -> unit
