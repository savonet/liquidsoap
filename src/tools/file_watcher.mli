(** A handle to the handler. *)
type t

(** Event to watch. *)
type event = Modify

(** Attach a handler to an event. *)
val watch : event -> string -> (unit -> unit) -> t

(** Remove a handler. *)
val unwatch : t -> unit

