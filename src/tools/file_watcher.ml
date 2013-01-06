(** Event to watch. *)
type event = [ `Modify ]

(** Type for unwatching function. *)
type unwatch = unit -> unit

(** Type for watching function. *)
type watch = event list -> string -> (unit -> unit) -> unwatch
