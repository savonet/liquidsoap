include module type of struct
  include Types.Event
end

val on_event : (event -> unit) -> unit

(** Logs the event and hands it to the subscribers. *)
val emit : log:Log.t -> clock:string -> event_kind -> unit

(** Whether a debug event is worth building. *)
val wants_debug : log:Log.t -> bool
