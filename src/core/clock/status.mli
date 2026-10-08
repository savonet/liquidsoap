include module type of struct
  include Types.Status
end

(** The run of a started or stopping clock. *)
val run : t -> run option

val no_figures : figures

(** The figures accumulated since [mark]. The maxima of a span cannot be had by
    subtraction: they are those of [slowest]. *)
val figures_since : slowest:slowest -> figures -> figures -> figures

val sync_mode_of_string : string -> [> `Automatic | `Cpu | `Passive | `Unsynced ]
val or_none : string option -> string

val string_of_sync_mode :
  [< `Automatic | `Cpu | `Passive | `Unsynced ] -> string

val string_of_source_type : [< `Active | `Output | `Passive ] -> string
val string_of_animator : [< `Task | `Thread ] -> string
val string_of_owner : owner -> string
val string_of_stop_reason : stop_reason -> string

(** The status report of spec/observability.md §3. *)
val report : t list -> string

(** The source graph of spec/observability.md §4, one per clock and sub-clock.
*)
val source_graph : t list -> string
