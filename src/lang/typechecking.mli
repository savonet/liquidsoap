exception No_method of string * Type.t

val debug : bool ref
val display_types : bool ref
val value_restriction : Term.t -> bool
val add_task : (unit -> unit) -> unit
val pop_tasks : unit -> unit

val type_of_pat :
  level:int ->
  pos:Pos.Option.t ->
  Term.pattern ->
  (string list * Type.t) list * Type.t

val check :
  ?env:Typing.env ->
  throw:(bt:Printexc.raw_backtrace -> exn -> unit) ->
  Term.t ->
  unit
