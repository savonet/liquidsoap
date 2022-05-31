val debug : bool ref
val value_restriction : Term.t -> bool
val add_task : (unit -> unit) -> unit
val pop_tasks : unit -> unit

val type_of_pat :
  level:int ->
  pos:Pos.Option.t ->
  Term.pattern ->
  (string list * Type.t) list * Type.t

val type_of_encoder : (pos:Pos.Option.t -> Term.encoder -> Type.t) ref
val check : ?ignored:bool -> throw:(exn -> unit) -> Term.t -> unit
