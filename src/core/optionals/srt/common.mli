(** What both operators share (spec/common.md): the parameters and their
    validation, the connection role and the script methods and callbacks. Each
    operator calls [proto] for its prototype, [parse] and [connection] when
    built, then hands [meth] and [callbacks] to [Lang.add_operator]. *)

val proto :
  mode:[ `Caller | `Listener ] ->
  (string * Lang.t * Lang.value option * string option) list

(** The parameter as a positive float, if not [null]. *)
val check_positive : (string * Lang.value) list -> string -> float option

(** The role's parameters, with [read_timeout] and [write_timeout], validated
    (spec/common.md §1). *)
val parse :
  (string * Lang.value) list -> Connection.params * float option * float option

(** The role [params.mode] names. *)
val connection :
  log:(unit -> Log.t) ->
  name:string ->
  params:Connection.params ->
  kind:'reaction Connection.kind ->
  unit ->
  'reaction Connection.connection

val meth :
  unit ->
  (< connection : 'reaction Connection.connection
   ; fallible : bool
   ; source_type : Source.source_type
   ; state : Start_stop.state
   ; transition_to : Start_stop.state -> unit
   ; .. > ->
  Lang.value)
  Lang.meth
  list

val callbacks :
  unit ->
  < connection : 'reaction Connection.connection ; .. > Lang_source.callback
  list
