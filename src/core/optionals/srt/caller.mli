(** The caller role (spec/caller.md): attempts on a scheduler-placed thread,
    each address in turn, and a new attempt [polling_delay] after a failure or a
    lost peer. *)

class ['reaction] connection :
  log:(unit -> Log.t) ->
  name:string ->
  params:Connection.params ->
  kind:'reaction Connection.kind ->
  unit ->
object
  inherit ['reaction] Connection.connection
  method private connect_role : int -> unit
  method private retry : int -> float -> unit
end
