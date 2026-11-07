(** The listener role (spec/listener.md): binds and listens in the event queue,
    accepts from the readiness loop, and admits peers up to the kind's client
    limit and the script's [listen_callback]. *)

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
