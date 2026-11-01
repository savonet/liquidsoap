# Listeners of icecast.server (listeners.md §1, §4 to §6). A mount output
# calls `icecast.server.listeners.accept` (listeners.liq) for every request;
# an admitted listener is a session of the mount, held in the table
# `icecast.server.listeners.sessions()` of its entry, served by `connected`
# and ended by `disconnected` or, when it is never served, by the
# `on_dropped` that `accept` hands the harbor output. Each session releases
# its decision once. Stats read the sessions through `current`, and `record`
# builds the listener record observers see.
let icecast.server.listeners = ()

# Sessions of a mount, by listener id: `user` the per-user limit counts,
# `address` the resolved address, `release` the decision's release, `served`
# the harbor listener once its connect event starts.
# `remove(id)` returns the session it removes.
# @flag hidden
def icecast.server.listeners.sessions() =
  sessions = ref([])
  def add(id, ~user, ~address, release) =
    atomic(
      fast=true,
      fun () ->
        sessions :=
          [
            ...sessions(),
            (
              id,
              {user = user, address = address, release = release, served = null}
            )
          ]
    )
  end

  def find(id) =
    list.assoc.nullable(id, atomic(fast=true, sessions))
  end

  def remove(id) =
    atomic(
      fast=true,
      fun () ->
        begin
          session = list.assoc.nullable(id, sessions())
          sessions := list.assoc.remove(id, sessions())
          session
        end
    )
  end

  def serve(listener) =
    atomic(
      fast=true,
      fun () ->
        sessions :=
          list.map(
            fun (s) ->
              begin
                let (key, session) = s
                (
                  key,
                  {
                    user = session.user,
                    address = session.address,
                    release = session.release,
                    served =
                      key == listener.id ? null(listener) : (session.served)
                  }
                )
              end,
            sessions()
          )
    )
  end

  def served() =
    list.filter_map(fun (s) -> snd(s).served, atomic(fast=true, sessions))
  end

  def of_user(user) =
    list.length(
      list.filter(
        fun (s) -> null.defined(snd(s).served) and snd(s).user == user,
        atomic(fast=true, sessions)
      )
    )
  end

  {
    add = add,
    find = find,
    remove = remove,
    serve = serve,
    served = served,
    of_user = of_user
  }
end

# The listeners of the mount `entry`: the output's, and those whose connect
# event is running, which the output does not list yet (observability.md §3).
# @flag hidden
def icecast.server.listeners.current(entry) =
  listed = entry.output.listeners()
  ids = list.map(fun (l) -> l.id, listed)
  [
    ...listed,
    ...list.filter(fun (l) -> not list.mem(l.id, ids), entry.sessions.served())
  ]
end

# The mount's current metadata.
# @flag hidden
def icecast.server.listeners.metadata(entry) =
  entry.output.last_metadata() ?? []
end

# Listeners connected to every mount, as `max_clients` counts them.
# @flag hidden
def icecast.server.listeners.total(state) =
  list.fold(fun (n, entry) -> n + entry.stats.current(), 0, state.mounts.all())
end

# The listener record observers see (observability.md §2).
# @flag hidden
def icecast.server.listeners.record(state, p, entry, listener) =
  session = entry.sessions.find(listener.id)
  address = null.defined(session) ? null.get(session).address : (listener.ip)
  user = null.defined(session) ? null.get(session).user : ""
  hash = p.address_hash
  {
    id = listener.id,
    mount = entry.path,
    ip =
      try
        hash(address)
      catch err do
        log.important(
          label=state.name,
          "address_hash failed: #{err.message}"
        )
        ""
      end,
    user = user == "" ? null : user,
    user_agent = list.assoc.nullable("user-agent", listener.headers),
    referer = list.assoc.nullable("referer", listener.headers),
    uri = listener.uri,
    protocol = listener.protocol,
    connected_at = listener.connected_at,
    duration = listener.duration,
    bytes_sent = listener.bytes_sent
  }
end
