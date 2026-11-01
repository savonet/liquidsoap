# Events of icecast.server (observability.md §1, §3). The server creates
# one registry per event with `icecast.server.events.create`, exposes each
# registry's `add` as an `on_*` method, and calls `fire` with the event's
# record and the `event_bindings` read at that use.
let icecast.server.events = ()

# Runs `handler()`, in order when `synchronous`, else in a thread of its own;
# a failure is logged and changes nothing.
# @flag hidden
def icecast.server.events.call(~synchronous, handler) =
  def run() =
    try
      handler()
    catch err do
      log.severe(
        label="icecast.server",
        "Event handler failed: #{err.message}"
      )
    end
  end

  if synchronous then run() else thread.run(run) end
end

# A registry of the handlers of the event named `event`: `add` registers a
# handler, `fire(~bindings, ~mount, record)` calls the registered handlers
# with `record`, then the bindings of that event and mount with `{event,
# mount}`: one binding list serves the five events, so its handlers take one
# record type.
# @flag hidden
def icecast.server.events.registry(event) =
  handlers = ref([])

  def add(~synchronous=false, handler) =
    atomic(
      fun () ->
        handlers :=
          [...handlers(), {synchronous = synchronous, handler = handler}]
    )
  end

  def fire(~bindings, ~mount, record) =
    list.iter(
      fun (h) ->
        icecast.server.events.call(
          synchronous=h.synchronous,
          fun () -> h.handler(record)
        ),
      handlers()
    )
    list.iter(
      fun (b) ->
        if
          b.event == event and (b?.mount ?? mount) == mount
        then
          icecast.server.events.call(
            synchronous=b?.synchronous ?? false,
            fun () -> b.handler({event = event, mount = mount})
          )
        end,
      bindings
    )
  end

  {add = add, fire = fire}
end

# The registries of the five events.
# @flag hidden
def icecast.server.events.create() =
  {
    source_connect = icecast.server.events.registry("source_connect"),
    source_disconnect = icecast.server.events.registry("source_disconnect"),
    listener_connect = icecast.server.events.registry("listener_connect"),
    listener_disconnect = icecast.server.events.registry("listener_disconnect"),
    metadata = icecast.server.events.registry("metadata")
  }
end
