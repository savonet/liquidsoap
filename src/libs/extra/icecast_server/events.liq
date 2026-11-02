# Events of icecast.server (observability.md §1, §3). The server creates
# one registry per event with `icecast.server.events.create`, exposes each
# registry's `add` as an `on_*` method, and calls `fire` with the event's
# record and the `event_bindings` read at that use.
let icecast.server.events = ()

# Runs `handler()`, in order when `synchronous`, else in a thread of its own;
# a failure is sent to `log` and changes nothing.
# @flag hidden
def icecast.server.events.call(
  ~log=fun (message) -> log.severe(label="icecast.server", message),
  ~synchronous,
  handler
) =
  def run() =
    try
      handler()
    catch err do
      log(
        "Event handler failed: #{err.message}"
      )
    end
  end

  if synchronous then run() else thread.run(run) end
end

# A registry of the handlers of the event named `event`: `add` registers a
# handler, `fire(~bindings, ~mount, ~listeners, record)` calls the registered
# handlers with `record`, then the bindings of that event and mount with
# `{event, mount, listeners, compatibility}`, `listeners` being the mount's
# count after the event: one binding list serves the five events, so its handlers take one
# record type.
# @flag hidden
def icecast.server.events.registry(~log, ~compatibility, event) =
  handlers = ref([])

  def add(~synchronous=false, handler) =
    atomic(
      fun () ->
        handlers :=
          [...handlers(), {synchronous = synchronous, handler = handler}]
    )
  end

  def fire(~bindings, ~mount, ~listeners, record) =
    list.iter(
      fun (h) ->
        icecast.server.events.call(
          log=log,
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
            log=log,
            synchronous=b?.synchronous ?? false,
            fun () ->
              b.handler(
                {
                  event = event,
                  mount = mount,
                  listeners = listeners,
                  compatibility = compatibility
                }
              )
          )
        end,
      bindings
    )
  end

  {add = add, fire = fire}
end

# A registry for each of `icecast.server.event_names`.
# @flag hidden
def icecast.server.events.create(~log, ~compatibility) =
  def registry(event) =
    icecast.server.events.registry(log=log, compatibility=compatibility, event)
  end

  {
    source_connect = registry("source_connect"),
    source_disconnect = registry("source_disconnect"),
    listener_connect = registry("listener_connect"),
    listener_disconnect = registry("listener_disconnect"),
    metadata = registry("metadata")
  }
end
