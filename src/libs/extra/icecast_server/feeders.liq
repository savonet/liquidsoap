# Feeders of icecast.server mounts (mounts.md §1.1, §2; sources.md §8). A
# source client's connection (sources.liq) and `attach_source` call `attach`;
# its disconnection and `detach_source` call `detach`. Both change the mount
# table under the server's lock, then raise the mount's source callbacks and
# the source events outside it.
let icecast.server.feeders = ()

# The source-connect and source-disconnect records of `feeder` on `path`.
# @flag hidden
def icecast.server.feeders.event_record(~server, path, feeder) =
  {mount = path, ip = feeder.ip, agent = feeder.agent, server = server}
end

# Calls a mount's nullable source callback in the background.
# @flag hidden
def icecast.server.feeders.run_callback(callback, record) =
  if
    null.defined(callback)
  then
    callback = null.get(callback)
    icecast.server.events.call(synchronous=false, fun () -> callback(record))
  end
end

# Makes `feeder` the feeder of `path`, creating or replacing its output
# (mounts.md §3), and raises its connect callbacks and events. `false` when the
# mount already has a feeder, or a source client would pass `max_sources`. With
# `strict`, a mount output of another format
# raises `error.icecast.server.format_mismatch` instead of being replaced.
# @flag hidden
def icecast.server.feeders.attach(state, ~strict=false, path, feeder) =
  def take() =
    p = state.params.get()
    mount = icecast.server.parameters.mount(p, path)
    current = icecast.server.mounts.find(state, path)
    continued =
      null.defined(current)
      and icecast.server.mounts.matches(null.get(current).format, feeder.format)
    max_sources = p.max_sources()
    full =
      feeder.is_client
      and null.defined(max_sources)
      and icecast.server.mounts.source_clients(state) >= null.get(max_sources)
    if
      full
      or (
        null.defined(current) and null.defined(null.get(current).feeder.get())
      )
    then
      null
    else
      # Values a getter or the dump can fail on are read before the table
      # changes, so a failure leaves the mount as it was.
      bindings = p.event_bindings()
      server = state.server(p.hostname())
      if
        null.defined(current) and not continued
      then
        if
          strict
        then
          error.raise(
            error.icecast.server.format_mismatch,
            "#{path} has an output of another format"
          )
        end
        old = null.get(current)
        ignore(state.mounts.remove(fun (e) -> e.path == path))
        icecast.server.log.important(
          state,
          "Mount #{path}: new stream format, replacing its output."
        )
        old.output.shutdown()
      end
      if
        not continued
      then
        state.mounts.add(icecast.server.mounts.create(state, p, path, feeder))
      end
      entry = null.get(icecast.server.mounts.find(state, path))
      dump = icecast.server.mounts.dump(state, mount, feeder)
      entry.feeder.set(feeder)
      entry.lingering.set(null)
      entry.started.set(time())
      entry.disconnect.set({on_disconnect = mount?.on_disconnect, dump = dump})
      icecast.server.mounts.refresh(state)
      state.counters.source_connected()
      {
        bindings = bindings,
        listeners = entry.stats.current(),
        server = server,
        on_connect = mount?.on_connect
      }
    end
  end

  taken = icecast.server.locked(state, take)
  if
    null.defined(taken)
  then
    taken = null.get(taken)
    record =
      icecast.server.feeders.event_record(server=taken.server, path, feeder).{
        source = feeder.source,
        format = null.get(default=feeder.format.mime, feeder.format.container),
        headers = feeder.headers
      }
    icecast.server.feeders.run_callback(taken.on_connect, record)
    state.events.source_connect.fire(
      bindings=taken.bindings,
      mount=path,
      listeners=taken.listeners,
      record
    )
  end
  null.defined(taken)
end

# Ends the linger of `entry` started for `feeder`, unless a feeder took the
# mount meanwhile.
# @flag hidden
def icecast.server.feeders.end_linger(state, entry, feeder) =
  icecast.server.locked(
    state,
    fun () ->
      begin
        lingering = entry.lingering.get()
        if
          null.defined(lingering)
          and source.id(null.get(lingering).source) == source.id(feeder.source)
        then
          entry.lingering.set(null)
          icecast.server.mounts.refresh(state)
        end
      end
  )
end

# Ends `feeder`'s feed of `path`, unless another feeder took the mount, and
# raises its disconnect callbacks and events; a mount left with nothing to send
# lingers for its `linger`.
# @flag hidden
def icecast.server.feeders.detach(state, path, feeder) =
  def leave() =
    entry = icecast.server.mounts.find(state, path)
    current = null.defined(entry) ? null.get(entry).feeder.get() : null
    if
      null.defined(current)
      and source.id(null.get(current).source) == source.id(feeder.source)
    then
      entry = null.get(entry)
      p = state.params.get()
      entry.feeder.set(null)
      disconnect = entry.disconnect.get()
      disconnect.dump()
      linger =
        try
          icecast.server.parameters.mount(p, path).linger ?? 0.
        catch err do
          icecast.server.log.debug(
            state,
            "Mount #{path} does not linger: #{err.message}"
          )
          0.
        end
      entry.fallback.set(icecast.server.mounts.resolve(state, p, entry))
      if
        linger > 0. and not null.defined(entry.fallback.get())
      then
        entry.lingering.set(feeder)
        thread.run(
          delay=linger,
          fun () -> icecast.server.feeders.end_linger(state, entry, feeder)
        )
      end
      icecast.server.mounts.refresh(state)
      {
        bindings = p.event_bindings(),
        listeners = entry.stats.current(),
        server = state.server(p.hostname()),
        on_disconnect = disconnect.on_disconnect
      }
    else
      null
    end
  end

  left = icecast.server.locked(state, leave)
  if
    null.defined(left)
  then
    left = null.get(left)
    record =
      icecast.server.feeders.event_record(server=left.server, path, feeder)
    icecast.server.feeders.run_callback(left.on_disconnect, record)
    state.events.source_disconnect.fire(
      bindings=left.bindings,
      mount=path,
      listeners=left.listeners,
      record
    )
  end
end
