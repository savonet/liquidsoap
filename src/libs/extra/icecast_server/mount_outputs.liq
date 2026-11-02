# Mount outputs of icecast.server (mounts.md §1, §4, §8, §9; harbor
# icecast-server.md §2). `icecast.server.mounts.create` builds a mount's entry
# and its harbor output, reading the mount output parameters once; the output
# admits listeners through listeners.liq and follows its feeder, else its
# fallback, switching at the next frame. An output keeps `buffer_size` bytes of
# stream for its listeners: output.harbor's default `buffer`, raised to the
# burst when the burst is larger.
let icecast.server.mounts.buffer_size = 5 * 65535

# A source playing `get()`, switching at once and replaying the metadata of
# the source it switches to.
# @flag hidden
def icecast.server.mounts.follow(id, get) =
  source.dynamic(id=id, track_sensitive=false, get).{
    track_sensitive = {false},
    replay_metadata = {true},
    on_select = fun (switch) -> switch.starting,
    on_leave = fun (_) -> ()
  }
end

# Puts the clock `c` of a new output on the server's clock and starts it, so
# that an output created by a running handler or thread serves at once. That
# clock stops with its last output; one that is stopping merges with nothing,
# and the server then takes a new clock.
# @flag hidden
def icecast.server.mounts.unify_clock(state, c) =
  try
    state.clock.get().unify(c)
  catch err do
    icecast.server.log.info(
      state,
      "Taking a new clock: #{err.message}"
    )
    fresh = clock.create(id=state.name)
    state.clock.set(fresh)
    fresh.unify(c)
  end
  try
    state.clock.get().start(force=false)
  catch err do
    icecast.server.log.debug(
      state,
      "Clock not started: #{err.message}"
    )
  end
end

# Muxer options of the container `format` under the parameters `p`.
# @flag hidden
def icecast.server.mounts.muxer_options(p, format) =
  if
    not null.defined(format)
  then
    []
  elsif
    null.defined(p?.muxer_options)
  then
    muxer_options = null.get(p?.muxer_options)
    muxer_options(null.get(format))
  else
    list.assoc(
      default=[],
      null.get(format),
      settings.icecast.server.default_muxer_options()
    )
  end
end

# The mount's metadata update: its history and playlist log line, then the
# metadata event.
# @flag hidden
def icecast.server.mounts.on_metadata(state, entry, metadata) =
  p = state.params.get()
  max_history =
    try
      icecast.server.parameters.mount(p, entry.path).max_history
    catch err do
      icecast.server.log.debug(
        state,
        "Mount #{entry.path} keeps the server's max_history: #{err.message}"
      )
      getter.get(p.max_history)
    end
  title = icecast.server.stats.title(metadata)
  if
    null.defined(title)
  then
    entry.stats.add_title(max_history=max_history, null.get(title))
  end
  state.logs.playlist(
    mount=entry.path,
    listeners=entry.stats.current(),
    metadata
  )
  state.events.metadata.fire(
    bindings=p.event_bindings(),
    mount=entry.path,
    listeners=entry.stats.current(),
    {mount = entry.path, metadata = metadata, listeners = entry.stats.current()}
  )
end

# The `icy-*` identity of the mount's replies (mounts.md §4): its feeder's,
# else the fallback's it follows, else a lingering feeder's.
# @flag hidden
def icecast.server.mounts.identity(feeder, fallback, lingering) =
  if
    null.defined(feeder)
    and (
      null.get(feeder).is_client
      or source.is_ready(null.get(feeder).source)
      or not null.defined(fallback)
    )
  then
    null.get(feeder).identity
  elsif
    null.defined(fallback)
  then
    null.get(fallback).identity
  elsif
    null.defined(lingering)
  then
    null.get(lingering).identity
  else
    []
  end
end

# A new mount entry for `path` under the parameters `p`, without a feeder, its
# output created for the format and encoder of the feeder `template`.
# @flag hidden
def icecast.server.mounts.create(state, p, path, template) =
  mount = icecast.server.parameters.mount(p, path)
  id = string.residual(prefix="/", path) ?? path
  feeder_ref = icecast.server.cell(null)
  fallback_ref = icecast.server.cell(null)
  lingering = icecast.server.cell(null)
  nothing = source.fail()
  selector =
    fallback(
      id="#{id}.selector",
      [
        icecast.server.mounts.follow(
          "#{id}.feeder",
          fun () -> null.map(fun (f) -> f.source, feeder_ref.get()) ?? nothing
        ),
        icecast.server.mounts.follow(
          "#{id}.fallback",
          fun () -> null.map(fun (f) -> f.source, fallback_ref.get()) ?? nothing
        )
      ]
    )
  entry_ref = ref(null)
  burst = p.burst_on_connect and mount.burst_size > 0 ? mount.burst_size : 0
  o =
    output.harbor(
      id="#{id}.output",
      fallible=true,
      keep_listeners=true,
      override_mount=true,
      mount=path,
      port=state.port,
      transport=state.transport,
      timeout=p.client_timeout,
      burst=burst > 0 ? burst : null,
      buffer=max(icecast.server.mounts.buffer_size, burst),
      dedicated_encoder=p.dedicated_encoder,
      format=template.format.mime,
      accept=fun (listener) ->
        icecast.server.listeners.accept(state, null.get(entry_ref()), listener),
      template.encoder(
        icecast.server.mounts.muxer_options(p, template.format.container)
      ),
      selector
    )
  icecast.server.mounts.unify_clock(state, o.clock)
  entry =
    {
      path = path,
      output = o,
      insert_metadata = fun (metadata) -> selector.insert_metadata(metadata),
      format = template.format,
      headers =
        list.map(
          fun (e) ->
            {
              name = e.name,
              value = e?.value,
              compute = e?.compute,
              status = e?.status
            },
          [...p.http_headers, ...mount.http_headers]
        ),
      stats = icecast.server.stats.mount(),
      sessions = icecast.server.listeners.sessions(),
      feeder = feeder_ref,
      fallback = fallback_ref,
      lingering = lingering,
      started = icecast.server.cell(time()),
      disconnect =
        icecast.server.cell(
          {on_disconnect = mount?.on_disconnect, dump = fun () -> ()}
        ),
      identity =
        fun () ->
          icecast.server.mounts.identity(
            feeder_ref.get(),
            fallback_ref.get(),
            lingering.get()
          ),
      audio =
        fun () ->
          begin
            f = feeder_ref.get()
            {
              channels = null.defined(f) ? null.get(f).format.channels : null,
              samplerate =
                null.defined(f) ? null.get(f).format.samplerate : null
            }
          end
    }
  entry_ref := entry
  o.on_connect(
    synchronous=true,
    fun (listener) -> icecast.server.listeners.connected(state, entry, listener)
  )
  o.on_disconnect(
    synchronous=true,
    fun (listener) ->
      icecast.server.listeners.disconnected(state, entry, listener)
  )
  o.on_metadata(
    synchronous=true,
    fun (metadata) -> icecast.server.mounts.on_metadata(state, entry, metadata)
  )
  entry
end
