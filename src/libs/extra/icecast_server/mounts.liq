# Mount table of icecast.server (mounts.md §1 to §3, §6). Source clients and
# attached Liquidsoap sources become feeders through `attach` and leave
# through `detach` (feeders.liq); both, a reload, a `fallbacks` request and a
# listener request that creates a mount end with `refresh`, which points every
# mount at its fallback and shuts down the mounts left with nothing to send.
# Every change to the table runs under `icecast.server.locked`. Fallback chains
# and `refresh` are in mount_chain.liq, mount outputs in mount_outputs.liq.
let icecast.server.mounts = ()

# A mutable value read with `get()` and written with `set(x)`, for the
# server's shared state: a `ref` field that a function reads before writing it
# is typed as a plain getter, which has no `set`.
# @flag hidden
def icecast.server.cell(value) =
  r = ref(value)
  {get = fun () -> r(), set = fun (x) -> r := x}
end

# Runs `fn()` under the server's lock and returns its result; an error is
# raised again once the lock is released. The table's changes and a reload
# take it, and they never take it twice.
# @flag hidden
def icecast.server.locked(state, fn) =
  # ponytail: one global lock for every mount, per-mount locks if contention
  # shows.
  def rec acquire() =
    if
      atomic(fun () -> state.lock.exchange(true))
    then
      thread.pause(0.01)
      acquire()
    end
  end

  acquire()
  try
    result = fn()
    state.lock := false
    result
  catch err do
    state.lock := false
    error.raise(err, err.message)
  end
end

# The live mounts: `all()` lists them, `add(entry)` adds one, `remove(f)`
# removes those `f` selects and returns them.
# @flag hidden
def icecast.server.mounts.table() =
  entries = ref([])
  def add(entry) =
    atomic(fun () -> entries := [...entries(), entry])
  end

  def remove(f) =
    atomic(
      fun () ->
        begin
          removed = list.filter(f, entries())
          entries := list.filter(fun (e) -> not f(e), entries())
          removed
        end
    )
  end

  {all = fun () -> atomic(entries), add = add, remove = remove}
end

# A feeder: a source client (`is_client`) or an attached Liquidsoap source.
# `format` is `{container, codecs, mime, channels, samplerate}`, an unknown
# container or rate being `null`; `encoder(options)` is the copy encoder of its
# stream, `stop()` ends a source client's connection.
# @flag hidden
def icecast.server.mounts.feeder(
  ~is_client,
  ~identity,
  ~headers,
  ~ip,
  ~agent,
  ~format,
  ~encoder,
  ~stop,
  (source:source)
) =
  {
    is_client = is_client,
    identity = identity,
    headers = headers,
    ip = ip,
    agent = agent,
    format = format,
    encoder = encoder,
    stop = stop,
    source = source
  }
end

# The live mount at `path`, `null` when it has no output.
# @flag hidden
def icecast.server.mounts.find(state, path) =
  list.case(
    list.filter(fun (entry) -> entry.path == path, state.mounts.all()),
    null,
    fun (entry, _) -> entry
  )
end

# @flag hidden
def icecast.server.mounts.has_feeder(state, path) =
  entry = icecast.server.mounts.find(state, path)
  null.defined(entry) and null.defined(null.get(entry).feeder.get())
end

# Mounts fed by a source client, as `max_sources` counts them.
# @flag hidden
def icecast.server.mounts.source_clients(state) =
  list.length(
    list.filter(
      fun (entry) ->
        null.defined(entry.feeder.get())
        and null.get(entry.feeder.get()).is_client,
      state.mounts.all()
    )
  )
end

# Whether streams of formats `a` and `b` are packet-compatible (mounts.md
# §3): same MIME type, container and codecs where both are known, and same
# sample rate and channels for a container that fixes them in its header.
# @flag hidden
def icecast.server.mounts.matches(a, b) =
  def same(x, y) =
    not null.defined(x) or not null.defined(y) or null.get(x) == null.get(y)
  end

  container = null.defined(a.container) ? a.container : (b.container)
  fixed_layout =
    list.mem(null.get(default="", container), ["matroska", "webm", "flac"])
  a.mime == b.mime
  and (a.codecs == [] or b.codecs == [] or a.codecs == b.codecs)
  and same(a.container, b.container)
  and (
    not fixed_layout
    or (same(a.channels, b.channels) and same(a.samplerate, b.samplerate))
  )
end
