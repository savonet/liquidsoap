# Statistics of icecast.server (observability.md §4, §5, §13). Each mount
# output owns the counters `icecast.server.stats.mount()` creates: the listener
# pipeline counts sessions in them, the metadata path adds titles to its history.
# `icecast.server.snapshot` (snapshot.liq) renders the server's mounts for
# `stats()`.
let icecast.server.stats = ()

# Listener counters and metadata history of one mount output.
# @flag hidden
def icecast.server.stats.mount() =
  state =
    ref(
      {
        current = 0,
        peak = 0,
        connections = 0,
        listening_time = 0.,
        bytes_sent = 0,
        history = ([] : [{title: string, time: float}])
      }
    )

  def connect() =
    atomic(
      fast=true,
      fun () ->
        begin
          s = state()
          current = s.current + 1
          state :=
            s.{
              current = current,
              connections = s.connections + 1,
              peak = max(s.peak, current)
            }
        end
    )
  end

  def disconnect(listener) =
    atomic(
      fast=true,
      fun () ->
        begin
          s = state()
          state :=
            s.{
              current = s.current - 1,
              listening_time = s.listening_time + listener.duration,
              bytes_sent = s.bytes_sent + listener.bytes_sent
            }
        end
    )
  end

  def add_title(~max_history, title) =
    atomic(
      fast=true,
      fun () ->
        begin
          s = state()
          history = [{title = title, time = time()}, ...s.history]
          state := s.{history = list.prefix(max_history, history)}
        end
    )
  end

  {
    connect = connect,
    disconnect = disconnect,
    add_title = add_title,
    get = fun () -> atomic(fast=true, state),
    current = fun () -> atomic(fast=true, state).current
  }
end

# The text of `metadata` (observability.md §11): `artist - title` when both
# are non-empty, else the one that is, `null` when neither is.
# @flag hidden
def icecast.server.stats.title(metadata) =
  def field(name) =
    value = list.assoc(default="", name, metadata)
    value == "" ? null : value
  end

  title = field("title")
  artist = field("artist")
  if
    null.defined(title) and null.defined(artist)
  then
    "#{null.get(artist)} - #{null.get(title)}"
  else
    title ?? artist
  end
end

# The statistics of the mount `entry` (see mounts.liq) with its current
# `listeners`, read together with its counters. `listener(entry, l)` gives the
# listener record of the harbor listener `l`.
# @flag hidden
def icecast.server.stats.of_mount(
  ~listener,
  ~listeners,
  ~hidden,
  ~fallback,
  entry
) =
  counters = entry.stats.get()
  feeder = entry.feeder.get()
  identity = entry.identity()
  audio = entry.audio()
  def field(name) =
    list.assoc(default="", name, identity)
  end

  def number(name) =
    value = list.assoc.nullable(name, identity)
    null.defined(value) ? int_of_string(default=0, null.get(value)) : null
  end

  {
    mount = entry.path,
    listeners = list.map(fun (l) -> listener(entry, l), listeners),
    peak_listeners = counters.peak,
    connections = counters.connections,
    listening_time = counters.listening_time,
    bytes_sent = counters.bytes_sent,
    bytes_read = null.defined(feeder) ? null.get(feeder).bytes_read() : 0,
    metadata = entry.output.last_metadata() ?? [],
    history = counters.history,
    name = field("icy-name"),
    description = field("icy-description"),
    genre = field("icy-genre"),
    url = field("icy-url"),
    public = field("icy-pub") == "1",
    content_type = null.get(default="", entry.format.container),
    mime_type = entry.format.mime,
    bitrate = number("icy-br"),
    channels = audio.channels,
    samplerate = audio.samplerate,
    started = entry.started.get(),
    fallback = fallback,
    hidden = hidden
  }
end

# Server-wide counters of observability.md §4.
# @flag hidden
def icecast.server.counters() =
  started = time()
  state =
    ref(
      {last_reload = started, listener_connections = 0, source_connections = 0}
    )
  def update(f) =
    atomic(fast=true, fun () -> state := f(state()))
  end

  {
    get = fun () -> atomic(fast=true, state).{server_start = started},
    reloaded = fun () -> update(fun (s) -> s.{last_reload = time()}),
    listener_connected =
      fun () ->
        update(
          fun (s) -> s.{listener_connections = s.listener_connections + 1}
        ),
    source_connected =
      fun () ->
        update(fun (s) -> s.{source_connections = s.source_connections + 1})
  }
end
