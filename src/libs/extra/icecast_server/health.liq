# `/health`, `/<mount>/health` (observability.md §7.4) and `/metrics` (§8) of
# icecast.server, rendered from the public mounts of the page view.

# The common name of a codec.
# @flag hidden
def icecast.server.pages.codec_name(codec) =
  list.assoc(
    default=codec,
    string.case(lower=true, codec),
    [
      ("mp3", "MP3"),
      ("aac", "AAC"),
      ("vorbis", "Vorbis"),
      ("opus", "Opus"),
      ("flac", "FLAC"),
      ("speex", "Speex")
    ]
  )
end

# The `/health` entry of `m`.
# @flag hidden
def icecast.server.pages.health_mount(m) =
  audio = m.audio
  format = json.object()
  list.iter(fun (field) -> format.add(fst(field), snd(field)), audio)
  format.add("content_type", m.mime_type)
  codecs = m.codecs
  list.iter(
    fun (codec) -> format.add("codec", icecast.server.pages.codec_name(codec)),
    list.prefix(1, codecs)
  )
  metadata = json.object()
  playing = icecast.server.stats.title(m.metadata)
  if
    null.defined(playing)
  then
    metadata.add("now_playing", null.get(playing))
  end
  entry = json.object()
  entry.add("format", format)
  entry.add("listener_count", list.length(m.listeners))
  entry.add("metadata", metadata)
  entry
end

# The body of `/health`.
# @flag hidden
def icecast.server.pages.health(view) =
  mounts = view.public_mounts
  entries = json.object()
  list.iter(
    fun (m) ->
      begin
        entry = icecast.server.pages.health_mount(m)
        entries.add(m.mount, entry)
      end,
    mounts
  )
  health = json.object()
  health.add("last_config_reload", int_of_float(view.last_reload + 0.5))
  health.add("mounts", entries)
  health.add(
    "total_listener_count",
    list.fold(fun (n, m) -> n + list.length(m.listeners), 0, mounts)
  )
  health.add(
    "total_source_count",
    list.length(list.filter(fun (m) -> m.has_source, mounts))
  )
  health.stringify(compact=true)
end

# A Prometheus label value.
# @flag hidden
def icecast.server.pages.label(value) =
  r/[\\"\n]/g.replace(fun (c) -> c == "\n" ? "\\n" : "\\#{c}", value)
end

# The body of `/metrics`: counts as integers, times with three decimals.
# @flag hidden
def icecast.server.pages.metrics(view) =
  mounts = view.public_mounts
  def seconds(x) =
    string.float(decimal_places=3, x)
  end

  def metric(name, kind, values) =
    "# TYPE #{name} #{kind}\n#{string.concat(values)}"
  end

  def global(name, kind, value) =
    metric(
      name,
      kind,
      [
        "#{name} #{value}\n"
      ]
    )
  end

  def per_mount(name, kind, get) =
    metric(
      name,
      kind,
      list.map(
        fun (m) ->
          "#{name}{mount=\"#{icecast.server.pages.label(m.mount)}\"} #{
            get(m)
          }\n",
        mounts
      )
    )
  end

  string.concat(
    [
      global(
        "icecast_server_start_time_seconds",
        "gauge",
        seconds(view.server_start)
      ),
      global(
        "icecast_server_last_reload_time_seconds",
        "gauge",
        seconds(view.last_reload)
      ),
      global("icecast_server_mounts", "gauge", "#{list.length(mounts)}"),
      per_mount(
        "icecast_server_source_up",
        "gauge",
        fun (m) -> m.has_source ? "1" : "0"
      ),
      per_mount(
        "icecast_server_source_start_time_seconds",
        "gauge",
        fun (m) -> seconds(m.started)
      ),
      per_mount(
        "icecast_server_listeners",
        "gauge",
        fun (m) -> "#{list.length(m.listeners)}"
      ),
      per_mount(
        "icecast_server_listeners_peak",
        "gauge",
        fun (m) -> "#{m.peak_listeners}"
      ),
      per_mount(
        "icecast_server_listener_connections_total",
        "counter",
        fun (m) -> "#{m.connections}"
      ),
      per_mount(
        "icecast_server_listening_seconds_total",
        "counter",
        fun (m) -> seconds(m.listening_time)
      ),
      per_mount(
        "icecast_server_sent_bytes_total",
        "counter",
        fun (m) -> "#{m.bytes_sent}"
      )
    ]
  )
end
