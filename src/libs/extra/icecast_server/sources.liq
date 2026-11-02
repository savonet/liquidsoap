# Source clients of icecast.server (sources.md §1 to §5, §8). The server's
# harbor input calls `icecast.server.sources.accept` (source_admission.liq)
# for every source request and, once it is admitted,
# `icecast.server.sources.connect` with the connection; the feeder that
# `connect` builds goes to feeders.liq, which updates the mount table, raises
# the events and runs the mount's callbacks.
let icecast.server.sources = ()

# Whether no source client may feed `path`: the server's pages and endpoints.
# @flag hidden
def icecast.server.sources.is_reserved(path) =
  list.mem(
    path,
    ["/", "/status.json", "/status-json.xsl", "/admin", "/admin.cgi", "/metrics"
    ]
  )
  or string.contains(prefix="/admin/", path)
  or string.contains(suffix="/health", path)
end

# The `icy-*` stream identity of a source client's request headers: `ice-*`
# and `icy-*` names alike, the first one received of each, without a value
# that has a line break.
# @flag hidden
def icecast.server.sources.identity(headers) =
  names =
    [
      ("name", "icy-name"),
      ("description", "icy-description"),
      ("genre", "icy-genre"),
      ("url", "icy-url"),
      ("bitrate", "icy-br"),
      ("br", "icy-br"),
      ("public", "icy-pub"),
      ("pub", "icy-pub")
    ]
  def add(identity, header) =
    let (name, value) = header
    name = string.case(lower=true, name)
    suffix =
      string.residual(prefix="ice-", name)
        ?? (string.residual(prefix="icy-", name) ?? "")
    target = list.assoc.nullable(suffix, names)
    if
      not null.defined(target)
      or list.assoc.mem(null.get(target), identity)
      or not icecast.server.headers.is_line(value)
    then
      identity
    else
      [...identity, (null.get(target), value)]
    end
  end

  list.fold(add, [], headers)
end

# Value of the first stream for which `get` gives one.
# @flag hidden
def icecast.server.sources.stream_field(streams, get) =
  list.fold(
    fun (found, stream) -> null.defined(found) ? found : get(stream),
    null,
    streams
  )
end

# The handler harbor calls with the client's source once `connection` is
# admitted: the client becomes the feeder of its mount, until it disconnects.
# @flag hidden
def icecast.server.sources.connect(state, connection) =
  fun (s) ->
    begin
      on_disconnect = s.on_disconnect
      p = state.params.get()
      path = connection.uri == "/" ? p.shoutcast_mount() : (connection.uri)
      streams = connection.streams
      feeder =
        icecast.server.mounts.feeder(
          is_client=true,
          identity=icecast.server.sources.identity(connection.headers),
          headers=connection.headers,
          ip=s.connected_client() ?? "",
          agent=list.assoc(
            default="-",
            "user-agent",
            icecast.server.headers.lowercase(connection.headers)
          ),
          format={
            container = connection.format,
            codecs = list.map(fun (stream) -> stream.codec, streams),
            mime = connection.mime_type ?? "audio/mpeg",
            channels =
              icecast.server.sources.stream_field(
                streams,
                fun (stream) -> stream?.channels
              ),
            samplerate =
              icecast.server.sources.stream_field(
                streams,
                fun (stream) -> stream?.samplerate
              )
          },
          encoder=fun (options) ->
            connection.copy_encoder(connection.format, options=options),
          stop=s.stop,
          bytes_read=s.bytes_read,
          s
        )
      quoted =
        icecast.server.log_lines.client_value(
          compatibility=state.compatibility,
          path
        )
      on_disconnect(
        synchronous=true,
        fun () ->
          begin
            icecast.server.feeders.detach(state, path, feeder)
            icecast.server.log.info(
              state,
              category="source/source_main",
              "Source at \"#{quoted}\" exiting"
            )
          end
      )
      icecast.server.log.info(
        state,
        category="connection-handle/_handle_source_request",
        "Source logging in at mountpoint \"#{quoted}\""
      )
      if
        not icecast.server.feeders.attach(state, path, feeder)
      then
        icecast.server.log.important(
          state,
          "Mount #{quoted} was taken meanwhile, dropping the source client."
        )
        s.stop()
      end
    end
end
