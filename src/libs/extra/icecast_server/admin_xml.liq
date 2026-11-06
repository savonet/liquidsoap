# Icecast's XML admin pages of icecast.server (observability.md §7.3):
# `/admin/stats`, `/admin/listmounts` and `/admin/listclients`, hidden mounts
# included.

# `value` as XML text: escaped, without the control bytes XML 1.0 forbids.
# @flag hidden
def icecast.server.pages.xml_text(value) =
  icecast.server.pages.html(
    r/[\x00-\x08\x0b\x0c\x0e-\x1f]/g.replace(fun (_) -> "", value)
  )
end

# `<name>value</name>`, the value as text.
# @flag hidden
def icecast.server.pages.element(name, value) =
  "<#{name}>#{icecast.server.pages.xml_text(value)}</#{name}>"
end

# The elements of the `(name, value)` pairs whose value is not `null`.
# @flag hidden
def icecast.server.pages.elements(fields) =
  string.concat(
    list.filter_map(
      fun (field) ->
        null.map(
          fun (value) -> icecast.server.pages.element(fst(field), value),
          snd(field)
        ),
      fields
    )
  )
end

# The `<icestats>` document of `body`.
# @flag hidden
def icecast.server.pages.xml_document(body) =
  "<?xml version=\"1.0\"?>\n<icestats>#{body}</icestats>\n"
end

# @flag hidden
def icecast.server.pages.source_element(mount, body) =
  "<source mount=\"#{icecast.server.pages.xml_text(mount)}\">#{body}</source>"
end

# Seconds since `t`, as a decimal string.
# @flag hidden
def icecast.server.pages.seconds_since(t) =
  "#{int_of_float(time() - t)}"
end

# The `<source>` of `m` in `/admin/stats`.
# @flag hidden
def icecast.server.pages.stats_source(info, request, m) =
  audio = list.map(fun (field) -> "ice-#{fst(field)}=#{snd(field)}", m.audio)
  date = icecast.server.pages.date
  live_bytes = list.fold(fun (n, l) -> n + l.bytes_sent, 0, m.listeners)
  def number(name) =
    null.map(fun (n) -> "#{n}", list.assoc.nullable(name, m.audio))
  end

  icecast.server.pages.source_element(
    m.mount,
    icecast.server.pages.elements(
      [
        (
          "listenurl",
          (icecast.server.pages.listen_url(info, request, m.mount) : string?)
        ),
        ("listeners", "#{list.length(m.listeners)}"),
        ("listener_peak", "#{m.peak_listeners}"),
        ("server_name", m.name),
        ("server_description", m.description),
        ("server_type", m.mime_type),
        ("server_url", m.url),
        ("genre", m.genre),
        ("stream_start", date(m.started)),
        ("stream_start_iso8601", date(iso=true, m.started)),
        ("title", icecast.server.stats.title(m.metadata)),
        ("bitrate", number("bitrate")),
        ("ice-bitrate", number("bitrate")),
        ("channels", number("channels")),
        ("samplerate", number("samplerate")),
        (
          "audio_info",
          audio == [] ? null : (string.concat(separator=";", audio))
        ),
        ("max_listeners", m.max_listeners),
        ("public", m.public ? "1" : "0"),
        ("slow_listeners", "0"),
        ("total_bytes_sent", "#{m.bytes_sent + live_bytes}"),
        ("total_bytes_read", "#{m.bytes_read}"),
        ("connected", icecast.server.pages.seconds_since(m.started)),
        ("source_ip", m.source_ip == "" ? null : (m.source_ip)),
        ("user_agent", m.source_ip == "" ? null : (m.source_agent))
      ]
    )
  )
end

# The body of `/admin/stats`.
# @flag hidden
def icecast.server.pages.admin_stats(info, view, request) =
  # The mounts are rendered before any other read of their fields: the
  # typechecker rejects the other order.
  mounts =
    string.concat(
      list.map(
        fun (m) -> icecast.server.pages.stats_source(info, request, m),
        view.mounts
      )
    )
  date = icecast.server.pages.date
  sources = list.length(list.filter(fun (m) -> m.has_source, view.mounts))
  listeners =
    list.fold(fun (n, m) -> n + list.length(m.listeners), 0, view.mounts)
  connections = view.listener_connections + view.source_connections
  icecast.server.pages.xml_document(
    icecast.server.pages.elements(
      [
        ("admin", (info.admin_contact : string?)),
        ("host", (info.hostname : string)),
        ("location", (info.location : string)),
        ("server_id", (info.server_id : string)),
        ("server_start", date(view.server_start)),
        ("server_start_iso8601", date(iso=true, view.server_start)),
        ("sources", "#{sources}"),
        ("listeners", "#{listeners}"),
        ("clients", "#{listeners + sources}"),
        ("listener_connections", "#{view.listener_connections}"),
        ("source_client_connections", "#{view.source_connections}"),
        ("source_total_connections", "#{view.source_connections}"),
        ("client_connections", "#{connections}"),
        ("connections", "#{connections}"),
        ("file_connections", "0"),
        ("stats", "0"),
        ("stats_connections", "0"),
        ("banned_IPs", "0")
      ]
    ) ^
      mounts
  )
end

# The body of `/admin/listmounts`.
# @flag hidden
def icecast.server.pages.listmounts(view) =
  def mount(m) =
    count = "#{list.length(m.listeners)}"
    icecast.server.pages.source_element(
      m.mount,
      icecast.server.pages.elements(
        [
          ("listeners", (count : string?)),
          ("Listeners", count),
          ("connected", icecast.server.pages.seconds_since(m.started)),
          ("content-type", m.mime_type),
          ("fallback", m.fallback)
        ]
      )
    )
  end

  icecast.server.pages.xml_document(string.concat(list.map(mount, view.mounts)))
end

# `/admin/listclients`, a mount command (sources.md §6).
# @flag hidden
def icecast.server.pages.listclients(state, request) =
  p = state.params.get()
  def listener(entry, l) =
    r = icecast.server.listeners.record(state, p, entry, l)
    "<listener>#{
      icecast.server.pages.elements(
        [
          ("ID", ("#{r.id}" : string?)),
          ("IP", r.ip),
          ("UserAgent", r.user_agent ?? ""),
          ("Connected", "#{int_of_float(r.duration)}"),
          ("referer", r.referer),
          ("username", r.user)
        ]
      )
    }</listener>"
  end

  def list_clients(entry) =
    listeners = icecast.server.listeners.current(entry)
    http.response(
      content_type="text/xml",
      data=icecast.server.pages.xml_document(
        icecast.server.pages.source_element(
          entry.path,
          icecast.server.pages.element(
            "Listeners",
            "#{list.length(listeners)}"
          ) ^
            string.concat(list.map(fun (l) -> listener(entry, l), listeners))
        )
      )
    )
  end

  icecast.server.source_admin.run(
    state,
    p=p,
    path=list.assoc(default="", "mount", request.query),
    request,
    list_clients
  )
end
