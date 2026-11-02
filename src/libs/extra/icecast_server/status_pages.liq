# The public status pages of icecast.server (observability.md §6): `/`,
# `/status.json` and `/status-json.xsl`, rendered from the public mounts of
# the page view (page_helpers.liq).

# The built-in body of `/`, listing the public mounts of the page view.
# @flag hidden
def icecast.server.pages.status_html(view) =
  html = icecast.server.pages.html
  def mount(m) =
    name = m.name == "" ? m.mount : (m.name)
    playing = icecast.server.stats.title(m.metadata) ?? ""
    "<section><h2>#{html(name)}</h2><dl><dt>Mount</dt><dd>#{
      html(m.mount)
    }</dd><dt>Format</dt><dd>#{html(m.mime_type)}</dd><dt>Started</dt><dd>#{
      html(icecast.server.pages.date(m.started))
    }</dd><dt>Now playing</dt><dd>#{html(playing)}</dd><dt>Listeners</dt><dd>#{
      list.length(m.listeners)
    }</dd><dt>Peak</dt><dd>#{m.peak_listeners}</dd></dl><audio controls \
     preload=\"none\" src=\"#{html(m.mount)}\"></audio></section>"
  end

  mounts = view.public_mounts
  "<!DOCTYPE html>\n<html><head><meta \
   charset=\"utf-8\"><title>Status</title></head><body><h1>Status</h1>#{
    string.concat(list.map(mount, mounts))
  }</body></html>\n"
end

# The built-in body of `/status.json`, listing the public mounts of the page
# view.
# @flag hidden
def icecast.server.pages.status_json(view) =
  def mount(m) =
    {
      mount = m.mount,
      name = m.name,
      content_type = m.content_type,
      mime_type = m.mime_type,
      started = m.started,
      listener_count = list.length(m.listeners),
      peak_listeners = m.peak_listeners,
      current_metadata =
        list.map(fun (kv) -> {key = fst(kv), value = snd(kv)}, m.metadata)
    }
  end

  mounts = view.public_mounts
  json.stringify({mounts = list.map(mount, mounts)})
end

# The `source` object of `m` in `/status-json.xsl`.
# @flag hidden
def icecast.server.pages.icestats_source(info, request, m) =
  audio = m.audio
  date = icecast.server.pages.date
  j = json.object()
  j.add("listenurl", icecast.server.pages.listen_url(info, request, m.mount))
  j.add("listeners", list.length(m.listeners))
  j.add("listener_peak", m.peak_listeners)
  j.add("server_name", m.name)
  j.add("server_description", m.description)
  j.add("server_type", m.mime_type)
  j.add("server_url", m.url)
  j.add("genre", m.genre)
  j.add("stream_start", date(m.started))
  j.add("stream_start_iso8601", date(iso=true, m.started))
  title = icecast.server.stats.title(m.metadata)
  if null.defined(title) then j.add("title", null.get(title)) end
  list.iter(fun (field) -> j.add(fst(field), snd(field)), audio)
  if
    audio != []
  then
    j.add(
      "audio_info",
      string.concat(
        separator=";",
        list.map(fun (field) -> "ice-#{fst(field)}=#{snd(field)}", audio)
      )
    )
  end
  j
end

# The body of `/status-json.xsl`: `source` is absent, one object or an array,
# by the number of public mounts of the page view.
# @flag hidden
def icecast.server.pages.icestats_json(info, view, request) =
  date = icecast.server.pages.date
  stats = json.object()
  stats.add("admin", info.admin_contact)
  stats.add("host", info.hostname)
  stats.add("location", info.location)
  stats.add("server_id", info.server_id)
  stats.add("server_start", date(view.server_start))
  stats.add("server_start_iso8601", date(iso=true, view.server_start))
  sources =
    list.map(
      fun (m) ->
        (m.mount, icecast.server.pages.icestats_source(info, request, m)),
      view.public_mounts
    )
  sources =
    list.map(
      snd,
      list.sort(fun (a, b) -> if fst(a) < fst(b) then -1 else 1 end, sources)
    )
  count = list.length(sources)
  if
    count == 1
  then
    stats.add("source", list.hd(sources))
  elsif
    count > 1
  then
    stats.add("source", sources)
  end
  json.stringify({icestats = stats})
end
