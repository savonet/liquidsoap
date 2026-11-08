# The public status pages of icecast.server (observability.md §6): `/`,
# `/status.json` and `/status-json.xsl`, rendered from the public mounts of
# the page view (page_helpers.liq).

# Keeps the counts and the now playing line of `/` current from
# `/status.json` without reloading the page, which would stop its players; a
# change of the mounts reloads it.
# @flag hidden
let icecast.server.pages.status_script =
  "<script>const \
   shown=[...document.querySelectorAll('section[data-mount]')].map(s=>s.dataset.mount).join('\\n');setInterval(async()=>{const \
   status=document.getElementById('status');try{const r=await \
   fetch('/status.json');if(!r.ok)throw r.status;const{mounts}=await \
   r.json();if(mounts.map(m=>m.mount).join('\\n')!==shown){location.reload();return}for(const \
   m of mounts){const \
   s=[...document.querySelectorAll('section[data-mount]')].find(e=>e.dataset.mount===m.mount);if(!s)continue;const \
   t=s.querySelector('[data-title]');t.textContent=m.title;t.hidden=!m.title;s.querySelector('[data-listeners]').textContent=m.listener_count;s.querySelector('[data-peak]').textContent=m.peak_listeners}status.textContent='Updated \
   '+new Date().toLocaleTimeString()}catch(e){status.textContent='Connection \
   error'}},5000)</script>"

# The built-in body of `/`, listing the public mounts of the page view.
# @flag hidden
def icecast.server.pages.status_html(view) =
  html = icecast.server.pages.html
  meta = icecast.server.pages.meta_item
  stat = icecast.server.pages.stat
  def player(m) =
    element = string.contains(prefix="video/", m.mime_type) ? "video" : "audio"
    "<#{element} controls preload=\"none\"><source src=\"#{html(m.mount)}\" \
     type=\"#{html(m.mime_type)}\"></#{element}>"
  end

  def mount(m) =
    name = m.name == "" ? m.mount : (m.name)
    "<section class=\"card\" data-mount=\"#{html(m.mount)}\"><div \
     class=\"card-title\"><a href=\"#{html(m.mount)}\">#{
      html(m.mount)
    }</a></div>#{
      icecast.server.pages.now_playing(
        icecast.server.stats.title(m.metadata) ?? ""
      )
    }<div class=\"meta\">#{meta("Name", name)}#{meta("Format", m.mime_type)}#{
      meta("Started", icecast.server.pages.date(m.started))
    }</div><hr><div class=\"stats-row\">#{
      stat(
        attribute="data-listeners",
        "Listeners",
        "#{list.length(m.listeners)}"
      )
    }#{stat(attribute="data-peak", "Peak", "#{m.peak_listeners}")}</div>#{
      player(m)
    }</section>"
  end

  mounts = view.public_mounts
  body =
    list.is_empty(mounts)
      ? "<p class=\"empty\">No active mounts.</p>"
      : (string.concat(list.map(mount, mounts)))
  icecast.server.pages.document(
    title="Liquidsoap Status",
    "#{body}#{icecast.server.pages.status_script}"
  )
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
      title = icecast.server.stats.title(m.metadata) ?? "",
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
