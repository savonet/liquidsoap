# Pages of icecast.server (observability.md §6 to §8): the status pages
# (status_pages.liq), the admin pages (admin_pages.liq, admin_xml.liq), and
# `/health` and `/metrics` (health.liq) render a view of the `stats()`
# snapshot built here; pages.liq registers them on the server's port.
let icecast.server.pages = ()

# @flag hidden
def icecast.server.pages.html(value) =
  string.escape.html(value)
end

# A time as Icecast's status pages write it, `iso` for the ISO 8601 form.
# @flag hidden
def icecast.server.pages.date(~iso=false, (t:float)) =
  time.string(
    time=t,
    iso
      ? "%Y-%m-%dT%H:%M:%S%z"
      : "%a, %d %b %Y %H:%M:%S %z"
  )
end

# A `401` with Icecast's challenge and an empty body.
# @flag hidden
def icecast.server.pages.unauthorized() =
  http.response(
    status_code=401,
    headers=[("WWW-Authenticate", icecast.server.realm)]
  )
end

# The `(user, password)` of the request's Basic credentials, `null` without.
# @flag hidden
def icecast.server.pages.credentials(request) =
  icecast.server.decision.credentials(
    icecast.server.headers.lowercase(request.headers)
  )
end

# Whether `request` carries the admin credentials of the parameters `p`.
# @flag hidden
def icecast.server.pages.is_admin(p, request) =
  credentials = icecast.server.pages.credentials(request)
  null.defined(credentials)
  and icecast.server.source_admin.is_admin(
    p,
    fst(null.get(credentials)),
    snd(null.get(credentials))
  )
end

# Whether `request` may see the public pages under `status_access`; a failing
# callback refuses.
# @flag hidden
def icecast.server.pages.is_public(state, request) =
  status_access = state.params.get()?.status_access
  if
    null.defined(status_access)
  then
    status_access = null.get(status_access)
    try
      status_access(request)
    catch err do
      icecast.server.log.important(
        state,
        "status_access failed, refusing: #{err.message}"
      )
      false
    end
  else
    true
  end
end

# The URL of `mount` as a listener of `request` reaches it, from the page
# `info` of `register` (pages.liq).
# @flag hidden
def icecast.server.pages.listen_url(info, request, mount) =
  host =
    list.assoc(
      default="#{info.hostname}:#{info.port}",
      "host",
      icecast.server.headers.lowercase(request.headers)
    )
  "#{info.scheme}://#{host}#{mount}"
end

# The page view: the server's snapshot with, per mount, `audio` (the known
# audio parameters as `(name, value)` pairs, in the order of Icecast's
# `audio_info`), `has_source`, the feeder's `codecs`, the `source_ip` and
# `source_agent` of a source client (`""` without one), `max_listeners` as
# written on the admin pages, `health_password`, `parameters`, the mount's
# parameters (`null` when they fail), `burst`, its burst size once the
# server's `burst_on_connect` applies, and `dedicated_encoder`; and `public_mounts`, the
# mounts public pages list. Renderers read these fields: a renderer that reads
# a nullable field of a mount it also passes on is rejected by the typechecker.
# @flag hidden
def icecast.server.pages.view(state) =
  snapshot = icecast.server.snapshot(state)
  p = state.params.get()
  def extend(m) =
    entry = icecast.server.mounts.find(state, m.mount)
    feeder = null.defined(entry) ? null.get(entry).feeder.get() : null
    client = null.defined(feeder) and null.get(feeder).is_client
    mount =
      try
        icecast.server.parameters.mount(p, m.mount)
      catch _ do
        null
      end
    max_listeners = null.defined(mount) ? null.get(mount).max_listeners : null
    m.{
      audio =
        list.filter_map(
          fun (field) -> null.map(fun (v) -> (fst(field), v), snd(field)),
          [
            ("samplerate", m.samplerate),
            ("bitrate", m.bitrate),
            ("channels", m.channels)
          ]
        ),
      has_source = null.defined(feeder),
      codecs = null.defined(feeder) ? null.get(feeder).format.codecs : [],
      source_ip = client ? null.get(feeder).ip : "",
      source_agent = client ? null.get(feeder).agent : "",
      max_listeners =
        null.defined(max_listeners)
          ? "#{null.get(max_listeners)}"
          : "unlimited",
      health_password =
        null.defined(mount)
          ? null.get(mount).health_password
          : (getter.get(p.health_password)),
      parameters = mount,
      dedicated_encoder =
        null.defined(mount)
          ? null.get(mount).dedicated_encoder
          : (getter.get(p.dedicated_encoder)),
      burst =
        p.burst_on_connect and null.defined(mount)
          ? null.get(mount).burst_size
          : (0)
    }
  end

  mounts = list.map(extend, snapshot.mounts)
  snapshot.{
    mounts = mounts,
    public_mounts = list.filter(fun (m) -> not m.hidden, mounts)
  }
end

# The `<head>` elements of the HTML pages: encoding, mobile viewport and their
# built-in stylesheet.
# @flag hidden
let icecast.server.pages.head =
  "<meta charset=\"utf-8\"><meta name=\"viewport\" \
   content=\"width=device-width,initial-scale=1\"><style>*{box-sizing:border-box;margin:0;padding:0}body{font-family:system-ui,-apple-system,sans-serif;background:#f0f2f5;color:#222;line-height:1.5}header{background:#16213e;color:#fff;padding:1.25rem \
   2rem;display:flex;align-items:center;justify-content:space-between}header \
   h1{font-size:1.3rem;font-weight:500}#status{font-size:.8rem;opacity:.7}main{max-width:860px;margin:2rem \
   auto;padding:0 \
   1rem}.empty{text-align:center;color:#888;padding:4rem;background:#fff;border-radius:8px}.card{background:#fff;border-radius:8px;padding:1.5rem;margin-bottom:1rem;box-shadow:0 \
   1px 3px \
   rgba(0,0,0,.08);overflow-x:auto}.card-title{font-size:1.05rem;font-weight:600;color:#16213e;margin-bottom:.75rem;font-family:monospace}.card-title \
   a{color:inherit;text-decoration:none}.now-playing{background:#eef2ff;color:#3730a3;border-radius:4px;padding:.4rem \
   .75rem;margin-bottom:.75rem;font-size:.9rem}.now-playing::before{content:'▶ \
   ';opacity:.7}.meta{display:flex;flex-wrap:wrap;gap:.75rem \
   2rem;margin-bottom:1rem;font-size:.875rem}.meta-item{display:flex;flex-direction:column}.meta-label{font-size:.7rem;text-transform:uppercase;letter-spacing:.5px;color:#888;margin-bottom:.15rem}hr{border:none;border-top:1px \
   solid #f0f0f0;margin:.75rem \
   0}.stats-row{display:flex;flex-wrap:wrap;align-items:center;gap:2rem}.stat{text-align:center}.stat-n{font-size:1.6rem;font-weight:600;color:#16213e;line-height:1}.stat-l{font-size:.7rem;text-transform:uppercase;color:#888;margin-top:.2rem}audio,video{width:100%;margin-top:1rem;accent-color:#16213e}table{width:100%;border-collapse:collapse;font-size:.85rem}th,td{text-align:left;padding:.3rem \
   .5rem;border-bottom:1px solid \
   #f0f0f0}th{font-size:.7rem;text-transform:uppercase;color:#888}form{display:flex;flex-wrap:wrap;gap:.5rem;align-items:center;margin-top:1rem}input{flex:1;min-width:10rem;padding:.4rem \
   .6rem;border:1px solid \
   #d0d4dc;border-radius:4px;font:inherit}button{padding:.4rem \
   .9rem;border:none;border-radius:4px;background:#16213e;color:#fff;font:inherit;cursor:pointer}output{font-size:.85rem;color:#555}.danger{background:#b42318}.muted{color:#888;font-size:.875rem}</style>"

# An HTML page titled `title`: a header bar with a `#status` line, then `body`
# in its `main` element.
# @flag hidden
def icecast.server.pages.document(~title, body) =
  "<!DOCTYPE html>\n<html lang=\"en\"><head>#{
    icecast.server.pages.head
  }<title>#{title}</title></head><body><header><h1>#{title}</h1><span \
   id=\"status\"></span></header><main>#{body}</main></body></html>\n"
end

# A labelled value of a mount card's meta row; `value` is plain text.
# @flag hidden
def icecast.server.pages.meta_item(label, value) =
  "<div class=\"meta-item\"><span class=\"meta-label\">#{label}</span><span>#{
    icecast.server.pages.html(value)
  }</span></div>"
end

# A large figure of a mount card's stats row; `attribute` marks the number for
# the page script.
# @flag hidden
def icecast.server.pages.stat(~attribute="", label, value) =
  "<div class=\"stat\"><div class=\"stat-n\"#{
    attribute == ""
      ? ""
      : " #{attribute}"
  }>#{icecast.server.pages.html(value)}</div><div class=\"stat-l\">#{
    label
  }</div></div>"
end

# The now playing line of a mount card, hidden while `title` is empty.
# @flag hidden
def icecast.server.pages.now_playing(title) =
  "<div class=\"now-playing\" data-title#{
    title == ""
      ? " hidden"
      : ""
  }>#{icecast.server.pages.html(title)}</div>"
end
