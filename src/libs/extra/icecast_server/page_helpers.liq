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
# written on the admin pages and `health_password`; and `public_mounts`, the
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
          : (getter.get(p.health_password))
    }
  end

  mounts = list.map(extend, snapshot.mounts)
  snapshot.{
    mounts = mounts,
    public_mounts = list.filter(fun (m) -> not m.hidden, mounts)
  }
end
