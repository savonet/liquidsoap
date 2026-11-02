# Routes of the icecast.server pages. The server calls
# `icecast.server.pages.register` before the catch-all route of
# mount_requests.liq, since harbor matches routes in registration order. Each
# handler reads the parameters in force at its request and renders one page
# view.

# @flag hidden
def icecast.server.pages.not_found() =
  http.response(status_code=404)
end

# The reply of a public page: its switch, `status_access`, then `render()`.
# @flag hidden
def icecast.server.pages.public(
  state,
  ~enabled,
  ~content_type,
  request,
  render
) =
  if
    not enabled
  then
    icecast.server.pages.not_found()
  elsif
    not icecast.server.pages.is_public(state, request)
  then
    icecast.server.pages.unauthorized()
  else
    try
      http.response(content_type=content_type, data=render())
    catch err do
      icecast.server.log.important(
        state,
        "Rendering #{request.path} failed: #{err.message}"
      )
      http.response(status_code=500)
    end
  end
end

# The JSON reply `body()` when `request` carries `password`, any user name;
# open when `password` is `null`.
# @flag hidden
def icecast.server.pages.health_guarded(password, request, body) =
  credentials = icecast.server.pages.credentials(request)
  if
    null.defined(password)
    and (
      not null.defined(credentials)
      or snd(null.get(credentials)) != null.get(password)
    )
  then
    icecast.server.pages.unauthorized()
  else
    http.response(content_type="application/json", data=body())
  end
end

# `/health` and `/<mount>/health`: a hidden or unknown mount is a `404` before
# any password is checked.
# @flag hidden
def icecast.server.pages.health_reply(~enabled, ~password, view, request) =
  path = request.path
  if
    not enabled
  then
    icecast.server.pages.not_found()
  elsif
    path == "/health"
  then
    view = view()
    icecast.server.pages.health_guarded(
      password,
      request,
      fun () -> icecast.server.pages.health(view)
    )
  else
    name = string.sub(path, start=0, length=string.length(path) - 7)
    def entry(m) =
      health = icecast.server.pages.health_mount(m)
      (m.mount, {password = m.health_password, health = health})
    end

    found = list.assoc.nullable(name, list.map(entry, view().public_mounts))
    if
      null.defined(found)
    then
      found = null.get(found)
      icecast.server.pages.health_guarded(
        found.password,
        request,
        fun () -> found.health.stringify(compact=true)
      )
    else
      icecast.server.pages.not_found()
    end
  end
end

# Registers the pages on the server's port.
# @flag hidden
def icecast.server.pages.register(~transport, state) =
  def stats() =
    icecast.server.snapshot(state)
  end

  def view() =
    icecast.server.pages.view(state)
  end

  def info() =
    p = state.params.get()
    {
      hostname = p.hostname(),
      admin_contact = p.admin_contact(),
      location = p.location(),
      server_id = p.server_id(),
      port = state.port,
      scheme = state.transport.protocol == "https" ? "https" : "http"
    }
  end

  def get(path, handler) =
    harbor.http.register.simple(
      port=state.port,
      transport=transport,
      method="GET",
      path,
      handler
    )
  end

  def public(path, ~enabled, ~content_type, render) =
    get(
      path,
      fun (request) ->
        icecast.server.pages.public(
          state,
          enabled=enabled(),
          content_type=content_type,
          request,
          fun () -> render(request)
        )
    )
  end

  def status(path, ~content_type, render) =
    public(
      path,
      enabled=fun () -> state.params.get().status_page(),
      content_type=content_type,
      render
    )
  end

  def body(custom, builtin) =
    if
      null.defined(custom)
    then
      custom = null.get(custom)
      custom(stats())
    else
      builtin(view())
    end
  end

  def admin(path, ~content_type="text/xml", render) =
    get(
      path,
      fun (request) ->
        icecast.server.pages.is_admin(state.params.get(), request)
          ? http.response(content_type=content_type, data=render(request))
          : (icecast.server.pages.unauthorized())
    )
  end

  def health(request) =
    p = state.params.get()
    icecast.server.pages.health_reply(
      enabled=p.status_page() or state.compatibility == "rsas",
      password=getter.get(p.health_password),
      view,
      request
    )
  end

  status(
    "/",
    content_type="text/html; charset=utf-8",
    fun (_) ->
      body(state.params.get()?.status_html, icecast.server.pages.status_html)
  )
  status(
    "/status\\.json",
    content_type="application/json",
    fun (_) ->
      body(state.params.get()?.status_json, icecast.server.pages.status_json)
  )
  status(
    "/status-json\\.xsl",
    content_type="application/json",
    fun (request) -> icecast.server.pages.icestats_json(info(), view(), request)
  )
  public(
    "/metrics",
    enabled=fun () -> state.params.get().metrics(),
    content_type="text/plain; version=0.0.4; charset=utf-8",
    fun (_) -> icecast.server.pages.metrics(view())
  )
  admin(
    "/admin/listeners",
    content_type="text/html; charset=utf-8",
    fun (_) -> icecast.server.pages.listeners_html(view())
  )
  admin(
    "/admin/listeners\\.json",
    content_type="application/json",
    fun (_) -> icecast.server.pages.listeners_json(view())
  )
  admin(
    "/admin/stats",
    fun (request) -> icecast.server.pages.admin_stats(info(), view(), request)
  )
  admin("/admin/listmounts", fun (_) -> icecast.server.pages.listmounts(view()))
  get(
    "/admin/listclients",
    fun (request) -> icecast.server.pages.listclients(state, request)
  )
  get("/health", health)
  harbor.http.register.simple.regexp(
    port=state.port,
    transport=transport,
    method="GET",
    r/^\/.+\/health$/,
    health
  )
end
