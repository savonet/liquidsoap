# RSAS's `POST /<mount>/manage`: `action=kick`, in the query or the form
# body, disconnects the mount's source client (sources.md §6.2).
# @flag hidden
def icecast.server.source_admin.manage(state, request) =
  path =
    string.sub(request.path, start=0, length=string.length(request.path) - 7)
  form = url.split_args(request.data(timeout=5.))
  action =
    list.assoc(
      default=list.assoc(default="", "action", form),
      "action",
      request.query
    )
  if
    action == "kick"
  then
    icecast.server.source_admin.kill(state, path=path, request)
  else
    icecast.server.source_admin.response(
      400,
      "No such action"
    )
  end
end

# Sets the mount's fallback in the run-time overlay (mounts.md §6.3).
# @flag hidden
def icecast.server.source_admin.fallbacks(state, request) =
  query = request.query
  path = list.assoc(default="", "mount", query)
  fallback = list.assoc.nullable("fallback", query)
  p = state.params.get()
  def set(_) =
    if
      not null.defined(fallback)
    then
      icecast.server.source_admin.response(
        400,
        "No fallback specified"
      )
    else
      icecast.server.locked(
        state,
        fun () ->
          begin
            state.overlay.set(
              [
                ...list.assoc.remove(path, state.overlay.get()),
                (path, null.get(fallback))
              ]
            )
            icecast.server.mounts.refresh(state)
          end
      )
      icecast.server.source_admin.response(
        200,
        "Fallback configured"
      )
    end
  end

  if
    state.compatibility == "kh"
    and path != ""
    and not null.defined(icecast.server.mounts.find(state, path))
  then
    if
      icecast.server.source_admin.allowed(state, p, path, request)
    then
      set(())
    else
      icecast.server.source_admin.response(
        401,
        "Authentication required"
      )
    end
  else
    icecast.server.source_admin.run(state, p=p, path=path, request, set)
  end
end

# Registers the mount commands on the server's port.
# @flag hidden
def icecast.server.source_admin.register(~transport, state) =
  def get(path, handler) =
    harbor.http.register.simple(
      port=state.port,
      transport=transport,
      method="GET",
      path,
      handler
    )
  end

  get(
    "/admin/metadata",
    fun (request) -> icecast.server.source_admin.metadata(state, request)
  )
  get(
    "/admin.cgi",
    fun (request) ->
      icecast.server.source_admin.metadata(state, shoutcast=true, request)
  )
  get(
    "/admin/killsource",
    fun (request) ->
      icecast.server.source_admin.kill(
        state,
        path=list.assoc(default="", "mount", request.query),
        request
      )
  )
  get(
    "/admin/fallbacks",
    fun (request) -> icecast.server.source_admin.fallbacks(state, request)
  )
  get(
    "/admin/fallback",
    fun (request) -> icecast.server.source_admin.fallbacks(state, request)
  )
  harbor.http.register.simple.regexp(
    port=state.port,
    transport=transport,
    method="POST",
    r/^\/.+\/manage$/,
    fun (request) -> icecast.server.source_admin.manage(state, request)
  )
end
