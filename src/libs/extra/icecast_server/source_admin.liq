# Mount commands of icecast.server (sources.md §6.1, §6.2, mounts.md §6.3):
# `metadata` (also `/admin.cgi`), `killsource`, `manage` and `fallbacks`. The
# server registers them on its port with
# `icecast.server.source_admin.register` (source_commands.liq, with `manage`
# and `fallbacks`); each is authenticated as the mount's source client would
# be, or with the admin credentials.
let icecast.server.source_admin = ()

# @flag hidden
def icecast.server.source_admin.response(status, message) =
  http.response(
    status_code=status,
    headers=status == 401
      ? [
        (
          "WWW-Authenticate",
          "Basic realm=\"Icecast2 Server\""
        )
      ]
      : [],
    content_type="text/xml",
    data="<?xml version=\"1.0\"?>\n<iceresponse><message>#{
      message
    }</message><return>#{status == 200 ? 1 : 0}</return></iceresponse>\n"
  )
end

# Whether `user` and `password` are the admin credentials of the parameters
# `p`.
# @flag hidden
def icecast.server.source_admin.is_admin(p, user, password) =
  admin_password = p.admin_password()
  null.defined(admin_password)
  and user == p.admin_user()
  and password == null.get(admin_password)
end

# Whether `request` may run a command on `path` under the parameters `p`
# (sources.md §6): Basic credentials, or `user` and `pass` query arguments on
# `/admin.cgi`, an absent `user` meaning the mount's `source_user`.
# @flag hidden
def icecast.server.source_admin.allowed(state, p, path, request) =
  headers = icecast.server.headers.lowercase(request.headers)
  try
    mount = icecast.server.parameters.mount(p, path)
    query_credentials =
      request.path == "/admin.cgi"
        ? (
          list.assoc(default=mount.source_user, "user", request.query),
          list.assoc(default="", "pass", request.query)
        )
        : ("", "")
    let (user, password) =
      icecast.server.decision.credentials(headers) ?? query_credentials
    icecast.server.source_admin.is_admin(p, user, password)
    or icecast.server.sources.authenticated(
      mount,
      icecast.server.sources.request(
        state,
        p,
        ip="",
        method=request.method,
        mount=path,
        query=request.query_string,
        user=user,
        password=password,
        admin=true,
        protocol=request.http_version,
        uri=request.path,
        headers
      )
    )
  catch err do
    icecast.server.log.important(
      state,
      "Refusing a mount command on #{
        icecast.server.log_lines.client_value(
          compatibility=state.compatibility,
          path
        )
      }: #{err.message}"
    )
    false
  end
end

# Runs `command(entry)` on the mount named by `path` after the checks of
# sources.md §6, in order: a mount named, a live mount, credentials.
# @flag hidden
def icecast.server.source_admin.run(state, ~p, ~path, request, command) =
  response = icecast.server.source_admin.response
  unknown = state.compatibility == "icecast-2.5" ? 404 : 400
  if
    path == ""
  then
    response(
      400,
      "No mount specified"
    )
  else
    entry = icecast.server.mounts.find(state, path)
    if
      not null.defined(entry)
    then
      response(
        unknown,
        "Source does not exist"
      )
    elsif
      not icecast.server.source_admin.allowed(state, p, path, request)
    then
      response(
        401,
        "Authentication required"
      )
    else
      command(null.get(entry))
    end
  end
end

# @flag hidden
def icecast.server.source_admin.metadata(state, ~shoutcast=false, request) =
  query = request.query
  p = state.params.get()
  default_mount = shoutcast ? p.shoutcast_mount() : ""
  path = list.assoc(default=default_mount, "mount", query)
  def update(entry) =
    response = icecast.server.source_admin.response
    format = entry.format
    takes_updates =
      null.defined(format.container)
        ? list.mem(
          null.get(format.container),
          ["mp3", "adts", "aac", "wav", "flac"]
        )
        : (
          list.mem(
            format.mime,
            ["audio/mpeg", "audio/aac", "audio/aacp", "audio/wav", "audio/flac"]
          )
        )
    if
      list.assoc(default="", "mode", query) != "updinfo"
    then
      response(
        400,
        "No such action"
      )
    elsif
      not takes_updates
    then
      response(
        405,
        "Mountpoint will not accept URL updates"
      )
    else
      charset = list.assoc.nullable("charset", query)
      def text(value) =
        string.recode(in_enc=charset, out_enc="UTF-8", value)
      end

      title = list.assoc.nullable("title", query)
      artist = list.assoc.nullable("artist", query)
      song = list.assoc.nullable("song", query)
      fields =
        null.defined(title) or null.defined(artist)
          ? [("title", title), ("artist", artist)]
          : [("title", song)]
      fields = [...fields, ("url", list.assoc.nullable("url", query))]
      metadata =
        list.filter_map(
          fun (field) ->
            null.map(fun (value) -> (fst(field), text(value)), snd(field)),
          fields
        )
      entry.insert_metadata(metadata)
      response(
        200,
        "Metadata update successful"
      )
    end
  end

  icecast.server.source_admin.run(state, p=p, path=path, request, update)
end

# Disconnects the mount's source client (sources.md §6.2).
# @flag hidden
def icecast.server.source_admin.kill(state, ~path, request) =
  def kill(entry) =
    feeder = entry.feeder.get()
    if
      null.defined(feeder) and null.get(feeder).is_client
    then
      stop = null.get(feeder).stop
      stop()
      icecast.server.source_admin.response(
        200,
        "Source Removed"
      )
    else
      icecast.server.source_admin.response(
        400,
        "No source client to remove"
      )
    end
  end

  icecast.server.source_admin.run(
    state,
    p=state.params.get(),
    path=path,
    request,
    kill
  )
end
