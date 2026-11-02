# Whether the credentials of `request` open the mount whose parameters are
# `mount`: its `source_decision` when it has one, else its source user and
# password, an empty password requiring none.
# @flag hidden
def icecast.server.sources.authenticated(mount, request) =
  decision = mount?.source_decision
  if
    null.defined(decision)
  then
    decision = null.get(decision)
    try
      decision(request)
    catch err do
      log.important(
        label="icecast.server",
        "Source decision for #{request.mount} failed: #{err.message}"
      )
      false
    end
  else
    mount.source_password == ""
    or (
      request.user == mount.source_user
      and request.password == mount.source_password
    )
  end
end

# The query string of harbor's query arguments.
# @flag hidden
def icecast.server.sources.query_string(query) =
  string.concat(
    separator="&",
    list.map(fun (arg) -> "#{fst(arg)}=#{snd(arg)}", query)
  )
end

# The refusal of a source request (sources.md §4) under `compatibility`.
# @flag hidden
def icecast.server.sources.refusal(compatibility, why) =
  icecast25 = compatibility == "icecast-2.5"
  if
    why == "credentials"
  then
    icecast.server.refusal(401)
  elsif
    why == "taken" and icecast25
  then
    icecast.server.refusal(409, reason="Conflict")
  elsif
    why == "taken"
  then
    icecast.server.refusal(
      403,
      reason="Mountpoint in use"
    )
  elsif
    why == "sources" and icecast25
  then
    icecast.server.refusal(503)
  elsif
    why == "sources"
  then
    icecast.server.refusal(
      403,
      reason="Too many sources"
    )
  elsif
    why == "path"
  then
    icecast.server.refusal(400)
  else
    icecast.server.refusal(403)
  end
end

# The source decision's request (sources.md §3.1) under the parameters `p`,
# `headers` having lowercase names; `ip` is the peer address.
# @flag hidden
def icecast.server.sources.request(
  state,
  p,
  ~ip,
  ~method,
  ~mount,
  ~query,
  ~user,
  ~password,
  ~admin,
  ~protocol,
  ~uri,
  headers
) =
  {
    address =
      icecast.server.addresses.resolve(
        proxy_ips=p.proxy_ips(),
        forwarded_address=p?.forwarded_address,
        {ip = ip, headers = headers, protocol = protocol, uri = uri}
      ),
    method = method,
    mount = mount,
    query = query,
    user = user,
    password = password,
    admin = admin,
    agent = list.assoc(default="-", "user-agent", headers),
    headers = headers,
    server = state.server(p.hostname()),
    compatibility = state.compatibility
  }
end

# Admission of a source request by harbor's `accept` (sources.md §3), in
# order: path, credentials, mount taken, `max_sources`.
# @flag hidden
def icecast.server.sources.accept(state, args) =
  p = state.params.get()
  is_source =
    list.mem(args.method, ["PUT", "POST", "SOURCE", "ICY", "WEBSOCKET"])
  path = args.method == "ICY" ? p.shoutcast_mount() : (args.uri)
  quoted =
    icecast.server.log_lines.client_value(
      compatibility=state.compatibility,
      path
    )
  def refuse(why) =
    icecast.server.log.important(
      state,
      "Refusing source client on #{quoted}: #{why}."
    )
    {refuse = icecast.server.sources.refusal(state.compatibility, why)}
  end

  if
    not is_source
  then
    null
  elsif
    not string.contains(prefix="/", path)
  then
    refuse("path")
  elsif
    icecast.server.sources.is_reserved(path)
  then
    refuse("reserved")
  else
    mount =
      try
        icecast.server.parameters.mount(p, path)
      catch err do
        icecast.server.log.important(
          state,
          "Invalid parameters for mount #{quoted}: #{err.message}"
        )
        null
      end
    request =
      icecast.server.sources.request(
        state,
        p,
        ip=args.address,
        method=args.method,
        mount=path,
        query=icecast.server.sources.query_string(args.query),
        user=args.user,
        password=args.password,
        admin=false,
        protocol="1.0",
        uri=args.uri,
        args.headers
      )
    if
      not null.defined(mount)
      or not icecast.server.sources.authenticated(null.get(mount), request)
    then
      refuse("credentials")
    elsif
      icecast.server.mounts.has_feeder(state, path)
    then
      refuse("taken")
    elsif
      null.defined(p.max_sources())
      and icecast.server.mounts.source_clients(state) >=
        null.get(p.max_sources())
    then
      refuse("sources")
    else
      null
    end
  end
end
