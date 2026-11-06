# Starts an Icecast-compatible server on one port: source clients feed mounts
# and listeners read them, under the policy of `parameters` (see the
# icecast.server specification). Routes registered on the same port before
# this call take precedence; later GET routes are shadowed. Raises
# `error.icecast.server.invalid_parameters` when `parameters` are invalid.
# @category Internet
# @flag extra
# @param parameters The server's parameters: every field is optional.
# @field parameters.access_log Access log path, `"-"` for standard error. \
#   Unset: no access log.
# @field parameters.address_hash Applied to every listener address before \
#   observers (callbacks, status pages, logs) see it. Defaults to an unkeyed \
#   MD5, 16 hexadecimal characters.
# @field parameters.admin_contact Administrator contact shown in the status \
#   JSON and XML.
# @field parameters.admin_password Password of the admin pages. Unset: they \
#   answer as if absent.
# @field parameters.admin_user User name of the admin pages, `"admin"` by \
#   default.
# @field parameters.bind_address Local address to bind. Unset: all interfaces. \
#   Read at startup.
# @field parameters.burst_on_connect Whether new listeners get a burst, `true` \
#   by default.
# @field parameters.burst_size Bytes sent at once to a new listener of a mount \
#   with no `burst_size` of its own, `65536` by default. `0` disables the burst.
# @field parameters.client_timeout Seconds a listener's connection may stall \
#   before it is dropped, `30.` by default.
# @field parameters.compatibility Implementation to mimic where they differ: \
#   `"icecast-2.5"` (default), `"icecast-2.4"`, `"rsas"` or `"kh"`. Read at \
#   startup.
# @field parameters.dedicated_encoder Give each listener its own encoder, \
#   `false` by default.
# @field parameters.error_log Path of the Icecast-format error log. Unset: no \
#   error log.
# @field parameters.event_bindings Callbacks bound to server events: \
#   `"source_connect"`, `"source_disconnect"`, `"listener_connect"`, \
#   `"listener_disconnect"` or `"metadata"`, for all mounts or one.
# @field parameters.forwarded_address Computes the client's address from the \
#   request, in place of the `X-Forwarded-For` walk of `proxy_ips`. Answering \
#   `null` keeps the peer address.
# @field parameters.health_password Password (HTTP Basic) of `/health` and of \
#   the health page of mounts that have none of their own. Unset: no password.
# @field parameters.hostname Name the server gives itself in status pages and \
#   URL authentication, `"localhost"` by default.
# @field parameters.http_headers Headers added to every listener reply: \
#   `{name, value}`, or `{name, compute}` for headers that depend on the \
#   request, optionally for one `status` only.
# @field parameters.location Location shown in the status JSON and XML.
# @field parameters.log_archive Rotated logs keep a timestamped name instead \
#   of replacing the previous copy.
# @field parameters.log_level Most verbose level written to the error log: 1 \
#   critical to 5 debug, `4` by default.
# @field parameters.log_size Size in bytes past which a log file rotates.
# @field parameters.max_clients Most simultaneous listeners over all mounts. \
#   Unset: unlimited.
# @field parameters.max_history Titles kept in the metadata history of a mount \
#   with no `max_history` of its own, 1 to 256, `10` by default.
# @field parameters.max_sources Most simultaneous source clients. Unset: \
#   unlimited.
# @field parameters.metrics Serve the Prometheus export at `/metrics`, `false` \
#   by default.
# @field parameters.mount Function of a mount path returning that mount's own \
#   parameters, or `null` to use the server's. See the icecast.server \
#   documentation for the mount fields.
# @field parameters.muxer_options Muxer options for a container format name. \
#   Unset: `settings.icecast.server.default_muxer_options`.
# @field parameters.name Label of the server in logs. Read at startup.
# @field parameters.playlist_log Playlist log path, `"-"` for standard error. \
#   Unset: no playlist log.
# @field parameters.port TCP port of source clients and listeners, `8000` by \
#   default. Shoutcast source clients use `port + 1`. Read at startup.
# @field parameters.proxy_ips Trusted reverse proxies, as addresses or CIDR \
#   ranges: the listener's address is taken from `X-Forwarded-For`, skipping \
#   them from the right. Empty list: trust the rightmost entry. Unset: use the \
#   peer address.
# @field parameters.server_id Server identification shown in the status JSON \
#   and XML.
# @field parameters.shoutcast_mount Mount that Shoutcast source clients feed, \
#   `"/stream"` by default.
# @field parameters.source_password Password of source clients on mounts with \
#   none of their own, `"hackme"` by default. Empty: no credentials required.
# @field parameters.source_timeout Seconds of inactivity after which a source \
#   client is dropped, `10.` by default.
# @field parameters.status_access Decides whether a request may see the status \
#   endpoints: `false` answers `401`. Unset: everyone may.
# @field parameters.status_html Replaces the rendering of the HTML status \
#   page.
# @field parameters.status_json Replaces the rendering of the JSON status \
#   endpoint.
# @field parameters.status_page Serve the public status endpoints, `true` by \
#   default. The admin pages are not affected.
# @field parameters.transport HTTP transport of the port, such as \
#   `http.transport.tls(...)` for TLS. Plain HTTP by default. Read at startup.
def replaces icecast.server(parameters) =
  settings.frame.duration.set_default(0.2)
  p = icecast.server.parameters.server(parameters)
  if
    null.defined(p?.bind_address)
  then
    settings.harbor.bind_addrs := [null.get(p?.bind_address)]
  end
  state = icecast.server.state(p)
  icecast.server.source_admin.register(transport=p.transport, state)
  icecast.server.pages.register(transport=p.transport, state)
  icecast.server.mount_requests.register(transport=p.transport, state)
  input.harbor.dynamic.regexp(
    icy=true,
    port=p.port,
    transport=p.transport,
    timeout=p.source_timeout,
    auth=fun (_) -> true,
    accept=fun (args) -> icecast.server.sources.accept(state, args),
    on_connect=fun (connection) ->
      icecast.server.sources.connect(state, connection),
    r/^\/.*$/
  )
  icecast.server.log.important(
    state,
    "Icecast server starting on port #{p.port}."
  )
  icecast.server.warn_default_password(state, p)
  if
    not null.defined(p.admin_password())
  then
    icecast.server.log.important(
      state,
      "No admin_password: the admin pages refuse everyone."
    )
  end
  transport_reload = parameters?.transport?.reload
  def reload(parameters) =
    icecast.server.reload(state, transport_reload=transport_reload, parameters)
  end

  events = state.events
  {
    on_source_connect = events.source_connect.add,
    on_source_disconnect = events.source_disconnect.add,
    on_listener_connect = events.listener_connect.add,
    on_listener_disconnect = events.listener_disconnect.add,
    on_metadata = events.metadata.add,
    stats = fun () -> icecast.server.snapshot(state),
    reload = reload,
    reopen_logs = state.logs.reopen,
    attach_source =
      fun (~headers=[], ~format, path, s) ->
        icecast.server.attached.attach(
          state,
          headers=headers,
          format=format,
          path,
          s
        ),
    detach_source = fun (path) -> icecast.server.attached.detach(state, path)
  }
end
