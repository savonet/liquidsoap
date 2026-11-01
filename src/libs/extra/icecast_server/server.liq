# Starts an Icecast-compatible server on one port: source clients feed mounts
# and listeners read them, under the policy of `parameters` (see the
# icecast.server specification). Raises
# `error.icecast.server.invalid_parameters` when `parameters` are invalid.
# @category Internet
# @flag extra
# @param parameters The server's parameters: every field is optional.
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
  icecast.server.mount_requests.register(transport=p.transport, state)
  input.harbor.dynamic.regexp(
    icy=true,
    port=p.port,
    transport=p.transport,
    timeout=p.source_timeout(),
    auth=fun (_) -> true,
    accept=fun (args) -> icecast.server.sources.accept(state, args),
    on_connect=fun (connection) ->
      icecast.server.sources.connect(state, connection),
    r/^\/.*$/
  )
  log.important(
    label=p.name,
    "Icecast server starting on port #{p.port}."
  )
  icecast.server.warn_default_password(state, p)
  if
    not null.defined(p.admin_password())
  then
    log.important(
      label=p.name,
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
    reopen_logs = fun () -> (),
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
