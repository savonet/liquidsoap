# URL services of source clients (sources.md §3.2, §7), internal helpers of
# the configuration translation: `icecast.server.source_decision.stream_auth`
# is a mount's `source_decision`, `icecast.server.source_callbacks.url` gives
# its `on_connect` and `on_disconnect`. Both POST form fields with the
# credentials and timeout of url-auth.md §3 and §7.
let icecast.server.source_decision = ()
let icecast.server.source_callbacks = ()

# URL authentication of a source client: the `stream_auth` reply admits it as
# a `listener_add` reply admits a listener (url-auth.md §4); a failed request
# refuses it.
# @flag hidden
def icecast.server.source_decision.stream_auth(options) =
  fun (request) ->
    begin
      config = icecast.server.url_auth.config(request.compatibility, options)
      fields =
        [
          ("action", "stream_auth"),
          ("server", request.server.hostname),
          ("port", string(request.server.port)),
          ("server-instance", request.server.instance),
          ("mount", request.mount),
          ("ip", request.address),
          ("agent", request.agent),
          ("user", request.user),
          ("pass", request.password),
          ...(request.admin ? [("admin", "1")] : [])
        ]
      try
        reply =
          icecast.server.url_auth.send(
            config=config,
            fields=fields,
            request,
            options?.stream_auth ?? ""
          )
        icecast.server.url_auth.admits(config=config, reply)
      catch err do
        log.severe(
          label="icecast.server",
          "stream_auth failed for #{request.mount}: #{err.message}"
        )
        false
      end
    end
end

# The `mount_add` and `mount_remove` notifications as a mount's `on_connect`
# and `on_disconnect`; a failure is logged and the reply ignored.
# @flag hidden
def icecast.server.source_callbacks.url(options) =
  def notify(action, url, record) =
    config = icecast.server.url_auth.config(record.compatibility, options)
    if
      url != ""
    then
      try
        ignore(
          icecast.server.url_auth.send(
            config=config,
            fields=[
              ("action", action),
              ("mount", record.mount),
              ("server", record.server.hostname),
              ("port", string(record.server.port)),
              ("server-instance", record.server.instance),
              ("ip", record.ip),
              ("agent", record.agent)
            ],
            {user = "", password = ""},
            url
          )
        )
      catch err do
        log.severe(
          label="icecast.server",
          "#{action} failed for #{record.mount}: #{err.message}"
        )
      end
    end
  end

  {
    on_connect =
      fun (record) -> notify("mount_add", options?.mount_add ?? "", record),
    on_disconnect =
      fun (record) ->
        notify("mount_remove", options?.mount_remove ?? "", record)
  }
end
