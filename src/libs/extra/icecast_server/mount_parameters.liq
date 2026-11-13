# The complete, validated parameters of the mount at `path` (parameters.md §4),
# getters evaluated: the server calls it at each use that needs a mount's
# values, with its complete server record, and the mount's own value beats the
# server's.
# @flag hidden
def icecast.server.parameters.mount(server, path) =
  m = (server.mount(path) : _?)
  def check(name) =
    icecast.server.parameters.check(
      "mount #{path}: #{name}"
    )
  end

  def value(check, value) =
    icecast.server.parameters.checked(check, getter.get(value))
  end

  def resolve_fallback(fallback) =
    source = fallback?.source
    mount = fallback?.mount
    if
      null.defined(source) and null.defined(mount)
    then
      log.important(
        label="icecast.server",
        "Mount #{path}: fallback has both a mount and a source, using the \
         source."
      )
    end
    if
      not null.defined(source)
      and null.defined(mount)
      and not string.contains(prefix="/", null.get(mount))
    then
      icecast.server.parameters.invalid(
        "mount #{path}: fallback.mount",
        "#{string.quote(null.get(mount))} does not start with /"
      )
    end
    ignore((fallback : unit))
    {mount = null.defined(source) ? null : mount, source = source}
  end

  mount =
    {
      source_user =
        value(
          icecast.server.parameters.any,
          m?.source_user ?? getter("source")
        ),
      source_password =
        value(
          icecast.server.parameters.any,
          m?.source_password ?? getter(server.source_password)
        ),
      source_decision = m?.source_decision,
      on_connect = m?.on_connect,
      on_disconnect = m?.on_disconnect,
      linger =
        value(check("linger").optional_duration, m?.linger ?? getter(null)),
      max_listeners =
        value(
          check("max_listeners").optional_count,
          m?.max_listeners ?? getter(null)
        ),
      listener_decision = m?.listener_decision,
      max_listener_duration =
        value(
          check("max_listener_duration").optional_duration,
          m?.max_listener_duration ?? getter(null)
        ),
      fallback =
        null.case(
          m?.fallback,
          {null},
          fun (fallback) ->
            begin
              fallback = resolve_fallback(fallback)
              null.defined(fallback.mount) or null.defined(fallback.source)
                ? fallback
                : null
            end
        ),
      fallback_when_full =
        value(
          icecast.server.parameters.any,
          m?.fallback_when_full ?? getter(false)
        ),
      hidden = value(icecast.server.parameters.any, m?.hidden ?? getter(false)),
      max_history =
        value(
          check("max_history").range(1, 256),
          m?.max_history ?? getter(server.max_history)
        ),
      health_password =
        value(
          icecast.server.parameters.any,
          m?.health_password ?? getter(server.health_password)
        ),
      burst_size =
        value(
          check("burst_size").count,
          m?.burst_size ?? getter(server.burst_size)
        ),
      dedicated_encoder =
        value(
          icecast.server.parameters.any,
          m?.dedicated_encoder ?? getter(server.dedicated_encoder)
        ),
      http_headers =
        value(check("http_headers").headers, m?.http_headers ?? []),
      dump_file =
        value(icecast.server.parameters.any, m?.dump_file ?? getter(null)),
      dump_file_size_limit =
        value(
          check("dump_file_size_limit").optional_count,
          m?.dump_file_size_limit ?? getter(null)
        ),
      dump_file_time_limit =
        value(
          check("dump_file_time_limit").optional_duration,
          m?.dump_file_time_limit ?? getter(null)
        )
    }
  ignore((m : unit?))
  mount
end
