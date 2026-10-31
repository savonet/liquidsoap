# Parameters of icecast.server (parameters.md §2 to §4, §7). The server
# calls `icecast.server.parameters.server(parameters)` at startup and at each
# reload: it validates the record and returns it complete, raising
# `error.icecast.server.invalid_parameters` when it is invalid. In the complete
# record a field read at use that is not a callback is a `() -> T`, which the
# consumer evaluates once per use: it reads the getter given for the field and
# validates its value, so an invalid value fails that use. Startup and mount
# output fields are plain values. Validators are in parameter_checks.liq, and
# `icecast.server.parameters.mount` in mount_parameters.liq.
# @flag hidden
def icecast.server.parameters.server(p) =
  check = icecast.server.parameters.check

  compatibility = p?.compatibility ?? "icecast-2.5"
  if
    not list.mem(compatibility, icecast.server.compatibilities)
  then
    icecast.server.parameters.invalid(
      "compatibility",
      "#{string.quote(compatibility)} is not one of #{
        icecast.server.compatibilities
      }"
    )
  end

  {
    name = p?.name ?? "icecast.server",
    port =
      icecast.server.parameters.checked(
        check("port").range(0, 65535),
        p?.port ?? 8000
      ),
    bind_address = p?.bind_address,
    transport = p?.transport ?? http.transport.unix,
    compatibility = compatibility,
    hostname =
      icecast.server.parameters.use(
        icecast.server.parameters.any,
        p?.hostname ?? getter("localhost")
      ),
    source_password =
      icecast.server.parameters.use(
        icecast.server.parameters.any,
        p?.source_password ?? getter("hackme")
      ),
    shoutcast_mount =
      icecast.server.parameters.use(
        icecast.server.parameters.any,
        p?.shoutcast_mount ?? getter("/stream")
      ),
    admin_user =
      icecast.server.parameters.use(
        icecast.server.parameters.any,
        p?.admin_user ?? getter("admin")
      ),
    admin_password =
      icecast.server.parameters.use(
        icecast.server.parameters.any,
        p?.admin_password ?? getter(null)
      ),
    health_password =
      icecast.server.parameters.use(
        icecast.server.parameters.any,
        p?.health_password ?? getter(null)
      ),
    max_history =
      icecast.server.parameters.use(
        check("max_history").range(1, 256),
        p?.max_history ?? getter(10)
      ),
    location =
      icecast.server.parameters.use(
        icecast.server.parameters.any,
        p?.location ?? getter("Earth")
      ),
    admin_contact =
      icecast.server.parameters.use(
        icecast.server.parameters.any,
        p?.admin_contact ?? getter("")
      ),
    server_id =
      icecast.server.parameters.use(
        icecast.server.parameters.any,
        p?.server_id ?? getter("Liquidsoap/#{liquidsoap.version}")
      ),
    max_sources =
      icecast.server.parameters.use(
        check("max_sources").optional_count,
        p?.max_sources ?? getter(null)
      ),
    max_clients =
      icecast.server.parameters.use(
        check("max_clients").optional_count,
        p?.max_clients ?? getter(null)
      ),
    source_timeout =
      icecast.server.parameters.use(
        check("source_timeout").duration,
        p?.source_timeout ?? getter(10.)
      ),
    proxy_ips =
      icecast.server.parameters.use(
        check("proxy_ips").optional_ranges,
        p?.proxy_ips ?? getter(null)
      ),
    forwarded_address = p?.forwarded_address,
    address_hash = p?.address_hash ?? icecast.server.addresses.hash,
    status_page =
      icecast.server.parameters.use(
        icecast.server.parameters.any,
        p?.status_page ?? getter(true)
      ),
    status_access = p?.status_access,
    status_json = p?.status_json,
    status_html = p?.status_html,
    metrics =
      icecast.server.parameters.use(
        icecast.server.parameters.any,
        p?.metrics ?? getter(false)
      ),
    access_log =
      icecast.server.parameters.use(
        icecast.server.parameters.any,
        p?.access_log ?? getter(null)
      ),
    playlist_log =
      icecast.server.parameters.use(
        icecast.server.parameters.any,
        p?.playlist_log ?? getter(null)
      ),
    error_log =
      icecast.server.parameters.use(
        icecast.server.parameters.any,
        p?.error_log ?? getter(null)
      ),
    log_level =
      icecast.server.parameters.use(
        check("log_level").range(1, 5),
        p?.log_level ?? getter(4)
      ),
    log_size =
      icecast.server.parameters.use(
        check("log_size").count,
        p?.log_size ?? getter(10240000)
      ),
    log_archive =
      icecast.server.parameters.use(
        icecast.server.parameters.any,
        p?.log_archive ?? getter(false)
      ),
    event_bindings =
      icecast.server.parameters.use(
        check("event_bindings").bindings,
        p?.event_bindings ?? getter([])
      ),
    mount = p?.mount ?? fun (_) -> null,
    client_timeout =
      icecast.server.parameters.checked(
        check("client_timeout").duration,
        p?.client_timeout ?? 30.
      ),
    burst_on_connect = p?.burst_on_connect ?? true,
    burst_size =
      icecast.server.parameters.checked(
        check("burst_size").count,
        p?.burst_size ?? 65536
      ),
    http_headers =
      icecast.server.parameters.checked(
        check("http_headers").headers,
        p?.http_headers ?? []
      ),
    dedicated_encoder = p?.dedicated_encoder ?? false,
    muxer_options = p?.muxer_options
  }
end
