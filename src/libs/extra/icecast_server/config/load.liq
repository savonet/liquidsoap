# @flag hidden
let icecast.server.config.root_ignored =
  [
    (
      "fileserve",
      "incompatible, the server does not serve static files"
    ),
    (
      "master*",
      "future work, the server never pulls streams from another"
    ),
    (
      "max-redirect-slaves",
      "future work, relay clusters"
    ),
    (
      "redirect",
      "future work, relay clusters"
    ),
    (
      "relay",
      "future work, relays"
    ),
    (
      "relays-on-demand",
      "future work, relays"
    ),
    (
      "include",
      "not implemented, the file is a single document"
    ),
    (
      "inactivity-timeout",
      "incompatible, request headers are read by the HTTP layer"
    ),
    (
      "header-timeout",
      "incompatible, request headers are read by the HTTP layer"
    ),
    (
      "directory",
      "not implemented, directory registration"
    ),
    (
      "yp-directory",
      "not implemented, directory registration"
    ),
    (
      "security",
      "incompatible, chroot and user change belong to the service manager"
    )
  ]

# @flag hidden
let icecast.server.config.root_known =
  [
    "hostname",
    "location",
    "admin",
    "server-id",
    "server_id",
    "listen-socket",
    "port",
    "bind-address",
    "source-password",
    "shoutcast-mount",
    "authentication",
    "limits",
    "mount",
    "paths",
    "logging",
    "http-headers",
    "custom-headers",
    "event-bindings",
    "kartoffelsalat"
  ]

# The transport of the listening socket: TLS when `tls` asks for it, built
# with the factory of `settings.icecast.server.tls_transport`.
# @flag hidden
def icecast.server.config.transport(~tls, ~certificate, ~key) =
  if
    not null.defined(tls) and null.defined(certificate)
  then
    icecast.server.config.reject(
      "A TLS certificate is configured without a <tls> mode: Icecast would serve \
       TLS and plain HTTP on one port, which this server cannot do. Set <tls> to \
       rfc2818 for TLS or disabled for plain HTTP."
    )
  end
  if
    tls == true
  then
    if
      not null.defined(certificate)
    then
      icecast.server.config.reject(
        "TLS is enabled but no certificate is configured."
      )
    end
    let (name, factory) = settings.icecast.server.tls_transport()
    if
      name == "none"
    then
      icecast.server.config.reject(
        "TLS is enabled but this build of liquidsoap has no TLS support."
      )
    end
    factory(certificate, key)
  else
    http.transport.unix
  end
end

# @flag hidden
def icecast.server.config.translate(~compatibility, root) =
  let (tag, _) = root
  if
    tag != "icecast"
  then
    icecast.server.config.reject(
      "the root element is <#{tag}>, not <icecast>."
    )
  end
  icecast.server.config.warn_children(
    known=icecast.server.config.root_known,
    ignored=icecast.server.config.root_ignored,
    root
  )
  def text(tag) =
    xml.string(xml.child(tag, root))
  end

  listen = icecast.server.config.listen(root)
  authentication = icecast.server.config.authentication(root)
  limits = icecast.server.config.limits(root)
  paths = icecast.server.config.paths(root)
  logging = icecast.server.config.logging(logdir=paths.logdir, root)
  cache = icecast.server.preroll_cache(directory=paths.webroot ?? ".")
  access =
    [
      ...list.map(
        fun (deny) -> icecast.server.decision.access_lists(deny=deny),
        paths.deny
      ),
      ...(
        null.defined(paths.allow) or null.defined(paths.agents)
          ? [
            icecast.server.decision.access_lists(
              allow=paths.allow,
              agents=paths.agents
            )
          ]
          : []
      )
    ]
  entries =
    list.filter_map(
      fun (mount) ->
        icecast.server.config.mount_entry(
          compatibility=compatibility,
          cache=cache,
          mount
        ),
      xml.children("mount", root)
    )
  proxy_ips = [...listen.trusted_proxies, ...paths.forwarded_for]
  {
    compatibility = compatibility,
    port = listen.port,
    bind_address = listen.bind_address,
    transport =
      icecast.server.config.transport(
        tls=listen.tls,
        certificate=paths.certificate,
        key=paths.key
      ),
    hostname = text("hostname"),
    location = text("location"),
    admin_contact = text("admin"),
    server_id = text("server-id") ?? text("server_id"),
    source_password = authentication.source_password ?? text("source-password"),
    shoutcast_mount = listen.shoutcast_mount,
    admin_user = authentication.admin_user,
    admin_password = authentication.admin_password,
    health_password = authentication.health_password,
    max_sources = limits.max_sources,
    max_clients = limits.max_clients,
    client_timeout = limits.client_timeout,
    source_timeout = limits.source_timeout,
    burst_size = limits.burst_size,
    burst_on_connect = limits.burst_on_connect,
    proxy_ips = list.is_empty(proxy_ips) ? null : proxy_ips,
    access_log = logging.access_log,
    error_log = logging.error_log,
    playlist_log = logging.playlist_log,
    log_level = logging.log_level,
    log_size = logging.log_size,
    log_archive = logging.log_archive,
    http_headers =
      icecast.server.config.headers(compatibility=compatibility, root),
    event_bindings =
      [
        ...icecast.server.config.event_bindings(root),
        ...list.flatten(list.map(fun (e) -> e.bindings, entries))
      ],
    mount =
      icecast.server.config.mount_callback(
        kh=compatibility == "kh",
        cache=cache,
        decisions=access,
        max_listeners=limits.max_listeners,
        entries
      )
  }
end

# The parameters of an Icecast, RSAS or Icecast-KH configuration file
# (icecast-config.md), for `icecast.server` and its `reload`. Elements the
# translation does not reproduce are ignored with a warning; a file it cannot
# honour safely raises `error.icecast.server.invalid_parameters`.
# @category Internet
# @flag extra
# @param ~compatibility The implementation to mimic: `"icecast-2.5"`, `"icecast-2.4"`, `"rsas"` or `"kh"`.
# @param path Configuration file.
def icecast.server.config.load(~compatibility="icecast-2.5", path) =
  if
    not list.mem(compatibility, icecast.server.compatibilities)
  then
    icecast.server.config.reject(
      "compatibility: #{string.quote(compatibility)} is not one of #{
        icecast.server.compatibilities
      }"
    )
  end
  log.info(
    label="icecast.server.config",
    "Translating #{path}."
  )
  parameters =
    try
      icecast.server.config.translate(
        compatibility=compatibility,
        xml.tree(file.contents(path))
      )
    catch err : [error.xml] do
      icecast.server.config.reject(
        "#{path}: #{err.message}"
      )
    end
  ignore(icecast.server.parameters.server(parameters))
  parameters
end
