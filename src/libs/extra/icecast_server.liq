# Icecast server implementation using input.harbor.dynamic
# This allows liquidsoap to act as an icecast-compatible server

let icecast = ()
let icecast.server = ()

let error.icecast = ()
let error.icecast.server = ()
let error.icecast.server.mount_taken =
  error.register("error.icecast.server.mount_taken")

# @flag hidden
let settings.icecast.server =
  settings.make.void(
    "Icecast server settings"
  )

# @flag hidden
def icecast.server.no_tls_transport(_certificate, _key) =
  ignore(_certificate)
  ignore(_key)
  log.critical(
    label="icecast.server",
    "TLS is enabled but no TLS transport is available. Please install TLS or SSL \
     support and rebuild liquidsoap."
  )
  shutdown()
  http.transport.unix
end

# Preferred TLS transport for `icecast.server`. A pair of `(name, factory)` where `factory` takes a certificate path and an optional private key path and returns an HTTP transport.
# @flag extra
# @category Source / Input / Active
let settings.icecast.server.tls_transport =
  settings.make(
    description="Preferred TLS transport for icecast.server. A pair of (name, \
     factory) where factory takes a certificate path and an optional private key \
     path and returns an http transport.",
    ("none", icecast.server.no_tls_transport)
  )

%ifdef http.transport.tls
# TLS transport for `icecast.server` using `http.transport.tls`.
# @flag extra
# @category Source / Input / Active
def icecast.server.tls_transport(certificate, key) =
  http.transport.tls(certificate=certificate, key=key)
end

settings.icecast.server.tls_transport := ("tls", icecast.server.tls_transport)
%endif

%ifdef http.transport.ssl
# SSL transport for `icecast.server` using `http.transport.ssl`. Overrides TLS transport when both are available.
# @flag extra
# @category Source / Input / Active
def icecast.server.ssl_transport(certificate, key) =
  http.transport.ssl(certificate=certificate, key=key)
end

settings.icecast.server.tls_transport := ("ssl", icecast.server.ssl_transport)
%endif

# Default muxer options applied when remuxing incoming streams via `copy_encoder`, keyed by container format name. Used to enable live/streaming-compatible output for formats that require explicit opt-in.
# @flag extra
# @category Source / Input / Active
let settings.icecast.server.default_muxer_options =
  settings.make(
    description="Default muxer options applied when remuxing incoming streams, \
     keyed by container format name. Used to enable live/streaming-compatible \
     output for formats that require explicit opt-in.",
    [
      ("matroska", [("dash", "1"), ("live", "1")]),
      ("webm", [("dash", "1"), ("live", "1")])
    ]
  )

let icecast.server.config = ()
let icecast.server.config.mount = ()

# @flag hidden
def icecast.server.config.warn_unsupported(section, options) =
  list.iter(
    fun ((option, present)) ->
      if
        present
      then
        log.important(
          label="icecast.server",
          "Config option '#{option}' in <#{section}> is not supported"
        )
      end,
    options
  )
end

# @flag hidden
def icecast.server.config.section(section, doc, parse) =
  try
    parse(doc)
  catch err : [error.xml] do
    error.raise(
      error.invalid,
      "Invalid <#{section}> section in icecast config: #{err.message}"
    )
  end
end

# Headers from the `<header>` children of a `<http-headers>` element. CORS headers without an explicit value take the icecast 2.5 defaults. Access-Control-Allow-Origin is `*` because the request origin cannot be echoed back.
# @flag hidden
def icecast.server.config.headers_of_nodes(nodes) =
  cors_defaults =
    [
      ("Access-Control-Allow-Origin", "*"),
      (
        "Access-Control-Allow-Headers",
        "range, if-range"
      ),
      (
        "Access-Control-Expose-Headers",
        "content-range, icy-br, icy-description, icy-genre, icy-name, icy-pub, \
         icy-url"
      ),
      ("Access-Control-Max-Age", "300")
    ]

  def header((tag, node)) =
    let {name, type, value} = node.xml_params
    name = name ?? ""
    if
      tag != "header" or name == ""
    then
      null
    elsif
      type == "cors"
    then
      if
        list.assoc.mem(name, cors_defaults)
      then
        (name, value ?? list.assoc(name, cors_defaults))
      else
        log.important(
          label="icecast.server",
          "Unknown CORS header '#{name}', skipping"
        )
        null
      end
    else
      (name, value ?? "")
    end
  end

  list.filter_map(header, nodes)
end

# Parse a `<http-headers>` element.
# @flag hidden
def icecast.server.config.headers(doc) =
  let xml.parse (parsed :
    {
      "http-headers" as headers: {
        xml_children: [
          (string * {xml_params: {name: string?, type: string?, value: string?}}
          )
        ]
      }
    }
  ) = doc
  icecast.server.config.headers_of_nodes(parsed.headers.xml_children)
end

# Mount configuration with nothing set. Also the shape of every parsed mount.
# @flag hidden
let icecast.server.config.mount.empty =
  {
    type = "normal",
    mount_name = (null : string?),
    username = (null : string?),
    password = (null : string?),
    max_listeners = (null : int?),
    dump_file = (null : string?),
    burst_size = (null : int?),
    fallback_mount = (null : string?),
    hidden = false,
    on_connect_cmd = (null : string?),
    on_disconnect_cmd = (null : string?),
    http_headers = ([] : [(string * string)])
  }

# Parse one `<mount>` element.
# @flag hidden
def icecast.server.config.mount.parse(doc) =
  let xml.parse (parsed :
    {
      mount: {
        "mount-name" as mount_name: string?,
        username: string?,
        password: string?,
        "max-listeners" as max_listeners: int?,
        "dump-file" as dump_file: string?,
        "burst-size" as burst_size: int?,
        "fallback-mount" as fallback_mount: string?,
        hidden: int?,
        "on-connect" as on_connect: string?,
        "on-disconnect" as on_disconnect: string?,
        "http-headers" as http_headers?: {
          xml_children: [
            (
              string
              *
              {xml_params: {name: string?, type: string?, value: string?}}
            )
          ]
        },
        "fallback-override" as fallback_override: int?,
        "fallback-when-full" as fallback_when_full: int?,
        public: string?,
        intro: string?,
        "max-listener-duration" as max_listener_duration: string?,
        authentication?: {xml_params: [(string * string)]},
        xml_params: {type: string?}
      }
    }
  ) = doc
  m = parsed.mount

  icecast.server.config.warn_unsupported(
    "mount",
    [
      ("public", null.defined(m.public)),
      ("intro", null.defined(m.intro)),
      ("max-listener-duration", null.defined(m.max_listener_duration)),
      ("authentication", null.defined(m?.authentication))
    ]
  )
  list.iter(
    fun ((option, value)) ->
      if
        value == 0
      then
        log.critical(
          label="icecast.server",
          "Mount config '#{option}=0' is not supported (internal logic differs)"
        )
      end,
    [
      ("fallback-override", m.fallback_override),
      ("fallback-when-full", m.fallback_when_full)
    ]
  )

  http_headers =
    icecast.server.config.headers_of_nodes(
      null.map(fun (h) -> h.xml_children, m?.http_headers) ?? []
    )

  {
    type = m.xml_params.type ?? "normal",
    mount_name = m.mount_name,
    username = m.username,
    password = m.password,
    max_listeners = m.max_listeners,
    dump_file = m.dump_file,
    burst_size = m.burst_size,
    fallback_mount = m.fallback_mount,
    hidden = m.hidden == 1,
    on_connect_cmd = m.on_connect,
    on_disconnect_cmd = m.on_disconnect,
    http_headers = http_headers
  }
end

# Effective configuration for `mount`: its own `<mount>` entry, with the `type="default"` mount filling every unset option. Headers accumulate, default first.
# @flag hidden
def icecast.server.config.mount.resolve(~default, ~mounts, (mount:string)) =
  config =
    list.find(
      default=icecast.server.config.mount.empty,
      fun (m) -> m.mount_name == mount,
      mounts
    )

  {
    mount_name = config.mount_name,
    username = config.username ?? default.username,
    password = config.password ?? default.password,
    max_listeners = config.max_listeners ?? default.max_listeners,
    dump_file = config.dump_file ?? default.dump_file,
    burst_size = config.burst_size ?? default.burst_size,
    fallback_mount = config.fallback_mount ?? default.fallback_mount,
    hidden = config.hidden or default.hidden,
    on_connect_cmd = config.on_connect_cmd ?? default.on_connect_cmd,
    on_disconnect_cmd = config.on_disconnect_cmd ?? default.on_disconnect_cmd,
    http_headers = [...default.http_headers, ...config.http_headers]
  }
end

# Parse a `<listen-socket>` element.
# @flag hidden
def icecast.server.config.listen_socket(doc) =
  let xml.parse (parsed :
    {
      "listen-socket" as socket: {
        port: int?,
        "bind-address" as bind_address: string?,
        tls: string?,
        ssl: string?,
        "shoutcast-mount" as shoutcast_mount: string?
      }
    }
  ) = doc
  s = parsed.socket

  icecast.server.config.warn_unsupported(
    "listen-socket",
    [("shoutcast-mount", null.defined(s.shoutcast_mount))]
  )

  tls_value = s.tls ?? (s.ssl ?? "0")
  tls =
    if
      tls_value == "1" or tls_value == "auto_no_plain"
    then
      true
    elsif
      tls_value == "0" or tls_value == "disabled"
    then
      false
    else
      log.important(
        label="icecast.server",
        "Unsupported <tls> value '#{tls_value}'; TLS disabled. Use \
         'auto_no_plain' (icecast 2.5) or '1' (icecast 2.4) to enable TLS."
      )
      false
    end

  {port = s.port, bind_address = s.bind_address, tls = tls}
end

# Parse a `<limits>` element.
# @flag hidden
def icecast.server.config.limits(doc) =
  let xml.parse (parsed :
    {
      limits: {
        sources: int?,
        clients: int?,
        "client-timeout" as client_timeout: float?,
        "source-timeout" as source_timeout: float?,
        "burst-on-connect" as burst_on_connect: int?,
        "burst-size" as burst_size: int?,
        "queue-size" as queue_size: string?,
        "header-timeout" as header_timeout: string?
      }
    }
  ) = doc
  l = parsed.limits

  icecast.server.config.warn_unsupported(
    "limits",
    [
      ("queue-size", null.defined(l.queue_size)),
      ("header-timeout", null.defined(l.header_timeout))
    ]
  )

  {
    max_sources = l.sources,
    max_clients = l.clients,
    client_timeout = l.client_timeout,
    source_timeout = l.source_timeout,
    burst_on_connect = l.burst_on_connect,
    burst_size = l.burst_size
  }
end

# Parse a `<paths>` element.
# @flag hidden
def icecast.server.config.paths(doc) =
  let xml.parse (parsed :
    {
      paths: {
        logdir: string?,
        "tls-certificate" as tls_certificate: string?,
        "ssl-certificate" as ssl_certificate: string?,
        "tls-key" as tls_key: string?,
        basedir: string?,
        pidfile: string?,
        webroot: string?,
        adminroot: string?,
        "allow-ip" as allow_ip: string?,
        "deny-ip" as deny_ip: string?,
        "ssl-allowed-ciphers" as ssl_allowed_ciphers: string?,
        alias?: {xml_params: [(string * string)]}
      }
    }
  ) = doc
  p = parsed.paths

  icecast.server.config.warn_unsupported(
    "paths",
    [
      ("basedir", null.defined(p.basedir)),
      ("pidfile", null.defined(p.pidfile)),
      ("webroot", null.defined(p.webroot)),
      ("adminroot", null.defined(p.adminroot)),
      ("allow-ip", null.defined(p.allow_ip)),
      ("deny-ip", null.defined(p.deny_ip)),
      ("ssl-allowed-ciphers", null.defined(p.ssl_allowed_ciphers)),
      ("alias", null.defined(p?.alias))
    ]
  )

  {
    logdir = p.logdir,
    tls_certificate = p.tls_certificate ?? p.ssl_certificate,
    tls_key = p.tls_key
  }
end

# Parse an `<authentication>` element.
# @flag hidden
def icecast.server.config.authentication(doc) =
  let xml.parse (parsed :
    {authentication: {"source-password" as source_password: string?}}
  ) = doc
  {password = parsed.authentication.source_password}
end

# Burst for a mount: its own `<burst-size>`, else the global one. A `0` at either level, or `<burst-on-connect>0</burst-on-connect>`, disables bursting.
# @flag hidden
def icecast.server.config.burst_size(~limits, (burst_size:int?)) =
  def disabled(value) =
    null.defined(value) and null.get(value) == 0
  end
  burst_on_connect = (limits.burst_on_connect : int?)
  if
    disabled(burst_on_connect) or disabled(burst_size)
  then
    null
  else
    burst_size ?? (limits.burst_size : int?)
  end
end

# Configuration with nothing set, used when no config file is given.
# @flag hidden
let icecast.server.config.empty =
  {
    port = (null : int?),
    bind_address = (null : string?),
    tls = false,
    tls_certificate = (null : string?),
    tls_key = (null : string?),
    logdir = (null : string?),
    password = (null : string?),
    max_sources = (null : int?),
    max_clients = (null : int?),
    client_timeout = (null : float?),
    source_timeout = (null : float?),
    burst_on_connect = (null : int?),
    burst_size = (null : int?),
    http_headers = ([] : [(string * string)]),
    default_mount = icecast.server.config.mount.empty,
    mounts = []
  }

# Parse an icecast XML configuration. Repeated elements (`<mount>`, `<listen-socket>`) are split off the root untyped, then every section is parsed against its own annotation. Pure: applying the result (log directory, bind address) is the caller's job.
# @flag hidden
def icecast.server.config.parse(doc) =
  let xml.parse (doc : (string * {xml_children: [(string * _)]})) = doc
  let (root_name, root) = doc
  if
    root_name != "icecast"
  then
    error.raise(
      error.invalid,
      "Invalid icecast config: root element is <#{root_name}>, expected \
       <icecast>"
    )
  end

  def sections(name) =
    list.filter_map(
      fun ((tag, node)) ->
        if
          tag == name
        then
          xml.stringify(compact=true, (tag, node))
        else
          null
        end,
      root.xml_children
    )
  end

  # An absent section parses as an empty element.
  def section(name, parse) =
    icecast.server.config.section(
      name,
      list.hd(default="<#{name}/>", sections(name)),
      parse
    )
  end

  listen_sockets = sections("listen-socket")
  if
    list.length(listen_sockets) > 1
  then
    log.important(
      label="icecast.server",
      "Multiple listen-socket entries found, only the first one will be used"
    )
  end
  listen_socket = section("listen-socket", icecast.server.config.listen_socket)
  authentication =
    section("authentication", icecast.server.config.authentication)
  limits = section("limits", icecast.server.config.limits)
  paths = section("paths", icecast.server.config.paths)
  http_headers = section("http-headers", icecast.server.config.headers)

  mounts =
    list.map(
      fun (m) ->
        icecast.server.config.section(
          "mount",
          m,
          icecast.server.config.mount.parse
        ),
      sections("mount")
    )
  default_mount =
    list.find(
      default=icecast.server.config.mount.empty,
      fun (m) -> m.type == "default",
      mounts
    )
  mounts = list.filter(fun (m) -> m.type == "normal", mounts)

  {
    port = listen_socket.port,
    bind_address = listen_socket.bind_address,
    tls = listen_socket.tls,
    tls_certificate = paths.tls_certificate,
    tls_key = paths.tls_key,
    logdir = paths.logdir,
    password = authentication.password,
    max_sources = limits.max_sources,
    max_clients = limits.max_clients,
    client_timeout = limits.client_timeout,
    source_timeout = limits.source_timeout,
    burst_on_connect = limits.burst_on_connect,
    burst_size = limits.burst_size,
    http_headers = http_headers,
    default_mount = default_mount,
    mounts = mounts
  }
end

# @flag hidden
def icecast.server.config.load(path) =
  icecast.server.config.parse(file.contents(path))
end

# @flag hidden
def icecast.server.log_mount_config(label, prefix, config) =
  def show(value, ~default) =
    if null.defined(value) then string(null.get(value)) else default end
  end
  password_info =
    if
      null.defined(config.password)
    then
      "#{string.length(null.get(config.password))} characters"
    else
      "(global)"
    end
  lines =
    [
      "mount-name: #{config.mount_name ?? "(default)"}",
      "username: #{config.username ?? "source"}",
      "password: #{password_info}",
      "max-listeners: #{show(config.max_listeners, default="unlimited")}",
      "burst-size: #{show(config.burst_size, default="default")}",
      "fallback-mount: #{config.fallback_mount ?? "none"}",
      "hidden: #{config.hidden}",
      "on-connect: #{config.on_connect_cmd ?? "none"}",
      "on-disconnect: #{config.on_disconnect_cmd ?? "none"}",
      "http-headers: #{list.length(config.http_headers)} header(s)",
      ...list.map(
        fun ((k, v)) ->
          "  #{k}: #{v}",
        config.http_headers
      )
    ]
  list.iter(fun (line) -> log.info(label=label, "#{prefix}#{line}"), lines)
end

# @flag hidden
def icecast.server.log_config(
  label,
  {port, password, max_sources, global_headers, default_mount, mount_configs}
) =
  max_sources_str =
    if
      null.defined(max_sources)
    then
      string(null.get(max_sources))
    else
      "unlimited"
    end

  log.info(
    label=label,
    "=== Configuration Report ==="
  )
  log.info(
    label=label,
    "Port: #{port}"
  )
  log.info(
    label=label,
    "Password: #{string.length(password)} characters"
  )
  log.info(
    label=label,
    "Max sources: #{max_sources_str}"
  )
  log.info(
    label=label,
    "Global HTTP headers: #{list.length(global_headers)} header(s)"
  )
  list.iter(
    fun ((k, v)) ->
      log.info(
        label=label,
        "  #{k}: #{v}"
      ),
    global_headers
  )
  log.info(
    label=label,
    "Default mount configuration:"
  )
  icecast.server.log_mount_config(
    label,
    "  ",
    default_mount
  )
  if
    mount_configs != []
  then
    log.info(
      label=label,
      "Mount configurations:"
    )
    list.iter(
      fun (config) ->
        begin
          log.info(
            label=label,
            "- Mount: #{config.mount_name ?? "?"}"
          )
          icecast.server.log_mount_config(
            label,
            "    ",
            config
          )
        end,
      mount_configs
    )
  end
  log.info(
    label=label,
    "=== End Configuration Report ==="
  )
end

# Table of live mounts and the fallback links between them. An entry holds the mount's current source, the source it falls back to, and caller data built once when the entry is created (the output, stats).
# @flag hidden
def icecast.server.mount_table() =
  entries = ref([])

  def has(mount) =
    list.assoc.mem(mount, entries())
  end

  def get(mount) =
    list.assoc(mount, entries())
  end

  def all() =
    entries()
  end

  def dependents((mount:string)) =
    list.filter(
      fun ((_, e)) ->
        null.defined(e.fallback_mount) and null.get(e.fallback_mount) == mount,
      entries()
    )
  end

  # Attach `source` to `mount`. On the first connection the entry is created with `create`, which receives the two refs an output should follow. Every mount falling back on this one now sees `source`.
  def connect(
    ~(mount:string),
    ~(fallback_mount:string?),
    ~(source:source),
    create
  ) =
    if
      has(mount)
    then
      get(mount).source := source
    else
      source_ref = ref((source : source?))
      fallback_ref = ref((null : source?))
      if
        null.defined(fallback_mount) and has(null.get(fallback_mount))
      then
        fallback_ref := get(null.get(fallback_mount)).source()
      end
      entry =
        {
          source = source_ref,
          fallback = fallback_ref,
          fallback_mount = fallback_mount,
          data = create({source = source_ref, fallback = fallback_ref})
        }
      entries := [(mount, entry), ...entries()]
    end
    list.iter(fun ((_, e)) -> e.fallback := source, dependents(mount))
    get(mount)
  end

  # Detach the source from `mount`. Entries left with neither a source nor a fallback are removed and returned so the caller can shut their output down.
  def disconnect(mount) =
    if has(mount) then get(mount).source := null end
    list.iter(fun ((_, e)) -> e.fallback := null, dependents(mount))
    orphans =
      list.filter(
        fun ((_, e)) ->
          not null.defined(e.source()) and not null.defined(e.fallback()),
        entries()
      )
    entries :=
      list.filter(fun ((m, _)) -> not list.assoc.mem(m, orphans), entries())
    orphans
  end

  # The live source on `mount`, null when there is none or no such mount.
  def source((mount:string)) =
    if has(mount) then get(mount).source() else null end
  end

  {
    has = has,
    get = get,
    all = all,
    source = source,
    connect = connect,
    disconnect = disconnect
  }
end

# Listeners of one mount. `output.harbor` reports a disconnect by connection IP only, so one disconnect drops one listener with that connection IP.
# @flag hidden
def icecast.server.listener_table() =
  listeners = ref([])
  peak = ref(0)

  def connect(~connection_ip, listener) =
    listeners := [(connection_ip, listener), ...listeners()]
    peak := max(peak(), list.length(listeners()))
  end

  def disconnect(connection_ip) =
    listeners := list.assoc.remove(connection_ip, listeners())
  end

  {
    connect = connect,
    disconnect = disconnect,
    all = fun () -> list.map(snd, listeners()),
    peak = peak
  }
end

# Default prepare function for `icecast.server`. Builds a standard fallback source and calls `output.harbor`.
# @flag extra
# @category Source / Input / Active
def icecast.server.default_prepare(
  {
    copy_encoder,
    format,
    mount,
    headers,
    burst,
    timeout,
    port,
    transport,
    dumpfile,
    dedicated_encoder,
    muxer_opts
  }
) =
  mount_id = r/^\//.replace(fun (_) -> "", mount)
  fun (
    {
      source = s,
      fallback = f,
      mime_type,
      on_listener_connect,
      on_listener_disconnect
    }
  ) ->
    begin
      default_source =
        if
          null.defined(f)
        then
          (
            fallback(
              id="#{mount_id}.selector",
              [(s : source), (null.get(f) : source)]
            )
          :
            source
          )
        else
          s
        end
      o =
        output.harbor(
          id="#{mount_id}.output",
          fallible=true,
          mount=mount,
          port=port,
          transport=transport,
          headers=headers,
          timeout=timeout,
          burst=burst,
          dumpfile=dumpfile,
          dedicated_encoder=dedicated_encoder,
          format=mime_type ?? "",
          copy_encoder(format, options=muxer_opts),
          default_source
        )

      o.on_connect(synchronous=true, on_listener_connect)
      o.on_disconnect(synchronous=true, on_listener_disconnect)

      {clock = o.clock, shutdown = o.shutdown, last_metadata = o.last_metadata}
    end
end

# Default X-Forwarded-For IP extraction for `icecast.server`. Walks the
# `X-Forwarded-For` header from right to left, skipping any IP present in
# `proxy_ips`, and returns the first non-proxy IP. When `proxy_ips` is empty,
# returns the rightmost entry unconditionally (trust-all mode).
# @flag extra
# @category Source / Input / Active
def icecast.server.x_forwarded_for({ip, headers, proxy_ips}) =
  value = list.assoc(default="", "x-forwarded-for", headers)
  if
    value == ""
  then
    ip
  else
    ips = list.map(string.trim, string.split(separator=",", value))
    if
      proxy_ips == []
    then
      list.last(default=ip, ips)
    else
      list.fold(
        fun (acc, candidate) ->
          if
            null.defined(acc)
          then
            acc
          elsif
            list.mem(candidate, proxy_ips)
          then
            null
          else
            candidate
          end,
        null,
        list.rev(ips)
      )
        ?? ip
    end
  end
end

# Source authorization for `icecast.server`. Listener requests are always accepted. `is_taken` says whether a mount currently has a source, `active_sources` counts them.
# @flag hidden
def icecast.server.authorize(
  ~label,
  ~password,
  ~max_sources,
  ~mount_config,
  ~is_taken,
  ~active_sources,
  {user, password = provided_password, uri, method}
) =
  is_source =
    method == "PUT" or method == "POST" or method == "SOURCE" or method == "ICY"

  if
    not is_source
  then
    true
  elsif
    r/^\/admin(\/|\.cgi|$)/.test(uri)
  then
    log.important(
      label=label,
      "Rejecting source connection to reserved admin mountpoint #{uri}"
    )
    false
  elsif
    is_taken(uri)
  then
    log.important(
      label=label,
      "Mountpoint #{uri} already in use, rejecting"
    )
    error.raise(
      error.icecast.server.mount_taken,
      "Mountpoint in use"
    )
  else
    config = mount_config(uri)
    expected_user = config.username ?? "source"
    expected_pass = config.password ?? password
    if
      expected_pass != ""
      and not (user == expected_user and provided_password == expected_pass)
    then
      false
    elsif
      null.defined(max_sources) and active_sources() >= null.get(max_sources)
    then
      log.important(
        label=label,
        "Maximum number of sources (#{null.get(max_sources)}) reached, rejecting \
         connection"
      )
      false
    else
      true
    end
  end
end

# Listener IP as reported in stats: the connection IP unless X-Forwarded-For handling is configured.
# @flag hidden
def icecast.server.listener_ip(
  ~x_forwarded_for,
  ~proxy_ips,
  (
  info:
  {headers: [(string * string)], ip: string, protocol: string, uri: string}
  )
) =
  if
    null.defined(x_forwarded_for) or null.defined(proxy_ips)
  then
    callback = x_forwarded_for ?? icecast.server.x_forwarded_for
    callback(
      {
        ip = info.ip,
        headers = info.headers,
        protocol = info.protocol,
        uri = info.uri,
        proxy_ips = proxy_ips ?? []
      }
    )
      ?? info.ip
  else
    info.ip
  end
end

# @flag hidden
def icecast.server.source_manager(
  ~(name:string),
  ~port,
  ~transport,
  ~password,
  ~max_sources,
  ~max_clients,
  ~client_timeout,
  ~limits,
  ~dedicated_encoder=false,
  ~x_forwarded_for_proxy_ips=null,
  ~x_forwarded_for=null,
  ~format_options=null,
  ~global_headers,
  ~default_mount,
  ~mount_configs
) =
  # TODO: max_clients not yet implemented
  ignore(max_clients)

  server_clock = clock.create(id=name)
  mounts = icecast.server.mount_table()
  on_connect_handlers = ref([])
  on_disconnect_handlers = ref([])

  def mount_config(mount) =
    icecast.server.config.mount.resolve(
      default=default_mount,
      mounts=mount_configs,
      mount
    )
  end

  def muxer_options(format) =
    if
      not null.defined(format)
    then
      []
    elsif
      null.defined(format_options)
    then
      format_options_fn = null.get(format_options)
      format_options_fn(null.get(format))
    else
      list.assoc(
        default=[],
        null.get(format),
        settings.icecast.server.default_muxer_options()
      )
    end
  end

  def run_command(cmd) =
    if
      null.defined(cmd)
    then
      cmd = null.get(cmd)
      thread.run(fun () -> ignore(process.run(cmd)))
    end
  end

  # Build the output and stats for a mount on its first connection.
  def create_entry(
    ~mount,
    ~config,
    ~format,
    ~mime_type,
    ~streams,
    ~copy_encoder
  ) =
    fun ({source = main, fallback = backup}) ->
      begin
        mount_id = r/^\//.replace(fun (_) -> "", mount)
        listeners = icecast.server.listener_table()

        def on_listener_connect(
          (
          info:
          {
            headers: [(string * string)],
            ip: string,
            protocol: string,
            uri: string
          }
          )
        ) =
          ip =
            icecast.server.listener_ip(
              x_forwarded_for=x_forwarded_for,
              proxy_ips=x_forwarded_for_proxy_ips,
              info
            )
          listeners.connect(
            connection_ip=info.ip,
            {ip = ip, protocol = info.protocol, uri = info.uri}
          )
        end

        connector =
          icecast.server.default_prepare(
            {
              copy_encoder = copy_encoder,
              format = format,
              mount = mount,
              headers = [...global_headers, ...config.http_headers],
              burst =
                icecast.server.config.burst_size(
                  limits=limits,
                  config.burst_size
                ),
              timeout = client_timeout ?? 30.,
              port = port,
              transport = transport,
              dumpfile = config.dump_file,
              dedicated_encoder = dedicated_encoder,
              muxer_opts = muxer_options(format)
            }
          )
        output =
          connector(
            {
              source = source.dynamic(id="#{mount_id}.source", main),
              fallback =
                if
                  null.defined(config.fallback_mount)
                then
                  (source.dynamic(id="#{mount_id}.fallback", backup) : source?)
                else
                  null
                end,
              mime_type = mime_type,
              on_listener_connect = on_listener_connect,
              on_listener_disconnect = listeners.disconnect
            }
          )
        server_clock.unify(output.clock)

        {
          output = output,
          listeners = listeners,
          format = ref(format),
          streams = ref(streams),
          source_name = ref(""),
          mime_type = ref(mime_type),
          start_time = ref(time())
        }
      end
  end

  def handle_connection(c) =
    let {copy_encoder, uri = mount, format, mime_type, streams, headers} = c
    fun (s) ->
      begin
        on_source_disconnect = s.on_disconnect
        config = mount_config(mount)

        log.important(
          label=name,
          "New source connected on mount #{mount}"
        )

        entry =
          mounts.connect(
            mount=mount,
            fallback_mount=config.fallback_mount,
            source=s,
            create_entry(
              mount=mount,
              config=config,
              format=format,
              mime_type=mime_type,
              streams=streams,
              copy_encoder=copy_encoder
            )
          )
        entry.data.format := format
        entry.data.streams := streams
        entry.data.source_name := list.assoc(default="", "icy-name", headers)
        entry.data.mime_type := mime_type
        entry.data.start_time := time()

        run_command(config.on_connect_cmd)
        arg =
          {
            mount = mount,
            source = s,
            format = format,
            streams = streams,
            headers = headers
          }
        list.iter(fun (h) -> h(arg), on_connect_handlers())

        on_source_disconnect(
          synchronous=true,
          fun () ->
            begin
              log.important(
                label=name,
                "Source disconnected from mount #{mount}"
              )
              list.iter(
                fun ((orphan, e)) ->
                  begin
                    log.info(
                      label=name,
                      "Shutting down output for mount #{orphan}"
                    )
                    e.data.output.shutdown()
                  end,
                mounts.disconnect(mount)
              )
              run_command(config.on_disconnect_cmd)
              list.iter(fun (h) -> h({mount = mount}), on_disconnect_handlers())
            end
        )
      end
  end

  def auth(args) =
    icecast.server.authorize(
      label=name,
      password=password,
      max_sources=max_sources,
      mount_config=mount_config,
      is_taken=fun (uri) -> null.defined(mounts.source(uri)),
      active_sources=fun () ->
        list.length(
          list.filter(
            fun ((mount, _)) -> null.defined(mounts.source(mount)),
            mounts.all()
          )
        ),
      args
    )
  end

  def add_handler(handlers, ~synchronous, handler) =
    wrapped =
      if
        synchronous
      then
        handler
      else
        fun (arg) -> thread.run(fun () -> handler(arg))
      end
    handlers := [...handlers(), wrapped]
  end

  def on_connect(~synchronous=false, handler) =
    add_handler(on_connect_handlers, synchronous=synchronous, handler)
  end

  def on_disconnect(~synchronous=false, handler) =
    add_handler(on_disconnect_handlers, synchronous=synchronous, handler)
  end

  def get_config(mount) =
    if
      mounts.has(mount)
    then
      entry = mounts.get(mount)
      {format = entry.data.format(), streams = entry.data.streams()}
    else
      null
    end
  end

  def stats() =
    list.map(
      fun ((mount, entry)) ->
        (
          mount,
          {
            name = entry.data.source_name(),
            content_type = entry.data.format() ?? "",
            mime_type = entry.data.mime_type(),
            started = entry.data.start_time(),
            listeners = entry.data.listeners.all(),
            peak_listeners = entry.data.listeners.peak(),
            current_metadata = entry.data.output.last_metadata() ?? []
          }
        ),
      mounts.all()
    )
  end

  server.register(
    namespace=name,
    description="List active mounts on the icecast server",
    "mounts",
    fun (_) ->
      begin
        def status(s) =
          if null.defined(s()) then "connected" else "disconnected" end
        end
        lines =
          list.map(
            fun ((mount, entry)) ->
              "#{mount}: source=#{status(entry.source)}, fallback=#{
                status(entry.fallback)
              }, format=#{entry.data.format()}, streams=#{
                entry.data.streams()
              }",
            mounts.all()
          )
        if
          lines == []
        then
          "No active mounts"
        else
          string.concat(separator="\n", lines)
        end
      end
  )

  {
    auth = auth,
    handle_connection = handle_connection,
    get_config = get_config,
    mounts = fun () -> list.map(fst, mounts.all()),
    get_source = mounts.source,
    on_connect = on_connect,
    on_disconnect = on_disconnect,
    stats = stats
  }
end

# @flag hidden
def icecast.server.serve(
  ~port,
  ~transport,
  ~auth=null,
  ~json=null,
  ~html=null,
  stats
) =
  css =
    "*{box-sizing:border-box;margin:0;padding:0}body{font-family:system-ui,-apple-system,sans-serif;background:#f0f2f5;color:#222;line-height:1.5}header{background:#16213e;color:#fff;padding:1.25rem \
     2rem;display:flex;align-items:center;justify-content:space-between}header \
     h1{font-size:1.3rem;font-weight:500}#status{font-size:.8rem;opacity:.7}main{max-width:860px;margin:2rem \
     auto;padding:0 \
     1rem}.empty{text-align:center;color:#888;padding:4rem;background:#fff;border-radius:8px}.card{background:#fff;border-radius:8px;padding:1.5rem;margin-bottom:1rem;box-shadow:0 \
     1px 3px \
     rgba(0,0,0,.08)}.card-title{font-size:1.05rem;font-weight:600;color:#16213e;margin-bottom:.75rem;font-family:monospace}.now-playing{background:#eef2ff;color:#3730a3;border-radius:4px;padding:.4rem \
     .75rem;margin-bottom:.75rem;font-size:.9rem}.now-playing::before{content:'▶ \
     ';opacity:.7}.meta{display:flex;flex-wrap:wrap;gap:.75rem \
     2rem;margin-bottom:1rem;font-size:.875rem}.meta-item{display:flex;flex-direction:column}.meta-label{font-size:.7rem;text-transform:uppercase;letter-spacing:.5px;color:#888;margin-bottom:.15rem}hr{border:none;border-top:1px \
     solid #f0f0f0;margin:.75rem \
     0}.stats-row{display:flex;align-items:center;gap:2rem}.stat{text-align:center}.stat-n{font-size:1.6rem;font-weight:600;color:#16213e;line-height:1}.stat-l{font-size:.7rem;text-transform:uppercase;color:#888;margin-top:.2rem}"

  js =
    "async function refresh(){try{const r=await fetch('/status.json');const \
     d=await \
     r.json();render(d.mounts);document.getElementById('status').textContent='Updated \
     '+new \
     Date().toLocaleTimeString();}catch(e){document.getElementById('status').textContent='Connection \
     error.';}}function meta(m,k){const e=m.find(x=>x.key===k);return \
     e?e.value:'';}function fmt(ts){return new \
     Date(ts*1000).toLocaleString();}function render(mounts){const \
     el=document.getElementById('mounts');if(!mounts||!mounts.length){el.innerHTML='<p \
     class=empty>No active mounts.</p>';return;}const ids=new \
     Set(mounts.map(m=>m.mount));Array.from(el.children).forEach(c=>{if(!c.dataset.mount)c.remove();});Array.from(el.querySelectorAll('[data-mount]')).forEach(c=>{if(!ids.has(c.dataset.mount))c.remove();});mounts.forEach(m=>{let \
     card=el.querySelector('[data-mount=\"'+m.mount+'\"]');if(!card){card=document.createElement('div');card.className='card';card.dataset.mount=m.mount;card.innerHTML=`<div \
     class=card-title><a href='${m.mount}' target='_blank' rel='noopener \
     noreferrer' \
     style='color:inherit;text-decoration:none'>${m.mount}</a></div><div \
     class=now-playing></div><div class=meta><div class=meta-item><span \
     class=meta-label>Name</span><span class=dyn-name></span></div><div \
     class=meta-item><span class=meta-label>Format</span><span \
     class=dyn-format></span></div><div class=meta-item><span \
     class=meta-label>Started</span><span \
     class=dyn-started></span></div></div><hr><div class=stats-row><div \
     class=stat><div class=stat-n dyn-listeners></div><div \
     class=stat-l>Listeners</div></div><div class=stat><div class=stat-n \
     dyn-peak></div><div \
     class=stat-l>Peak</div></div></div>${m.mime_type&&m.mime_type.startsWith('video/')?`<video \
     controls preload='none' src='${m.mount}' type='${m.mime_type}' \
     style='width:100%;margin-top:1rem'></video>`:`<audio controls \
     preload='none' src='${m.mount}'${m.mime_type?` type='${m.mime_type}'`:''} \
     style='width:100%;margin-top:1rem;accent-color:#16213e'></audio>`}`;el.appendChild(card);}const \
     title=meta(m.current_metadata,'title');const \
     artist=meta(m.current_metadata,'artist');const \
     np=[artist,title].filter(Boolean).join(' \u2013 ');const \
     npEl=card.querySelector('.now-playing');npEl.textContent=np;npEl.style.display=np?'':'none';card.querySelector('.dyn-name').textContent=m.name||m.mount;card.querySelector('.dyn-format').textContent=m.mime_type||m.content_type;card.querySelector('.dyn-started').textContent=fmt(m.started);card.querySelector('[dyn-listeners]').textContent=m.listener_count;card.querySelector('[dyn-peak]').textContent=m.peak_listeners;});}refresh();setInterval(refresh,5000);"

  html_page =
    "<!DOCTYPE html><html lang='en'><head><meta charset='UTF-8'><meta \
     name='viewport' \
     content='width=device-width,initial-scale=1'><title>Liquidsoap \
     Status</title><style>#{css}</style></head><body><header><h1>Liquidsoap \
     Status</h1><span id='status'></span></header><main id='mounts'><p \
     class='empty'>Loading\u2026</p></main><script>#{js}</script></body></html>"

  def wrap_auth(handler) =
    fun (req, response) ->
      begin
        if
          null.defined(auth)
        then
          auth_fn = null.get(auth)
          if
            not auth_fn(req)
          then
            response.status_code(401)
            response.header(
              "WWW-Authenticate",
              "Basic realm=\"Liquidsoap\""
            )
            response.data("")
          else
            handler(req, response)
          end
        else
          handler(req, response)
        end
      end
  end

  def html_handler(_, response) =
    if
      null.defined(html)
    then
      html_fn = null.get(html)
      response.html(html_fn(stats()))
    else
      response.html(html_page)
    end
  end

  def json_handler(_, response) =
    if
      null.defined(json)
    then
      json_fn = null.get(json)
      response.header("Content-Type", "application/json")
      response.data(json_fn(stats()))
    else
      mount_stats =
        list.map(
          fun ((mount, s)) ->
            {
              mount = mount,
              name = s.name,
              content_type = s.content_type,
              mime_type = s.mime_type,
              started = s.started,
              listener_count = list.length(s.listeners),
              peak_listeners = s.peak_listeners,
              current_metadata =
                list.map(
                  fun ((k, v)) -> {key = k, value = v},
                  s.current_metadata
                )
            },
          stats()
        )
      response.json({mounts = mount_stats})
    end
  end

  harbor.http.register(
    port=port,
    transport=transport,
    method="GET",
    "/",
    wrap_auth(html_handler)
  )
  harbor.http.register(
    port=port,
    transport=transport,
    method="GET",
    "/status.json",
    wrap_auth(json_handler)
  )
end

# Start an icecast-compatible server
# @flag extra
# @category Source / Input / Active
# @param ~name Telnet command namespace
# @param ~port Port to listen on
# @param ~password Source password for authentication
# @param ~dedicated_encoder When `true`, create a separate encoder instance for each listener. This uses more resources but ensures each listener gets a clean encoder state.
# @param ~config Optional path to an icecast XML configuration file
# @param ~serve When `false`, disable the status page at `/` and JSON stats at `/status.json`.
# @param ~serve_auth When provided, called with the request and must return `true` to allow access to the status endpoints.
# @param ~serve_json When provided, called with the stats list and must return a JSON string for the `/status.json` endpoint.
# @param ~serve_html When provided, called with the stats list and must return an HTML string for the `/` endpoint.
# @param ~x_forwarded_for_proxy_ips List of known reverse-proxy IPs. When set, the `X-Forwarded-For` header is walked right-to-left and the first non-proxy IP is used as the listener IP in stats. Use only when liquidsoap is behind a trusted reverse proxy.
# @param ~x_forwarded_for Advanced: custom callback `({ip, headers, proxy_ips, protocol, uri}) -> string` to extract the real listener IP. When set, overrides the default `X-Forwarded-For` logic. `proxy_ips` in the record is populated from `x_forwarded_for_proxy_ips`.
# @param ~format_options Callback `(string) -> [(string * string)]` that returns muxer options for a given container format name. When `null`, falls back to `settings.icecast.server.default_muxer_options`.
# @callback on_connect Called when a source connects. Receives a record with mount, source, format, streams, and headers fields.
# @callback on_disconnect Called when a source disconnects. Receives a record with a mount field.
# @method stats Returns a list of `(mount, stats)` pairs. Each stats record contains: name, content_type, started, listeners, peak_listeners, and current_metadata.
def replaces icecast.server(
  ~name="icecast.server",
  ~port=8000,
  ~password="hackme",
  ~dedicated_encoder=false,
  ~x_forwarded_for_proxy_ips=null,
  ~x_forwarded_for=null,
  ~format_options=null,
  ~config=null,
  ~serve=true,
  ~serve_auth=null,
  ~serve_json=null,
  ~serve_html=null
) =
  settings.frame.duration.set_default(0.2)

  parsed =
    if
      null.defined(config)
    then
      icecast.server.config.load(null.get(config))
    else
      icecast.server.config.empty
    end

  port = parsed.port ?? port
  password = parsed.password ?? password
  max_sources = parsed.max_sources

  if
    null.defined(parsed.logdir)
  then
    settings.log.file := true
    settings.log.file.path := "#{null.get(parsed.logdir)}/<script>.log"
    settings.log.stdout := true
  end

  if
    null.defined(parsed.bind_address)
  then
    settings.harbor.bind_addrs := [null.get(parsed.bind_address)]
  end

  transport =
    if
      parsed.tls
    then
      if
        not null.defined(parsed.tls_certificate)
      then
        error.raise(
          error.invalid,
          "TLS is enabled but no tls-certificate path is configured"
        )
      end
      let (tls_name, tls_factory) = settings.icecast.server.tls_transport()
      log.important(
        label=name,
        "Using TLS transport: #{tls_name}"
      )
      tls_factory(null.get(parsed.tls_certificate), parsed.tls_key)
    else
      http.transport.unix
    end

  log.important(
    label=name,
    "Initializing icecast emulation server on port #{port}"
  )
  if
    null.defined(max_sources)
  then
    log.important(
      label=name,
      "Maximum sources: #{null.get(max_sources)}"
    )
  end
  if
    parsed.mounts != []
  then
    log.important(
      label=name,
      "Loaded #{list.length(parsed.mounts)} mount configuration(s)"
    )
  end

  icecast.server.log_config(
    name,
    {
      port = port,
      password = password,
      max_sources = max_sources,
      global_headers = parsed.http_headers,
      default_mount = parsed.default_mount,
      mount_configs = parsed.mounts
    }
  )

  manager =
    icecast.server.source_manager(
      name=name,
      port=port,
      transport=transport,
      password=password,
      max_sources=max_sources,
      max_clients=parsed.max_clients,
      client_timeout=parsed.client_timeout,
      limits=parsed,
      dedicated_encoder=dedicated_encoder,
      x_forwarded_for_proxy_ips=x_forwarded_for_proxy_ips,
      x_forwarded_for=x_forwarded_for,
      format_options=format_options,
      global_headers=parsed.http_headers,
      default_mount=parsed.default_mount,
      mount_configs=parsed.mounts
    )

  input.harbor.dynamic(
    "/:mount",
    icy=true,
    port=port,
    transport=transport,
    timeout=parsed.source_timeout ?? 30.,
    auth=manager.auth,
    on_connect=manager.handle_connection
  )

  server.register(
    namespace=name,
    description="Show icecast server configuration",
    "config",
    fun (_) ->
      begin
        max_sources_str =
          if
            null.defined(max_sources)
          then
            string(null.get(max_sources))
          else
            "unlimited"
          end
        lines =
          [
            "Port: #{port}",
            "Password: #{string.length(password)} characters",
            "Max sources: #{max_sources_str}",
            "Global HTTP headers: #{list.length(parsed.http_headers)}",
            "Mount configurations: #{list.length(parsed.mounts)}",
            ...list.map(
              fun (config) ->
                "  - #{config.mount_name ?? "(default)"}",
              parsed.mounts
            )
          ]
        string.concat(separator="\n", lines)
      end
  )

  if
    serve
  then
    icecast.server.serve(
      port=port,
      transport=transport,
      auth=serve_auth,
      json=serve_json,
      html=serve_html,
      manager.stats
    )
  end

  {
    mounts = manager.mounts,
    get_source = manager.get_source,
    get_config = manager.get_config,
    on_connect = manager.on_connect,
    on_disconnect = manager.on_disconnect,
    stats = manager.stats
  }
end
