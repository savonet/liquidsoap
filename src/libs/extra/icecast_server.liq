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
let icecast.server.config.logging = ()

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
    {
      authentication: {
        "source-password" as source_password: string?,
        "admin-user" as admin_user: string?,
        "admin-password" as admin_password: string?
      }
    }
  ) = doc
  a = parsed.authentication
  {
    password = a.source_password,
    admin_user = a.admin_user,
    admin_password = a.admin_password
  }
end

# Logging configuration with nothing set.
# @flag hidden
let icecast.server.config.logging.empty =
  {
    accesslog = (null : string?),
    errorlog = (null : string?),
    playlistlog = (null : string?),
    loglevel = (null : int?),
    logsize = (null : int?),
    logarchive = false
  }

# Icecast log level of a `<loglevel>`, given as a number (1 error to 4 debug) or a name.
# @flag hidden
def icecast.server.config.logging.level(loglevel) =
  levels =
    [
      ("1", 1),
      ("error", 1),
      ("eror", 1),
      ("2", 2),
      ("warning", 2),
      ("warn", 2),
      ("3", 3),
      ("information", 3),
      ("info", 3),
      ("4", 4),
      ("debug", 4),
      ("dbug", 4)
    ]
  level = list.assoc.nullable(string.case(lower=true, loglevel), levels)
  if
    not null.defined(level)
  then
    log.important(
      label="icecast.server",
      "Unsupported <loglevel> value '#{loglevel}', ignoring"
    )
  end
  level
end

# Parse a `<logging>` element.
# @flag hidden
def icecast.server.config.logging.parse(doc) =
  let xml.parse (parsed :
    {
      logging: {
        accesslog: string?,
        errorlog: string?,
        playlistlog: string?,
        loglevel: string?,
        logsize: int?,
        logarchive: int?,
        memorybacklog?: {xml_params: [(string * string)]}
      }
    }
  ) = doc
  l = parsed.logging

  icecast.server.config.warn_unsupported(
    "logging",
    [("memorybacklog", null.defined(l?.memorybacklog))]
  )

  {
    accesslog = l.accesslog,
    errorlog = l.errorlog,
    playlistlog = l.playlistlog,
    loglevel = null.map(icecast.server.config.logging.level, l.loglevel),
    logsize = l.logsize,
    logarchive = l.logarchive == 1
  }
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
    logging = icecast.server.config.logging.empty,
    password = (null : string?),
    admin_user = (null : string?),
    admin_password = (null : string?),
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
  logging = section("logging", icecast.server.config.logging.parse)
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
    logging = logging,
    password = authentication.password,
    admin_user = authentication.admin_user,
    admin_password = authentication.admin_password,
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

# Effective `icecast.server` settings: explicit parameters first, then the config file, then defaults. Log files named in the config live in `<logdir>`, and icecast's default log names only apply when a config file is loaded.
# @flag hidden
def icecast.server.config.resolve(
  ~port,
  ~password,
  ~access_log,
  ~playlist_log,
  ~admin_user,
  ~admin_password,
  ~from_file,
  parsed
) =
  logging = parsed.logging

  def log_path(name) =
    if
      null.defined(name) and null.get(name) == "-"
    then
      "-"
    elsif
      null.defined(name) and null.defined(parsed.logdir)
    then
      path.concat(null.get(parsed.logdir), null.get(name))
    else
      null
    end
  end

  def first((param:string?), (fallback:string?)) =
    if null.defined(param) then param else fallback end
  end

  def file_default(name, (default:string)) =
    if from_file then (name ?? default : string?) else name end
  end

  {
    port = port ?? (parsed.port ?? 8000),
    password = password ?? (parsed.password ?? "hackme"),
    error_log = log_path(file_default(logging.errorlog, "error.log")),
    access_log =
      first(
        access_log,
        log_path(file_default(logging.accesslog, "access.log"))
      ),
    playlist_log = first(playlist_log, log_path(logging.playlistlog)),
    admin_user = admin_user ?? (parsed.admin_user ?? "admin"),
    admin_password = first(admin_password, parsed.admin_password)
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

# Listener counters of one mount. Live listeners belong to the mount's output, which pairs every synchronous connect with exactly one later disconnect, so `current` cannot drift.
# @flag hidden
def icecast.server.listener_counters() =
  current = ref(0)
  peak = ref(0)
  connections = ref(0)
  listening_time = ref(0.)
  bytes_sent = ref(0)

  def connect() =
    atomic(
      fast=true,
      fun () ->
        begin
          current := current() + 1
          connections := connections() + 1
          peak := max(peak(), current())
        end
    )
  end

  def disconnect(listener) =
    atomic(
      fast=true,
      fun () ->
        begin
          current := current() - 1
          listening_time := listening_time() + listener.duration
          bytes_sent := bytes_sent() + listener.bytes_sent
        end
    )
  end

  def get() =
    atomic(
      fast=true,
      fun () ->
        {
          peak = peak(),
          connections = connections(),
          listening_time = listening_time(),
          bytes_sent = bytes_sent()
        }
    )
  end

  {connect = connect, disconnect = disconnect, get = get}
end

# Default listener IP hash for `icecast.server`: an MD5 digest of the address with a fixed public key, truncated to 16 hex characters. It keeps addresses out of logs and pages but does not resist someone testing guessed addresses: pass your own `ip_hash` if that matters.
# @flag extra
# @category Source / Input / Active
def icecast.server.ip_hash(ip) =
  string.sub(string.digest("icecast.server:" ^ ip), start=0, length=16)
end

# Escape a log field like icecast's `%H` format. Bytes from 0x80 up are kept so UTF-8 text stays readable.
# @flag hidden
def icecast.server.log_escape(~keep_spaces=false, s) =
  def escape(c) =
    "\\x#{string.hex_of_int(pad=2, string.nth(c, 0))}"
  end
  if
    keep_spaces
  then
    r/[\x00-\x1f!"`\\]/g.replace(escape, s)
  else
    r/[\x00-\x20!"`\\]/g.replace(escape, s)
  end
end

# Icecast date format, in local time.
# @flag hidden
def icecast.server.log_date(date) =
  time.string(
    time=date,
    "%d/%b/%Y:%H:%M:%S %z"
  )
end

# Access log line for a finished listener session, in icecast's combined log format extended with the connected duration. Icecast stamps the line with the disconnect time.
# @flag hidden
def icecast.server.access_log_line(~now, listener) =
  esc = icecast.server.log_escape
  request =
    "GET #{esc(listener.uri)} HTTP/#{esc(listener.protocol)}"
  referer = esc(keep_spaces=true, listener.referer ?? "-")
  user_agent = esc(keep_spaces=true, listener.user_agent ?? "-")
  "#{esc(listener.ip)} - - [#{icecast.server.log_date(now)}] \"#{request}\" 200 #{
    listener.bytes_sent
  } \"#{referer}\" \"#{user_agent}\" #{int(listener.duration)}"
end

# Playlist log line for a metadata update, or `null` when it has no title. Icecast writes the text unescaped: `|` and line breaks are blanked here so each update stays one parseable line.
# @flag hidden
def icecast.server.playlist_log_line(~now, update) =
  artist = update.metadata["artist"]
  title = update.metadata["title"]
  text =
    if
      artist != "" and title != ""
    then
      "#{artist} - #{title}"
    else
      "#{artist}#{title}"
    end
  if
    text == ""
  then
    null
  else
    text =
      r/[|\r\n]/g.replace(
        fun (_) ->
          " ",
        text
      )
    "#{icecast.server.log_date(now)}|#{update.mount}|#{update.listeners}|#{
      text
    }"
  end
end

# Line writer for an icecast log file. `target` gives the current path (null disables the log, `-` is standard error), size limit in bytes and archive flag. The file is kept open and reopened when its path changes, and on `SIGHUP` (icecast's logrotate hook) or `SIGUSR1` (liquidsoap's). Past the size limit it is renamed to `<path>.old`, or to a timestamped name when `archive` is set.
# @flag hidden
def icecast.server.log_writer(target) =
  current = ref(null)

  def close_log() =
    if
      null.defined(current())
    then
      let {socket} = null.get(current())
      socket.close()
      current := null
    end
  end

  def open_log(path) =
    to_stderr = path == "-"
    file_path = to_stderr ? "/dev/stderr" : path
    current :=
      {
        path = path,
        to_stderr = to_stderr,
        file_path = file_path,
        socket = file.open(write=true, append=true, file_path),
        size = ref(if to_stderr then 0 else file.size(file_path) end)
      }
  end

  def rotate_log(~archive, entry) =
    close_log()
    suffix = archive ? time.string("%Y%m%d_%H%M%S") : "old"
    file.move(force=true, entry.file_path, "#{entry.file_path}.#{suffix}")
    open_log(entry.path)
  end

  def write(line) =
    let {path, size_limit, archive} = target()
    if
      not null.defined(path)
    then
      close_log()
    else
      path = null.get(path)
      if
        not null.defined(current()) or null.get(current()).path != path
      then
        close_log()
        open_log(path)
      end
      entry = null.get(current())
      if
        not entry.to_stderr and entry.size() > size_limit
      then
        rotate_log(archive=archive, entry)
      end
      entry = null.get(current())
      data = "#{line}\n"
      entry.socket.write(data)
      entry.size := entry.size() + string.length(data)
    end
  end

  on_signal("hup", fun () -> atomic(close_log))
  on_signal("usr1", fun () -> atomic(close_log))

  fun (line) ->
    # ponytail: one global atomic section for all logs, per-file locks if log volume ever matters.
    atomic(
      fun () ->
        try
          write(line)
        catch err do
          log.severe(
            label="icecast.server",
            "Could not write log line: #{err.message}"
          )
        end
    )
end

# Output of an `icecast.server` mount: its source with an optional fallback, served by `output.harbor`.
# @flag hidden
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
      on_listener_disconnect,
      on_metadata
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
      o.on_metadata(
        synchronous=true,
        fun (metadata) ->
          on_metadata(
            {metadata = metadata, listeners = list.length(o.listeners())}
          )
      )

      {
        clock = o.clock,
        shutdown = o.shutdown,
        last_metadata = o.last_metadata,
        listeners = o.listeners
      }
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
  ~log_event,
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
    log_event(
      level=2,
      "connection-handle/_handle_source_request",
      "Rejecting source connection to reserved admin mountpoint #{
        icecast.server.log_escape(uri)
      }"
    )
    false
  elsif
    is_taken(uri)
  then
    log_event(
      level=2,
      "connection-handle/_handle_source_request",
      "Mountpoint #{icecast.server.log_escape(uri)} in use"
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
      log_event(
        level=2,
        "connection-handle/_handle_source_request",
        "Rejecting source at #{icecast.server.log_escape(uri)}: too many sources \
         connected (#{null.get(max_sources)})"
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

# Public view of an `output.harbor` listener. Its address is resolved through X-Forwarded-For then `ip_hash`, and raw headers are left out since they can carry the unhashed address.
# @flag hidden
def icecast.server.listener_view(
  ~mount,
  ~x_forwarded_for,
  ~proxy_ips,
  ~ip_hash,
  listener
) =
  ip =
    icecast.server.listener_ip(
      x_forwarded_for=x_forwarded_for,
      proxy_ips=proxy_ips,
      {
        headers = listener.headers,
        ip = listener.ip,
        protocol = listener.protocol,
        uri = listener.uri
      }
    )
  {
    id = listener.id,
    mount = (mount : string),
    ip = ip_hash(ip),
    user_agent = list.assoc.nullable("user-agent", listener.headers),
    referer = list.assoc.nullable("referer", listener.headers),
    uri = listener.uri,
    protocol = listener.protocol,
    connected_at = listener.connected_at,
    duration = listener.duration,
    bytes_sent = listener.bytes_sent
  }
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
  ~ip_hash,
  ~global_headers,
  ~default_mount,
  ~mount_configs,
  ~log_event
) =
  # TODO: max_clients not yet implemented
  ignore(max_clients)

  server_clock = clock.create(id=name)
  mounts = icecast.server.mount_table()
  on_source_connect_handlers = ref([])
  on_source_disconnect_handlers = ref([])
  on_listener_connect_handlers = ref([])
  on_listener_disconnect_handlers = ref([])
  on_metadata_handlers = ref([])

  def listener_view(~mount, listener) =
    icecast.server.listener_view(
      mount=mount,
      x_forwarded_for=x_forwarded_for,
      proxy_ips=x_forwarded_for_proxy_ips,
      ip_hash=ip_hash,
      listener
    )
  end

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
        listeners = icecast.server.listener_counters()

        def on_listener_connect(listener) =
          listeners.connect()
          view = listener_view(mount=mount, listener)
          list.iter(fun (h) -> h(view), on_listener_connect_handlers())
        end

        def on_listener_disconnect(listener) =
          listeners.disconnect(listener)
          view = listener_view(mount=mount, listener)
          list.iter(fun (h) -> h(view), on_listener_disconnect_handlers())
        end

        def on_metadata({metadata, listeners}) =
          arg = {mount = mount, metadata = metadata, listeners = listeners}
          list.iter(fun (h) -> h(arg), on_metadata_handlers())
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
              on_listener_disconnect = on_listener_disconnect,
              on_metadata = on_metadata
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

        log_event(
          level=3,
          "connection-handle/_handle_source_request",
          "Source logging in at mountpoint \"#{
            icecast.server.log_escape(mount)
          }\""
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
        list.iter(fun (h) -> h(arg), on_source_connect_handlers())

        on_source_disconnect(
          synchronous=true,
          fun () ->
            begin
              log_event(
                level=3,
                "source/source_main",
                "Source at \"#{icecast.server.log_escape(mount)}\" exiting"
              )
              list.iter(
                fun ((orphan, e)) ->
                  begin
                    log_event(
                      level=4,
                      "source/source_shutdown",
                      "Shutting down output for mount #{
                        icecast.server.log_escape(orphan)
                      }"
                    )
                    e.data.output.shutdown()
                  end,
                mounts.disconnect(mount)
              )
              run_command(config.on_disconnect_cmd)
              list.iter(
                fun (h) -> h({mount = mount}),
                on_source_disconnect_handlers()
              )
            end
        )
      end
  end

  def auth(args) =
    icecast.server.authorize(
      log_event=log_event,
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

  def on_source_connect(~synchronous=false, handler) =
    add_handler(on_source_connect_handlers, synchronous=synchronous, handler)
  end

  def on_source_disconnect(~synchronous=false, handler) =
    add_handler(on_source_disconnect_handlers, synchronous=synchronous, handler)
  end

  def on_listener_connect(~synchronous=false, handler) =
    add_handler(on_listener_connect_handlers, synchronous=synchronous, handler)
  end

  def on_listener_disconnect(~synchronous=false, handler) =
    add_handler(
      on_listener_disconnect_handlers,
      synchronous=synchronous,
      handler
    )
  end

  def on_metadata(~synchronous=false, handler) =
    add_handler(on_metadata_handlers, synchronous=synchronous, handler)
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
        begin
          counters = entry.data.listeners.get()
          (
            mount,
            {
              name = entry.data.source_name(),
              content_type = entry.data.format() ?? "",
              mime_type = entry.data.mime_type(),
              started = entry.data.start_time(),
              listeners =
                list.map(
                  fun (l) -> listener_view(mount=mount, l),
                  entry.data.output.listeners()
                ),
              peak_listeners = counters.peak,
              connections = counters.connections,
              listening_time = counters.listening_time,
              bytes_sent = counters.bytes_sent,
              current_metadata = entry.data.output.last_metadata() ?? []
            }
          )
        end,
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
    on_source_connect = on_source_connect,
    on_source_disconnect = on_source_disconnect,
    on_listener_connect = on_listener_connect,
    on_listener_disconnect = on_listener_disconnect,
    on_metadata = on_metadata,
    stats = stats
  }
end

# Style shared by the status and admin pages.
# @flag hidden
let icecast.server.status_css =
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

# Handler answering 401 unless `auth`, when set, accepts the request.
# @flag hidden
def icecast.server.wrap_auth(auth, handler) =
  fun (req, response) ->
    begin
      accepted =
        if
          null.defined(auth)
        then
          auth_fn = null.get(auth)
          auth_fn(req)
        else
          true
        end
      if
        not accepted
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
    end
end

# `true` when the request carries HTTP Basic credentials matching `user` and `password`.
# @flag hidden
def icecast.server.basic_auth(~user, ~password, request) =
  credentials =
    string.residual(
      prefix="Basic ",
      list.assoc(default="", "authorization", request.headers)
    )
  try
    null.defined(credentials)
    and string.base64.decode(null.get(credentials)) == "#{user}:#{password}"
  catch _ do
    false
  end
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
     Status</title><style>#{
      icecast.server.status_css
    }</style></head><body><header><h1>Liquidsoap Status</h1><span \
     id='status'></span></header><main id='mounts'><p \
     class='empty'>Loading\u2026</p></main><script>#{js}</script></body></html>"

  def wrap_auth(handler) =
    icecast.server.wrap_auth(auth, handler)
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

# Admin listener page at `/admin/listeners`, with its data at `/admin/listeners.json`. Listener fields are client-controlled, so the page only ever inserts them as text.
# @flag hidden
def icecast.server.serve_admin(~port, ~transport, ~user, ~password, stats) =
  css =
    "#{
      icecast.server.status_css
    }table{width:100%;border-collapse:collapse;font-size:.85rem}th,td{text-align:left;padding:.3rem \
     .5rem;border-bottom:1px solid \
     #f0f0f0}th{font-size:.7rem;text-transform:uppercase;color:#888}td:nth-child(2){word-break:break-all}"

  js =
    "async function refresh(){try{const r=await \
     fetch('/admin/listeners.json');if(!r.ok)throw r.status;const d=await \
     r.json();render(d.mounts);document.getElementById('status').textContent='Updated \
     '+new \
     Date().toLocaleTimeString();}catch(e){document.getElementById('status').textContent='Connection \
     error.';}}function el(t,c,x){const \
     e=document.createElement(t);if(c)e.className=c;if(x!==undefined)e.textContent=String(x);return \
     e;}function dur(s){s=Math.floor(s);const \
     h=Math.floor(s/3600),m=Math.floor(s%3600/60);return (h?h+'h \
     ':'')+(h||m?m+'m ':'')+s%60+'s';}function stat(n,l){const \
     s=el('div','stat');s.append(el('div','stat-n',n),el('div','stat-l',l));return \
     s;}function render(mounts){const \
     root=document.getElementById('mounts');root.replaceChildren();if(!mounts.length){root.append(el('p','empty','No \
     active mounts.'));return;}mounts.forEach(m=>{const \
     card=el('div','card');card.append(el('div','card-title',m.mount));const \
     row=el('div','stats-row');row.append(stat(m.listeners.length,'Listeners'),stat(m.peak_listeners,'Peak'),stat(m.connections,'Connections'),stat(dur(m.listening_time),'Listened'));card.append(row);if(m.listeners.length){const \
     t=el('table');const h=el('tr');['Listener','User \
     agent','Connected','Sent'].forEach(x=>h.append(el('th','',x)));t.append(h);m.listeners.forEach(l=>{const \
     r=el('tr');r.append(el('td','',l.ip),el('td','',l.user_agent||'-'),el('td','',dur(l.duration)),el('td','',Math.round(l.bytes_sent/1024)+' \
     KiB'));t.append(r);});card.append(el('hr'),t);}root.append(card);});}refresh();setInterval(refresh,5000);"

  html_page =
    "<!DOCTYPE html><html lang='en'><head><meta charset='UTF-8'><meta \
     name='viewport' \
     content='width=device-width,initial-scale=1'><title>Liquidsoap \
     Listeners</title><style>#{css}</style></head><body><header><h1>Liquidsoap \
     Listeners</h1><span id='status'></span></header><main id='mounts'><p \
     class='empty'>Loading…</p></main><script>#{js}</script></body></html>"

  def auth(request) =
    icecast.server.basic_auth(user=user, password=password, request)
  end

  def json_handler(_, response) =
    mounts =
      list.map(
        fun ((mount, s)) ->
          {
            mount = mount,
            peak_listeners = s.peak_listeners,
            connections = s.connections,
            listening_time = s.listening_time,
            bytes_sent = s.bytes_sent,
            listeners = s.listeners
          },
        stats()
      )
    response.json({mounts = mounts})
  end

  harbor.http.register(
    port=port,
    transport=transport,
    method="GET",
    "/admin/listeners",
    icecast.server.wrap_auth(
      auth,
      fun (_, response) -> response.html(html_page)
    )
  )
  harbor.http.register(
    port=port,
    transport=transport,
    method="GET",
    "/admin/listeners.json",
    icecast.server.wrap_auth(auth, json_handler)
  )
end

# Start an icecast-compatible server
# @flag extra
# @category Source / Input / Active
# @param ~name Telnet command namespace
# @param ~port Port to listen on. Overrides `<port>` from the config file, defaults to `8000`.
# @param ~password Source password for authentication. Overrides `<source-password>` from the config file, defaults to `"hackme"`.
# @param ~dedicated_encoder When `true`, create a separate encoder instance for each listener. This uses more resources but ensures each listener gets a clean encoder state.
# @param ~config Optional path to an icecast XML configuration file
# @param ~serve When `false`, disable the status page at `/` and JSON stats at `/status.json`.
# @param ~serve_auth When provided, called with the request and must return `true` to allow access to the status endpoints.
# @param ~serve_json When provided, called with the stats list and must return a JSON string for the `/status.json` endpoint.
# @param ~serve_html When provided, called with the stats list and must return an HTML string for the `/` endpoint.
# @param ~x_forwarded_for_proxy_ips List of known reverse-proxy IPs. When set, the `X-Forwarded-For` header is walked right-to-left and the first non-proxy IP is used as the listener IP in stats. Use only when liquidsoap is behind a trusted reverse proxy.
# @param ~x_forwarded_for Advanced: custom callback `({ip, headers, proxy_ips, protocol, uri}) -> string` to extract the real listener IP. When set, overrides the default `X-Forwarded-For` logic. `proxy_ips` in the record is populated from `x_forwarded_for_proxy_ips`.
# @param ~format_options Callback `(string) -> [(string * string)]` that returns muxer options for a given container format name. When `null`, falls back to `settings.icecast.server.default_muxer_options`.
# @param ~access_log Path of an icecast-style access log, `-` for standard error. Overrides `<accesslog>` from the config file.
# @param ~playlist_log Path of an icecast-style playlist log, `-` for standard error. Overrides `<playlistlog>` from the config file.
# @param ~admin_user User name for the admin listener page. Overrides `<admin-user>` from the config file.
# @param ~admin_password Password for the admin listener page, which is disabled when no password is set. Overrides `<admin-password>` from the config file.
# @param ~ip_hash Function applied to every listener IP, after X-Forwarded-For resolution, before it reaches callbacks, stats or logs. Defaults to `icecast.server.ip_hash`, a truncated MD5 digest that is the same across restarts. Pass `fun (ip) -> ip` to keep plain IPs.
# @method on_source_connect Register a handler called when a source connects. Receives a record with mount, source, format, streams, and headers fields.
# @method on_source_disconnect Register a handler called when a source disconnects. Receives a record with a mount field.
# @method on_listener_connect Register a handler called when a listener connects. Receives a record with id, mount, ip (hashed), user_agent, referer, uri, protocol, connected_at, duration and bytes_sent fields.
# @method on_listener_disconnect Register a handler called when a listener disconnects, with the same record as `on_listener_connect` and final duration and bytes_sent. Synchronous handlers are called exactly once per listener, after its connect handlers.
# @method on_metadata Register a handler called on metadata updates of a mount. Receives a record with mount, metadata, and listeners (count) fields.
# @method stats Returns a list of `(mount, stats)` pairs. Each stats record contains: name, content_type, mime_type, started, listeners, peak_listeners, connections, listening_time, bytes_sent and current_metadata.
def replaces icecast.server(
  ~name="icecast.server",
  ~port=null,
  ~password=null,
  ~dedicated_encoder=false,
  ~x_forwarded_for_proxy_ips=null,
  ~x_forwarded_for=null,
  ~format_options=null,
  ~ip_hash=icecast.server.ip_hash,
  ~access_log=null,
  ~playlist_log=null,
  ~admin_user=null,
  ~admin_password=null,
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

  effective =
    icecast.server.config.resolve(
      port=port,
      password=password,
      access_log=access_log,
      playlist_log=playlist_log,
      admin_user=admin_user,
      admin_password=admin_password,
      from_file=null.defined(config),
      parsed
    )
  port = effective.port
  password = effective.password
  max_sources = parsed.max_sources
  logging = parsed.logging

  def log_writer(path) =
    icecast.server.log_writer(
      fun () ->
        {
          path = path,
          size_limit =
            null.map(fun (kib) -> 1024 * kib, logging.logsize) ?? 1000000000,
          archive = logging.logarchive
        }
    )
  end

  write_error_log = log_writer(effective.error_log)

  # Written to liquidsoap's log and, up to `<loglevel>`, to the error log in icecast's format: local time, level, `category/function` and message.
  def log_event(~level, category, message) =
    if
      level == 1
    then
      log.severe(label=name, message)
    elsif
      level == 4
    then
      log.info(label=name, message)
    else
      log.important(label=name, message)
    end
    if
      level <= (logging.loglevel ?? 3)
    then
      level_name =
        list.nth(default="DBUG", ["EROR", "WARN", "INFO", "DBUG"], level - 1)
      write_error_log(
        "#{
          time.string(
            "[%Y-%m-%d  %H:%M:%S]"
          )
        } #{level_name} #{category} #{message}"
      )
    end
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
      log_event(
        level=3,
        "main/main",
        "Using TLS transport: #{tls_name}"
      )
      tls_factory(null.get(parsed.tls_certificate), parsed.tls_key)
    else
      http.transport.unix
    end

  log_event(
    level=3,
    "main/main",
    "Icecast emulation server starting on port #{port}"
  )
  if
    null.defined(max_sources)
  then
    log_event(
      level=3,
      "main/main",
      "Maximum sources: #{null.get(max_sources)}"
    )
  end
  if
    parsed.mounts != []
  then
    log_event(
      level=3,
      "main/main",
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
      ip_hash=ip_hash,
      global_headers=parsed.http_headers,
      default_mount=parsed.default_mount,
      mount_configs=parsed.mounts,
      log_event=log_event
    )

  write_access_log = log_writer(effective.access_log)
  manager.on_listener_disconnect(
    synchronous=true,
    fun (listener) ->
      write_access_log(icecast.server.access_log_line(now=time(), listener))
  )

  write_playlist_log = log_writer(effective.playlist_log)
  manager.on_metadata(
    synchronous=true,
    fun (update) ->
      begin
        line = icecast.server.playlist_log_line(now=time(), update)
        if null.defined(line) then write_playlist_log(null.get(line)) end
      end
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

    admin_password = effective.admin_password
    if
      null.defined(admin_password)
    then
      icecast.server.serve_admin(
        port=port,
        transport=transport,
        user=effective.admin_user,
        password=null.get(admin_password),
        manager.stats
      )
    else
      log_event(
        level=3,
        "main/main",
        "Admin listener page disabled: no admin password configured"
      )
    end
  end

  {
    mounts = manager.mounts,
    get_source = manager.get_source,
    get_config = manager.get_config,
    on_source_connect = manager.on_source_connect,
    on_source_disconnect = manager.on_source_disconnect,
    on_listener_connect = manager.on_listener_connect,
    on_listener_disconnect = manager.on_listener_disconnect,
    on_metadata = manager.on_metadata,
    stats = manager.stats
  }
end
