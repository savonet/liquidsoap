# Icecast server implementation using input.harbor.dynamic
# This allows liquidsoap to act as an icecast-compatible server

let icecast = ()
let icecast.server = ()

let error.icecast = ()
let error.icecast.server = ()
let error.icecast.server.mount_taken =
  error.register("error.icecast.server.mount_taken")

# @flag hidden
let settings.icecast =
  settings.make.void(
    "Icecast server emulation settings"
  )

# @flag hidden
let settings.icecast.server =
  settings.make.void(
    "Icecast server settings"
  )

# @flag hidden
def icecast.server.no_tls_transport(_certificate) =
  ignore(_certificate)
  log.critical(
    label="icecast.server",
    "TLS is enabled but no TLS transport is available. Please install TLS or SSL \
     support and rebuild liquidsoap."
  )
  shutdown()
  http.transport.unix
end

# Preferred TLS transport for `icecast.server`. A pair of `(name, factory)` where `factory` takes a certificate path and returns an HTTP transport.
# @flag extra
# @category Source / Input / Active
let settings.icecast.server.tls_transport =
  settings.make(
    description="Preferred TLS transport for icecast.server. A pair of (name, \
     factory) where factory takes a certificate path and returns an http \
     transport.",
    ("none", icecast.server.no_tls_transport)
  )

%ifdef http.transport.tls
# TLS transport for `icecast.server` using `http.transport.tls`.
# @flag extra
# @category Source / Input / Active
def icecast.server.tls_transport(certificate) =
  http.transport.tls(certificate=certificate)
end

settings.icecast.server.tls_transport := ("tls", icecast.server.tls_transport)
%endif

%ifdef http.transport.ssl
# SSL transport for `icecast.server` using `http.transport.ssl`. Overrides TLS transport when both are available.
# @flag extra
# @category Source / Input / Active
def icecast.server.ssl_transport(certificate) =
  http.transport.ssl(certificate=certificate)
end

settings.icecast.server.tls_transport := ("ssl", icecast.server.ssl_transport)
%endif

# @flag hidden
def icecast.server.parse_headers(node) =
  list.filter_map(
    fun (child) ->
      begin
        let (name, props) = child
        if
          name == "header"
        then
          header_name = list.assoc(default="", "name", props.xml_params)
          header_value = list.assoc(default="", "value", props.xml_params)
          if header_name != "" then (header_name, header_value) else null end
        else
          null
        end
      end,
    node.xml_children
  )
end

# @flag hidden
def icecast.server.get_child_string(children, name) =
  if
    list.assoc.mem(name, children)
  then
    let (_, node) = list.find(fun (n) -> fst(n) == name, children)
    node.xml_text
  else
    null
  end
end

# @flag hidden
def icecast.server.get_child_int(children, name) =
  s = icecast.server.get_child_string(children, name)
  if null.defined(s) then int_of_string(null.get(s)) else null end
end

# @flag hidden
def icecast.server.get_child_float(children, name) =
  s = icecast.server.get_child_string(children, name)
  if null.defined(s) then float_of_string(null.get(s)) else null end
end

# @flag hidden
def icecast.server.warn_unsupported(children) =
  list.iter(
    fun (opt) ->
      if
        list.assoc.mem(opt, children)
      then
        log.important(
          label="icecast.server",
          "Mount config option '#{opt}' is not currently supported"
        )
      end,
    ["public", "intro", "max-listener-duration", "authentication"]
  )

  list.iter(
    fun (opt) ->
      begin
        v = icecast.server.get_child_string(children, opt)
        if
          null.defined(v) and null.get(v) == "0"
        then
          log.critical(
            label="icecast.server",
            "Mount config '#{opt}=0' is not supported (internal logic differs)"
          )
        end
      end,
    ["fallback-override", "fallback-when-full"]
  )
end

# @flag hidden
def icecast.server.parse_mount_config(mount) =
  children = mount.xml_children
  icecast.server.warn_unsupported(children)

  http_headers =
    if
      list.assoc.mem("http-headers", children)
    then
      icecast.server.parse_headers(list.assoc("http-headers", children))
    else
      []
    end

  {
    mount_name = icecast.server.get_child_string(children, "mount-name"),
    username = icecast.server.get_child_string(children, "username"),
    password = icecast.server.get_child_string(children, "password"),
    max_listeners = icecast.server.get_child_int(children, "max-listeners"),
    dump_file = icecast.server.get_child_string(children, "dump-file"),
    burst_size = icecast.server.get_child_int(children, "burst-size"),
    fallback_mount =
      icecast.server.get_child_string(children, "fallback-mount"),
    hidden = icecast.server.get_child_string(children, "hidden") == "1",
    on_connect_cmd = icecast.server.get_child_string(children, "on-connect"),
    on_disconnect_cmd =
      icecast.server.get_child_string(children, "on-disconnect"),
    http_headers = http_headers
  }
end

# @flag hidden
def icecast.server.parser(path) =
  contents = file.contents(path)

  let xml.parse config = contents
  children = config.icecast.xml_children

  def get_child(name) =
    list.assoc.mem(name, children) ? list.assoc(name, children) : null
  end

  def get_children(name) =
    if
      null.defined(get_child(name))
    then
      null.get(get_child(name)).xml_children
    else
      []
    end
  end

  def listen_socket() =
    listen_sockets =
      list.filter_map(
        fun (child) ->
          begin
            let (name, props) = child
            if name == "listen-socket" then props else null end
          end,
        children
      )

    if
      list.length(listen_sockets) > 1
    then
      log.important(
        label="icecast.server",
        "Multiple listen-socket entries found, only the first one will be used"
      )
    end

    c =
      if
        list.length(listen_sockets) > 0
      then
        list.hd(listen_sockets).xml_children
      else
        []
      end

    if
      null.defined(icecast.server.get_child_string(c, "shoutcast-mount"))
    then
      log.important(
        label="icecast.server",
        "Config option 'shoutcast-mount' is not implemented"
      )
    end

    {
      port = icecast.server.get_child_int(c, "port"),
      bind_address = icecast.server.get_child_string(c, "bind-address"),
      tls = icecast.server.get_child_string(c, "tls") == "1"
    }
  end

  def authentication() =
    c = get_children("authentication")
    {password = icecast.server.get_child_string(c, "source-password")}
  end

  def limits() =
    c = get_children("limits")

    if
      null.defined(icecast.server.get_child_string(c, "queue-size"))
    then
      log.important(
        label="icecast.server",
        "Config option 'queue-size' is not used"
      )
    end
    if
      null.defined(icecast.server.get_child_string(c, "header-timeout"))
    then
      log.important(
        label="icecast.server",
        "Config option 'header-timeout' is not used"
      )
    end

    {
      max_sources = icecast.server.get_child_int(c, "sources"),
      max_clients = icecast.server.get_child_int(c, "clients"),
      client_timeout = icecast.server.get_child_float(c, "client-timeout"),
      source_timeout = icecast.server.get_child_float(c, "source-timeout"),
      burst_on_connect = icecast.server.get_child_int(c, "burst-on-connect"),
      burst_size = icecast.server.get_child_int(c, "burst-size")
    }
  end

  def paths() =
    c = get_children("paths")

    basedir = icecast.server.get_child_string(c, "basedir")
    if
      null.defined(basedir)
    then
      log.important(
        label="icecast.server",
        "Config option 'basedir' is not implemented"
      )
    end

    logdir = icecast.server.get_child_string(c, "logdir")
    if
      null.defined(logdir)
    then
      settings.log.file := true
      settings.log.file.path := "#{null.get(logdir)}/<script>.log"

      # Keep log to stdout by default
      settings.log.stdout := true
    end

    pidfile = icecast.server.get_child_string(c, "pidfile")
    if
      null.defined(pidfile)
    then
      settings.init.daemon.pidfile := true
      settings.init.daemon.pidfile.path := null.get(pidfile)
    end

    if
      null.defined(icecast.server.get_child_string(c, "webroot"))
    then
      log.important(
        label="icecast.server",
        "Config option 'webroot' is not implemented"
      )
    end
    if
      null.defined(icecast.server.get_child_string(c, "adminroot"))
    then
      log.important(
        label="icecast.server",
        "Config option 'adminroot' is not implemented"
      )
    end
    if
      null.defined(icecast.server.get_child_string(c, "allow-ip"))
    then
      log.important(
        label="icecast.server",
        "Config option 'allow-ip' is not implemented"
      )
    end
    if
      null.defined(icecast.server.get_child_string(c, "deny-ip"))
    then
      log.important(
        label="icecast.server",
        "Config option 'deny-ip' is not implemented"
      )
    end
    if
      null.defined(icecast.server.get_child_string(c, "ssl-allowed-ciphers"))
    then
      log.important(
        label="icecast.server",
        "Config option 'ssl-allowed-ciphers' is not implemented"
      )
    end
    if
      list.exists(fun (child) -> fst(child) == "alias", c)
    then
      log.important(
        label="icecast.server",
        "Config option 'alias' is not implemented"
      )
    end

    {tls_certificate = icecast.server.get_child_string(c, "tls-certificate")}
  end

  def http_headers() =
    h = get_child("http-headers")
    if
      null.defined(h)
    then
      icecast.server.parse_headers(null.get(h))
    else
      []
    end
  end

  def mounts() =
    raw_mounts =
      list.filter_map(
        fun (child) ->
          begin
            let (name, props) = child
            if name == "mount" then props else null end
          end,
        children
      )

    default_mount =
      list.find(
        default={xml_params = [], xml_children = []},
        fun (m) -> list.assoc(default="", "type", m.xml_params) == "default",
        raw_mounts
      )

    default_config = icecast.server.parse_mount_config(default_mount)

    mount_configs =
      list.filter_map(
        fun (m) ->
          begin
            mount_type = list.assoc(default="normal", "type", m.xml_params)
            if
              mount_type == "normal"
            then
              icecast.server.parse_mount_config(m)
            else
              null
            end
          end,
        raw_mounts
      )

    {default = default_config, configs = mount_configs}
  end

  {
    listen_socket = listen_socket,
    authentication = authentication,
    limits = limits,
    paths = paths,
    http_headers = http_headers,
    mounts = mounts
  }
end

# @flag hidden
def icecast.server.parse_config(path) =
  parser = icecast.server.parser(path)

  listen_socket = parser.listen_socket()
  authentication = parser.authentication()
  limits = parser.limits()
  paths = parser.paths()
  http_headers = parser.http_headers()
  mounts = parser.mounts()

  {
    port = listen_socket.port,
    bind_address = listen_socket.bind_address,
    tls = listen_socket.tls,
    tls_certificate = paths.tls_certificate,
    password = authentication.password,
    max_sources = limits.max_sources,
    max_clients = limits.max_clients,
    client_timeout = limits.client_timeout,
    source_timeout = limits.source_timeout,
    burst_on_connect = limits.burst_on_connect,
    burst_size = limits.burst_size,
    http_headers = [...http_headers, ...mounts.default.http_headers],
    default_mount = mounts.default,
    mounts = mounts.configs
  }
end

# @flag hidden
def icecast.server.log_mount_config(label, prefix, config) =
  mount_name = config.mount_name ?? "(default)"
  username = config.username ?? "source"
  password_info =
    if
      null.defined(config.password)
    then
      "#{string.length(null.get(config.password))} characters"
    else
      "(global)"
    end
  max_listeners =
    if
      null.defined(config.max_listeners)
    then
      string(null.get(config.max_listeners))
    else
      "unlimited"
    end
  burst_size =
    if
      null.defined(config.burst_size)
    then
      string(null.get(config.burst_size))
    else
      "default"
    end
  fallback_mount = config.fallback_mount ?? "none"
  on_connect = config.on_connect_cmd ?? "none"
  on_disconnect = config.on_disconnect_cmd ?? "none"

  log.info(
    label=label,
    "#{prefix}mount-name: #{mount_name}"
  )
  log.info(
    label=label,
    "#{prefix}username: #{username}"
  )
  log.info(
    label=label,
    "#{prefix}password: #{password_info}"
  )
  log.info(
    label=label,
    "#{prefix}max-listeners: #{max_listeners}"
  )
  log.info(
    label=label,
    "#{prefix}burst-size: #{burst_size}"
  )
  log.info(
    label=label,
    "#{prefix}fallback-mount: #{fallback_mount}"
  )
  log.info(
    label=label,
    "#{prefix}hidden: #{config.hidden}"
  )
  log.info(
    label=label,
    "#{prefix}on-connect: #{on_connect}"
  )
  log.info(
    label=label,
    "#{prefix}on-disconnect: #{on_disconnect}"
  )
  log.info(
    label=label,
    "#{prefix}http-headers: #{list.length(config.http_headers)} header(s)"
  )
  list.iter(
    fun (h) ->
      log.info(
        label=label,
        "#{prefix}  #{fst(h)}: #{snd(h)}"
      ),
    config.http_headers
  )
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
    fun (h) ->
      log.info(
        label=label,
        "  #{fst(h)}: #{snd(h)}"
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
    list.length(mount_configs) > 0
  then
    log.info(
      label=label,
      "Mount configurations:"
    )
    list.iter(
      fun (config) ->
        begin
          mount_name = config.mount_name ?? "?"
          log.info(
            label=label,
            "- Mount: #{mount_name}"
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

# @flag hidden
def icecast.server.mount_entries() =
  entries = ref([])

  def get(mount) =
    (list.assoc(mount, entries()) :
      {
        output: _,
        source: ref(source?),
        fallback_mount: string?,
        fallback: ref(source?),
        format: ref(_),
        streams: ref(_),
        source_name: ref(string),
        mime_type: ref(string?),
        start_time: ref(float),
        listeners: ref([{ip: string, protocol: string, uri: string}]),
        peak_listeners: ref(int)
      }
    )
  end

  def has(mount) =
    list.assoc.mem(mount, entries())
  end

  def set(mount, entry) =
    entries :=
      [(mount, entry), ...list.filter(fun (m) -> fst(m) != mount, entries())]
  end

  def remove(mount) =
    entries := list.filter(fun (m) -> fst(m) != mount, entries())
  end

  def all() =
    entries()
  end

  def find_by_mount(mount) =
    if list.assoc.mem(mount, entries()) then [(mount, get(mount))] else [] end
  end

  def find_by_fallback_mount(mount) =
    list.filter_map(
      fun ((entry_mount, entry)) ->
        if
          null.defined(entry.fallback_mount)
          and null.get(entry.fallback_mount) == mount
        then
          (entry_mount, entry)
        else
          null
        end,
      entries()
    )
  end

  {
    get = get,
    has = has,
    set = set,
    remove = remove,
    all = all,
    find_by_mount = find_by_mount,
    find_by_fallback_mount = find_by_fallback_mount
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
    streams = _,
    source_headers = _
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
              track_sensitive=false,
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
          copy_encoder(format),
          default_source
        )

      o.on_connect(synchronous=true, on_listener_connect)
      o.on_disconnect(synchronous=true, on_listener_disconnect)

      {clock = o.clock, shutdown = o.shutdown}
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
  ~burst_size,
  ~dedicated_encoder=false,
  ~global_headers,
  ~default_mount,
  ~prepare,
  ~mount_configs
) =
  # TODO: max_clients not yet implemented
  ignore(max_clients)
  empty_mount =
    {
      mount_name = null,
      username = null,
      password = null,
      max_listeners = null,
      dump_file = null,
      burst_size = null,
      fallback_mount = null,
      hidden = false,
      on_connect_cmd = null,
      on_disconnect_cmd = null,
      http_headers = []
    }

  server_clock = clock.create(id=name)

  def get_mount_config(mount_path) =
    config =
      list.find(
        default=empty_mount,
        fun (m) ->
          null.defined(m.mount_name) and null.get(m.mount_name) == mount_path,
        mount_configs
      )

    {
      mount_name = config.mount_name,
      username = config.username ?? default_mount.username,
      password = config.password ?? default_mount.password,
      max_listeners = config.max_listeners ?? default_mount.max_listeners,
      dump_file = config.dump_file ?? default_mount.dump_file,
      burst_size = config.burst_size ?? default_mount.burst_size,
      fallback_mount = config.fallback_mount ?? default_mount.fallback_mount,
      hidden = config.hidden or default_mount.hidden,
      on_connect_cmd = config.on_connect_cmd ?? default_mount.on_connect_cmd,
      on_disconnect_cmd =
        config.on_disconnect_cmd ?? default_mount.on_disconnect_cmd,
      http_headers = [...default_mount.http_headers, ...config.http_headers]
    }
  end

  entries = icecast.server.mount_entries()
  on_connect_handlers = ref([])
  on_disconnect_handlers = ref([])

  def update_fallbacks(mount, source) =
    list.iter(
      fun ((_, entry)) ->
        if
          null.defined(entry.fallback_mount)
          and null.get(entry.fallback_mount) == mount
        then
          entry.fallback := source
        end,
      entries.all()
    )
  end

  def on_shutdown(find_and_clear) =
    list.iter(
      fun ((mount, entry)) ->
        if
          not null.defined(entry.source())
          and not null.defined(entry.fallback())
        then
          log.info(
            label=name,
            "Shutting down output for mount #{mount}"
          )
          entry.output.shutdown()
          entries.remove(mount)
        end,
      find_and_clear()
    )
  end

  def handle_connection(c) =
    let {copy_encoder, query, uri, format, mime_type, streams, headers} = c
    fun (s) ->
      begin
        on_source_disconnect = s.on_disconnect

        mount = query["mount"]
        mount = if r/^\//.test(mount) then mount else "/#{mount}" end
        mount_config = get_mount_config(mount)

        log.important(
          label=name,
          "New source connected on mount #{mount}"
        )

        if
          entries.has(mount)
        then
          entry = entries.get(mount)
          log.info(
            label=name,
            "Reusing existing output for mount #{mount}"
          )
          entry.source := s
          entry.format := format
          entry.streams := streams
          entry.source_name := list.assoc(default="", "icy-name", headers)
          entry.mime_type := mime_type
          entry.start_time := time()
        else
          source_ref = ref((s : source?))
          fallback_ref = ref(null)
          format_ref = ref(format)
          streams_ref = ref(streams)
          source_name_ref = ref(list.assoc(default="", "icy-name", headers))
          mime_type_ref = ref(mime_type)
          start_time_ref = ref(time())
          listeners_ref = ref([])
          peak_listeners_ref = ref(0)

          fallback_mount_config = mount_config.fallback_mount

          if
            null.defined(fallback_mount_config)
            and entries.has(null.get(fallback_mount_config))
          then
            fb_entry = entries.get(null.get(fallback_mount_config))
            fallback_ref := fb_entry.source()
          end

          mount_id = r/^\//.replace(fun (_) -> "", mount)
          main_dyn = source.dynamic(id="#{mount_id}.source", source_ref)
          fallback_dyn =
            if
              null.defined(fallback_mount_config)
            then
              (source.dynamic(id="#{mount_id}.fallback", fallback_ref) :
                source?
              )
            else
              null
            end

          http_headers = [...global_headers, ...mount_config.http_headers]

          effective_burst =
            if
              null.defined(mount_config.burst_size)
            then
              let b = null.get(mount_config.burst_size)
              if b == 0 then null else b end
            else
              burst_size
            end

          def on_listener_connect(info) =
            listener = {ip = info.ip, protocol = info.protocol, uri = info.uri}
            listeners_ref := [listener, ...listeners_ref()]
            if
              list.length(listeners_ref()) > peak_listeners_ref()
            then
              peak_listeners_ref := list.length(listeners_ref())
            end
          end

          def on_listener_disconnect(ip) =
            listeners_ref := list.filter(fun (l) -> l.ip != ip, listeners_ref())
          end

          prepare = prepare ?? icecast.server.default_prepare
          connector =
            prepare(
              {
                copy_encoder = copy_encoder,
                format = format,
                streams = streams,
                source_headers = headers,
                mount = mount,
                headers = http_headers,
                burst = effective_burst,
                timeout = client_timeout ?? 30.,
                port = port,
                transport = transport,
                dumpfile = mount_config.dump_file,
                dedicated_encoder = dedicated_encoder
              }
            )
          o =
            connector(
              {
                source = main_dyn,
                fallback = fallback_dyn,
                mime_type = mime_type,
                on_listener_connect = on_listener_connect,
                on_listener_disconnect = on_listener_disconnect
              }
            )

          server_clock.unify(o.clock)

          entries.set(
            mount,
            {
              output = o,
              source = source_ref,
              fallback_mount = fallback_mount_config,
              fallback = fallback_ref,
              format = format_ref,
              streams = streams_ref,
              source_name = source_name_ref,
              mime_type = mime_type_ref,
              start_time = start_time_ref,
              listeners = listeners_ref,
              peak_listeners = peak_listeners_ref
            }
          )
        end

        update_fallbacks(mount, s)

        on_connect_cmd = mount_config.on_connect_cmd
        if
          null.defined(on_connect_cmd)
        then
          cmd = null.get(on_connect_cmd)
          thread.run(fun () -> ignore(process.run(cmd)))
        end

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

              on_shutdown(
                fun () ->
                  list.map(
                    fun ((m, e)) ->
                      begin
                        e.source := null
                        (m, e)
                      end,
                    entries.find_by_mount(mount)
                  )
              )
              on_shutdown(
                fun () ->
                  list.map(
                    fun ((m, e)) ->
                      begin
                        e.fallback := null
                        (m, e)
                      end,
                    entries.find_by_fallback_mount(mount)
                  )
              )

              on_disconnect_cmd = mount_config.on_disconnect_cmd
              if
                null.defined(on_disconnect_cmd)
              then
                cmd = null.get(on_disconnect_cmd)
                thread.run(fun () -> ignore(process.run(cmd)))
              end

              arg = {mount = mount}
              list.iter(fun (h) -> h(arg), on_disconnect_handlers())
            end
        )
      end
  end

  def auth(args) =
    let {user, password = provided_password, uri, method, ..._} = args
    is_source_connection =
      method == "PUT"
      or method == "POST"
      or method == "SOURCE"
      or method == "ICY"

    if
      is_source_connection and r/^\/admin(\/|\.cgi|$)/.test(uri)
    then
      log.important(
        label=name,
        "Rejecting source connection to reserved admin mountpoint #{uri}"
      )
      false
    elsif
      is_source_connection
      and entries.has(uri)
      and null.defined(entries.get(uri).source())
    then
      log.important(
        label=name,
        "Mountpoint #{uri} already in use, rejecting"
      )
      error.raise(
        error.icecast.server.mount_taken,
        "Mountpoint in use"
      )
    else
      mount_config = get_mount_config(uri)

      expected_user = mount_config.username ?? "source"
      expected_pass = mount_config.password ?? password

      if
        is_source_connection
        and expected_pass != ""
        and not (user == expected_user and provided_password == expected_pass)
      then
        false
      elsif
        is_source_connection
        and null.defined(max_sources)
        and list.length(entries.all()) >= null.get(max_sources)
      then
        log.important(
          label=name,
          "Maximum number of sources (#{null.get(max_sources)}) reached, \
           rejecting connection"
        )
        false
      else
        true
      end
    end
  end

  def on_connect(~synchronous=false, handler) =
    wrapped =
      if
        synchronous
      then
        handler
      else
        fun (arg) -> thread.run(fun () -> handler(arg))
      end
    on_connect_handlers := [...on_connect_handlers(), wrapped]
  end

  def on_disconnect(~synchronous=false, handler) =
    wrapped =
      if
        synchronous
      then
        handler
      else
        fun (arg) -> thread.run(fun () -> handler(arg))
      end
    on_disconnect_handlers := [...on_disconnect_handlers(), wrapped]
  end

  def get_config(mount) =
    if
      entries.has(mount)
    then
      entry = entries.get(mount)
      {format = entry.format(), streams = entry.streams()}
    else
      null
    end
  end

  def mounts() =
    list.map(fst, entries.all())
  end

  def get_source(mount) =
    if entries.has(mount) then entries.get(mount).source() else null end
  end

  def stats() =
    list.map(
      fun ((mount, entry)) ->
        (
          mount,
          {
            name = entry.source_name(),
            content_type = entry.format() ?? "",
            mime_type = entry.mime_type(),
            started = entry.start_time(),
            listeners = entry.listeners(),
            peak_listeners = entry.peak_listeners(),
            current_metadata =
              if
                null.defined(entry.source())
              then
                source.methods(null.get(entry.source())).last_metadata() ?? []
              else
                []
              end
          }
        ),
      entries.all()
    )
  end

  server.register(
    namespace=name,
    description="List active mounts on the icecast server",
    "mounts",
    fun (_) ->
      begin
        all_entries = entries.all()
        if
          list.length(all_entries) == 0
        then
          "No active mounts"
        else
          lines =
            list.map(
              fun ((mount, entry)) ->
                begin
                  source_status =
                    if
                      null.defined(entry.source())
                    then
                      "connected"
                    else
                      "disconnected"
                    end
                  fallback_status =
                    if
                      null.defined(entry.fallback())
                    then
                      "connected"
                    else
                      "disconnected"
                    end
                  "#{mount}: source=#{source_status}, fallback=#{
                    fallback_status
                  }, format=#{entry.format()}, streams=#{entry.streams()}"
                end,
              all_entries
            )
          string.concat(separator="\n", lines)
        end
      end
  )

  {
    auth = auth,
    handle_connection = handle_connection,
    get_config = get_config,
    mounts = mounts,
    get_source = get_source,
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
     class=card-title>${m.mount}</div><div class=now-playing></div><div \
     class=meta><div class=meta-item><span class=meta-label>Name</span><span \
     class=dyn-name></span></div><div class=meta-item><span \
     class=meta-label>Format</span><span class=dyn-format></span></div><div \
     class=meta-item><span class=meta-label>Started</span><span \
     class=dyn-started></span></div></div><hr><div class=stats-row><div \
     class=stat><div class=stat-n dyn-listeners></div><div \
     class=stat-l>Listeners</div></div><div class=stat><div class=stat-n \
     dyn-peak></div><div \
     class=stat-l>Peak</div></div></div>${m.mime_type&&m.mime_type.startsWith('video/')?`<video \
     controls src='${m.mount}' type='${m.mime_type}' \
     style='width:100%;margin-top:1rem'></video>`:`<audio controls \
     src='${m.mount}'${m.mime_type?` type='${m.mime_type}'`:''} \
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
# @param ~prepare When provided, called with `{source, fallback, config, headers, burst, timeout, port, transport, dumpfile, dedicated_encoder}` and must return the output (typically by calling `output.harbor`). When `null`, a default output is created using `copy_encoder` and a standard fallback.
# @param ~dedicated_encoder When `true`, create a separate encoder instance for each listener. This uses more resources but ensures each listener gets a clean encoder state.
# @param ~config Optional path to an icecast XML configuration file
# @param ~serve When `false`, disable the status page at `/` and JSON stats at `/status.json`.
# @param ~serve_auth When provided, called with the request and must return `true` to allow access to the status endpoints.
# @param ~serve_json When provided, called with the stats list and must return a JSON string for the `/status.json` endpoint.
# @param ~serve_html When provided, called with the stats list and must return an HTML string for the `/` endpoint.
# @callback on_connect Called when a source connects. Receives a record with mount, source, format, streams, and headers fields.
# @callback on_disconnect Called when a source disconnects. Receives a record with a mount field.
# @method stats Returns a list of `(mount, stats)` pairs. Each stats record contains: name, content_type, started, listeners, peak_listeners, and current_metadata.
def icecast.server(
  ~name="icecast.server",
  ~port=8000,
  ~password="hackme",
  ~dedicated_encoder=false,
  ~prepare=null,
  ~config=null,
  ~serve=true,
  ~serve_auth=null,
  ~serve_json=null,
  ~serve_html=null
) =
  empty_mount =
    {
      mount_name = null,
      username = null,
      password = null,
      max_listeners = null,
      dump_file = null,
      burst_size = null,
      fallback_mount = null,
      hidden = false,
      on_connect_cmd = null,
      on_disconnect_cmd = null,
      http_headers = []
    }

  parsed =
    if
      null.defined(config)
    then
      icecast.server.parse_config(null.get(config))
    else
      {
        port = null,
        bind_address = null,
        tls = false,
        tls_certificate = null,
        password = null,
        max_sources = null,
        max_clients = null,
        client_timeout = null,
        source_timeout = null,
        burst_on_connect = null,
        burst_size = null,
        http_headers = [],
        default_mount = empty_mount,
        mounts = []
      }
    end

  max_sources = parsed.max_sources
  max_clients = parsed.max_clients
  client_timeout = parsed.client_timeout
  source_timeout = parsed.source_timeout
  burst_on_connect = parsed.burst_on_connect
  burst_size =
    if
      null.defined(burst_on_connect) and null.get(burst_on_connect) == 0
    then
      null
    else
      parsed.burst_size
    end
  port = if null.defined(parsed.port) then null.get(parsed.port) else port end
  password =
    if
      null.defined(parsed.password)
    then
      null.get(parsed.password)
    else
      password
    end
  global_headers = parsed.http_headers
  default_mount = parsed.default_mount
  mount_configs = parsed.mounts

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
      tls_factory(null.get(parsed.tls_certificate))
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
    list.length(mount_configs) > 0
  then
    log.important(
      label=name,
      "Loaded #{list.length(mount_configs)} mount configuration(s)"
    )
  end

  icecast.server.log_config(
    name,
    {
      port = port,
      password = password,
      max_sources = max_sources,
      global_headers = global_headers,
      default_mount = default_mount,
      mount_configs = mount_configs
    }
  )

  manager =
    icecast.server.source_manager(
      name=name,
      port=port,
      transport=transport,
      password=password,
      max_sources=max_sources,
      max_clients=max_clients,
      client_timeout=client_timeout,
      burst_size=burst_size,
      dedicated_encoder=dedicated_encoder,
      global_headers=global_headers,
      default_mount=default_mount,
      prepare=prepare,
      mount_configs=mount_configs
    )

  input.harbor.dynamic(
    "/:mount",
    icy=true,
    port=port,
    transport=transport,
    timeout=source_timeout ?? 30.,
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
            "Global HTTP headers: #{list.length(global_headers)}",
            "Mount configurations: #{list.length(mount_configs)}"
          ]

        mount_lines =
          list.map(
            fun (config) ->
              begin
                mount_name = config.mount_name ?? "(default)"
                "  - #{mount_name}"
              end,
            mount_configs
          )

        string.concat(separator="\n", [...lines, ...mount_lines])
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
