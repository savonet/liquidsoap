# Icecast server implementation using input.harbor.dynamic
# This allows liquidsoap to act as an icecast-compatible server

let icecast = ()
let icecast.server = ()

let error.icecast = ()
let error.icecast.server = ()
let error.icecast.server.mount_taken =
  error.register("error.icecast.server.mount_taken")

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
    mount_name=icecast.server.get_child_string(children, "mount-name"),
    username=icecast.server.get_child_string(children, "username"),
    password=icecast.server.get_child_string(children, "password"),
    max_listeners=icecast.server.get_child_int(children, "max-listeners"),
    dump_file=icecast.server.get_child_string(children, "dump-file"),
    burst_size=icecast.server.get_child_int(children, "burst-size"),
    fallback_mount=icecast.server.get_child_string(children, "fallback-mount"),
    hidden=icecast.server.get_child_string(children, "hidden") == "1",
    on_connect_cmd=icecast.server.get_child_string(children, "on-connect"),
    on_disconnect_cmd=
      icecast.server.get_child_string(children, "on-disconnect"),
    http_headers=http_headers
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
      port=icecast.server.get_child_int(c, "port"),
      bind_address=icecast.server.get_child_string(c, "bind-address"),
      tls=icecast.server.get_child_string(c, "tls") == "1"
    }
  end

  def authentication() =
    c = get_children("authentication")
    {password=icecast.server.get_child_string(c, "source-password")}
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
      max_sources=icecast.server.get_child_int(c, "sources"),
      max_clients=icecast.server.get_child_int(c, "clients"),
      client_timeout=icecast.server.get_child_float(c, "client-timeout"),
      source_timeout=icecast.server.get_child_float(c, "source-timeout"),
      burst_on_connect=icecast.server.get_child_int(c, "burst-on-connect"),
      burst_size=icecast.server.get_child_int(c, "burst-size")
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
      settings.log.file.path := null.get(logdir)
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

    {tls_certificate=icecast.server.get_child_string(c, "tls-certificate")}
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
        default={xml_params=[], xml_children=[]},
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

    {default=default_config, configs=mount_configs}
  end

  {
    listen_socket=listen_socket,
    authentication=authentication,
    limits=limits,
    paths=paths,
    http_headers=http_headers,
    mounts=mounts
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
    port=listen_socket.port,
    bind_address=listen_socket.bind_address,
    tls=listen_socket.tls,
    tls_certificate=paths.tls_certificate,
    password=authentication.password,
    max_sources=limits.max_sources,
    max_clients=limits.max_clients,
    client_timeout=limits.client_timeout,
    source_timeout=limits.source_timeout,
    burst_on_connect=limits.burst_on_connect,
    burst_size=limits.burst_size,
    http_headers=[...http_headers, ...mounts.default.http_headers],
    default_mount=mounts.default,
    mounts=mounts.configs
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
        streams: ref(_)
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
        and
          null.get(entry.fallback_mount) == mount
        then
          (entry_mount, entry)
        else
          null
        end,
      entries()
    )
  end

  {
    get=get,
    has=has,
    set=set,
    remove=remove,
    all=all,
    find_by_mount=find_by_mount,
    find_by_fallback_mount=find_by_fallback_mount
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
  ~burst_size,
  ~global_headers,
  ~default_mount,
  ~mount_configs
) =
  # TODO: max_clients not yet implemented
  ignore(max_clients)
  empty_mount =
    {
      mount_name=null,
      username=null,
      password=null,
      max_listeners=null,
      dump_file=null,
      burst_size=null,
      fallback_mount=null,
      hidden=false,
      on_connect_cmd=null,
      on_disconnect_cmd=null,
      http_headers=[]
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
      mount_name=config.mount_name,
      username=config.username ?? default_mount.username,
      password=config.password ?? default_mount.password,
      max_listeners=config.max_listeners ?? default_mount.max_listeners,
      dump_file=config.dump_file ?? default_mount.dump_file,
      burst_size=config.burst_size ?? default_mount.burst_size,
      fallback_mount=config.fallback_mount ?? default_mount.fallback_mount,
      hidden=config.hidden or default_mount.hidden,
      on_connect_cmd=config.on_connect_cmd ?? default_mount.on_connect_cmd,
      on_disconnect_cmd=
        config.on_disconnect_cmd ?? default_mount.on_disconnect_cmd,
      http_headers=[...default_mount.http_headers, ...config.http_headers]
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
        and
          null.get(entry.fallback_mount) == mount
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
        and
          not null.defined(entry.fallback())
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

  def on_connection(r) =
    let {source = s, copy_encoder, query, uri, format, streams, headers} = r
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
    else
      source_ref = ref((s : source?))
      fallback_ref = ref(null)
      format_ref = ref(format)
      streams_ref = ref(streams)
      fallback_mount_config = mount_config.fallback_mount

      if
        null.defined(fallback_mount_config)
      and
        entries.has(null.get(fallback_mount_config))
      then
        fb_entry = entries.get(null.get(fallback_mount_config))
        fallback_ref := fb_entry.source()
      end

      main_dyn = source.dynamic(id="#{mount}.source", source_ref)

      output_source =
        if
          null.defined(fallback_mount_config)
        then
          fallback_dyn = source.dynamic(id="#{mount}.fallback", fallback_ref)

          fallback(
            id="#{mount}.selector",
            track_sensitive=false,
            [main_dyn, fallback_dyn]
          )
        else
          main_dyn
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

      o =
        output.harbor(
          id="#{mount}.output",
          fallible=true,
          mount=mount,
          port=port,
          transport=transport,
          headers=http_headers,
          timeout=client_timeout ?? 30.,
          burst=effective_burst,
          dumpfile=mount_config.dump_file,
          copy_encoder,
          output_source
        )

      server_clock.unify(o.clock)

      entries.set(
        mount,
        {
          output=o,
          source=source_ref,
          fallback_mount=fallback_mount_config,
          fallback=fallback_ref,
          format=format_ref,
          streams=streams_ref
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
      {mount=mount, source=s, format=format, streams=streams, headers=headers}
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

          arg = {mount=mount}
          list.iter(fun (h) -> h(arg), on_disconnect_handlers())
        end
    )
  end

  def auth(args) =
    let {user, password = provided_password, uri = mount, ..._} = args

    if
      entries.has(mount) and null.defined(entries.get(mount).source())
    then
      log.important(
        label=name,
        "Mountpoint #{mount} already in use, rejecting"
      )
      error.raise(
        error.icecast.server.mount_taken,
        "Mountpoint in use"
      )
    end

    mount_config = get_mount_config(mount)

    expected_user = mount_config.username ?? "source"
    expected_pass = mount_config.password ?? password

    if
      expected_pass != ""
    and
      not (user == expected_user and provided_password == expected_pass)
    then
      false
    elsif
      null.defined(max_sources)
    and
      list.length(entries.all()) >= null.get(max_sources)
    then
      log.important(
        label=name,
        "Maximum number of sources (#{null.get(max_sources)}) reached, rejecting \
         connection"
      )
      false
    else
      true
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
      {format=entry.format(), streams=entry.streams()}
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
    auth=auth,
    on_connection=on_connection,
    get_config=get_config,
    mounts=mounts,
    get_source=get_source,
    on_connect=on_connect,
    on_disconnect=on_disconnect
  }
end

# Start an icecast-compatible server
# @flag extra
# @category Source / Input
# @param ~name Telnet command namespace
# @param ~port Port to listen on
# @param ~password Source password for authentication
# @param ~config Optional path to an icecast XML configuration file
def icecast.server(
  ~name="icecast.server",
  ~port=8000,
  ~password="hackme",
  ~config=null
) =
  empty_mount =
    {
      mount_name=null,
      username=null,
      password=null,
      max_listeners=null,
      dump_file=null,
      burst_size=null,
      fallback_mount=null,
      hidden=false,
      on_connect_cmd=null,
      on_disconnect_cmd=null,
      http_headers=[]
    }

  parsed =
    if
      null.defined(config)
    then
      icecast.server.parse_config(null.get(config))
    else
      {
        port=null,
        bind_address=null,
        tls=false,
        tls_certificate=null,
        password=null,
        max_sources=null,
        max_clients=null,
        client_timeout=null,
        source_timeout=null,
        burst_on_connect=null,
        burst_size=null,
        http_headers=[],
        default_mount=empty_mount,
        mounts=[]
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
      http.transport.ssl(certificate=null.get(parsed.tls_certificate))
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
      port=port,
      password=password,
      max_sources=max_sources,
      global_headers=global_headers,
      default_mount=default_mount,
      mount_configs=mount_configs
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
      global_headers=global_headers,
      default_mount=default_mount,
      mount_configs=mount_configs
    )

  input.harbor.dynamic(
    "/:mount",
    port=port,
    transport=transport,
    timeout=source_timeout ?? 30.,
    auth=manager.auth,
    on_connection=manager.on_connection
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

  {
    mounts=manager.mounts,
    get_source=manager.get_source,
    get_config=manager.get_config,
    on_connect=manager.on_connect,
    on_disconnect=manager.on_disconnect
  }
end
