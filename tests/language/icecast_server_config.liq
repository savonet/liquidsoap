%include "../test.liq"

def f() =
  # <http-headers>: CORS defaults, explicit values, unknown CORS, plain headers
  headers =
    icecast.server.config.headers(
      '<http-headers>
         <header name="Access-Control-Allow-Origin" type="cors" />
         <header name="Access-Control-Max-Age" type="cors" value="60" />
         <header name="X-Unknown" type="cors" />
         <header name="X-Robots-Tag" value="noindex" />
         <header type="cors" />
         <not-a-header name="X-Ignored" />
       </http-headers>'
    )
  test.equal(
    headers,
    [
      ("Access-Control-Allow-Origin", "*"),
      ("Access-Control-Max-Age", "60"),
      ("X-Robots-Tag", "noindex")
    ]
  )
  test.equal(icecast.server.config.headers("<http-headers/>"), [])

  # <mount>: every field, plus nested headers
  m =
    icecast.server.config.mount.parse(
      '<mount type="normal">
         <mount-name>/live</mount-name>
         <username>dj</username>
         <password>secret</password>
         <max-listeners>10</max-listeners>
         <dump-file>/tmp/dump</dump-file>
         <burst-size>65536</burst-size>
         <fallback-mount>/backup</fallback-mount>
         <hidden>1</hidden>
         <on-connect>/bin/true</on-connect>
         <on-disconnect>/bin/false</on-disconnect>
         <http-headers><header name="X-Mount" value="live" /></http-headers>
       </mount>'
    )
  test.equal(m.type, "normal")
  test.equal(m.mount_name, "/live")
  test.equal(m.username, "dj")
  test.equal(m.password, "secret")
  test.equal(m.max_listeners, 10)
  test.equal(m.dump_file, "/tmp/dump")
  test.equal(m.burst_size, 65536)
  test.equal(m.fallback_mount, "/backup")
  test.equal(m.hidden, true)
  test.equal(m.on_connect_cmd, "/bin/true")
  test.equal(m.on_disconnect_cmd, "/bin/false")
  test.equal(m.http_headers, [("X-Mount", "live")])

  # <mount>: element children under unsupported options must not break parsing
  m =
    icecast.server.config.mount.parse(
      '<mount><mount-name>/x</mount-name><authentication type="htpasswd"><option \
       name="filename" value="f"/></authentication></mount>'
    )
  test.equal(m.mount_name, "/x")

  # <mount>: defaults when nothing is set
  m =
    icecast.server.config.mount.parse(
      '<mount type="default"/>'
    )
  test.equal(m.type, "default")
  test.equal(m.mount_name, null)
  test.equal(m.hidden, false)
  test.equal(m.http_headers, [])
  test.equal(icecast.server.config.mount.parse("<mount/>").type, "normal")

  # mount.resolve: own value wins, default fills gaps, headers accumulate
  default =
    icecast.server.config.mount.parse(
      '<mount type="default">
         <password>global</password>
         <fallback-mount>/backup</fallback-mount>
         <burst-size>1</burst-size>
         <hidden>1</hidden>
         <http-headers><header name="X-Default" value="1" /></http-headers>
       </mount>'
    )
  live =
    icecast.server.config.mount.parse(
      '<mount>
         <mount-name>/live</mount-name>
         <password>own</password>
         <burst-size>2</burst-size>
         <http-headers><header name="X-Live" value="1" /></http-headers>
       </mount>'
    )
  r =
    icecast.server.config.mount.resolve(default=default, mounts=[live], "/live")
  test.equal(r.mount_name, "/live")
  test.equal(r.password, "own")
  test.equal(r.burst_size, 2)
  test.equal(r.fallback_mount, "/backup")
  test.equal(r.hidden, true)
  test.equal(r.http_headers, [("X-Default", "1"), ("X-Live", "1")])
  r =
    icecast.server.config.mount.resolve(
      default=default,
      mounts=[live],
      "/other"
    )
  test.equal(r.mount_name, null)
  test.equal(r.password, "global")
  test.equal(r.burst_size, 1)
  test.equal(r.http_headers, [("X-Default", "1")])

  # <listen-socket>
  s =
    icecast.server.config.listen_socket(
      "<listen-socket><port>8100</port><bind-address>127.0.0.1</bind-address><tls>auto_no_plain</tls></listen-socket>"
    )
  test.equal(s.port, 8100)
  test.equal(s.bind_address, "127.0.0.1")
  test.equal(s.tls, true)
  s =
    icecast.server.config.listen_socket(
      "<listen-socket><ssl>1</ssl></listen-socket>"
    )
  test.equal(s.port, null)
  test.equal(s.tls, true)
  s =
    icecast.server.config.listen_socket(
      "<listen-socket><tls>auto</tls></listen-socket>"
    )
  test.equal(s.tls, false)
  test.equal(icecast.server.config.listen_socket("<listen-socket/>").tls, false)

  # <limits>: burst-on-connect=0 disables bursting
  l =
    icecast.server.config.limits(
      "<limits><sources>2</sources><clients>100</clients><client-timeout>30</client-timeout><source-timeout>10.5</source-timeout><burst-size>65535</burst-size></limits>"
    )
  test.equal(l.max_sources, 2)
  test.equal(l.max_clients, 100)
  test.equal(l.client_timeout, 30.)
  test.equal(l.source_timeout, 10.5)
  test.equal(l.burst_size, 65535)
  l =
    icecast.server.config.limits(
      "<limits><burst-on-connect>0</burst-on-connect><burst-size>65535</burst-size></limits>"
    )
  test.equal(l.burst_on_connect, 0)
  test.equal(l.burst_size, 65535)

  # burst_size: mount over global, 0 disables at either level
  def burst(limits, mount) =
    icecast.server.config.burst_size(
      limits=icecast.server.config.limits(limits),
      icecast.server.config.mount.parse(mount).burst_size
    )
  end
  test.equal(burst("<limits/>", "<mount/>"), null)
  test.equal(
    burst("<limits><burst-size>10</burst-size></limits>", "<mount/>"),
    10
  )
  test.equal(
    burst(
      "<limits><burst-size>10</burst-size></limits>",
      "<mount><burst-size>20</burst-size></mount>"
    ),
    20
  )
  test.equal(
    burst(
      "<limits><burst-size>10</burst-size></limits>",
      "<mount><burst-size>0</burst-size></mount>"
    ),
    null
  )
  test.equal(
    burst(
      "<limits><burst-on-connect>0</burst-on-connect><burst-size>10</burst-size></limits>",
      "<mount><burst-size>20</burst-size></mount>"
    ),
    null
  )
  test.equal(icecast.server.config.limits("<limits/>").max_sources, null)

  # <paths>: tls-certificate wins over ssl-certificate
  p =
    icecast.server.config.paths(
      "<paths><logdir>/var/log</logdir><ssl-certificate>/old.pem</ssl-certificate><tls-certificate>/new.pem</tls-certificate><tls-key>/key.pem</tls-key><alias \
       source=\"/a\" dest=\"/b\"/></paths>"
    )
  test.equal(p.logdir, "/var/log")
  test.equal(p.tls_certificate, "/new.pem")
  test.equal(p.tls_key, "/key.pem")
  p =
    icecast.server.config.paths(
      "<paths><ssl-certificate>/old.pem</ssl-certificate></paths>"
    )
  test.equal(p.tls_certificate, "/old.pem")
  test.equal(p.logdir, null)

  # <authentication>
  test.equal(
    icecast.server.config.authentication(
      "<authentication><source-password>pw</source-password></authentication>"
    ).password,
    "pw"
  )
  test.equal(
    icecast.server.config.authentication("<authentication/>").password,
    null
  )
  a =
    icecast.server.config.authentication(
      "<authentication><admin-user>root</admin-user><admin-password>apw</admin-password></authentication>"
    )
  test.equal(a.admin_user, "root")
  test.equal(a.admin_password, "apw")

  # <logging>
  l =
    icecast.server.config.logging.parse(
      '<logging>
         <accesslog>a.log</accesslog>
         <errorlog>-</errorlog>
         <playlistlog>p.log</playlistlog>
         <loglevel>debug</loglevel>
         <logsize>100</logsize>
         <logarchive>1</logarchive>
         <memorybacklog logfile="error">10</memorybacklog>
       </logging>'
    )
  test.equal(l.accesslog, "a.log")
  test.equal(l.errorlog, "-")
  test.equal(l.playlistlog, "p.log")
  test.equal(l.loglevel, 4)
  test.equal(l.logsize, 100)
  test.equal(l.logarchive, true)
  l = icecast.server.config.logging.parse("<logging/>")
  test.equal(l.accesslog, null)
  test.equal(l.loglevel, null)
  test.equal(l.logarchive, false)
  test.equal(icecast.server.config.logging.level("1"), 1)
  test.equal(icecast.server.config.logging.level("INFO"), 3)
  test.equal(icecast.server.config.logging.level("verbose"), null)

  # Full document: repeated mounts, default mount split off, sections optional
  c =
    icecast.server.config.parse(
      '<icecast>
         <limits><sources>3</sources></limits>
         <authentication><source-password>pw</source-password></authentication>
         <logging><playlistlog>p.log</playlistlog></logging>
         <listen-socket><port>9000</port></listen-socket>
         <listen-socket><port>9001</port></listen-socket>
         <http-headers><header name="X-Global" value="1" /></http-headers>
         <mount type="default"><fallback-mount>/backup</fallback-mount></mount>
         <mount><mount-name>/live</mount-name></mount>
         <mount type="normal"><mount-name>/backup</mount-name></mount>
       </icecast>'
    )
  test.equal(c.port, 9000)
  test.equal(c.password, "pw")
  test.equal(c.max_sources, 3)
  test.equal(c.http_headers, [("X-Global", "1")])
  test.equal(c.default_mount.fallback_mount, "/backup")
  test.equal(list.map(fun (m) -> m.mount_name, c.mounts), ["/live", "/backup"])
  test.equal(c.tls, false)
  test.equal(c.logdir, null)
  test.equal(c.logging.playlistlog, "p.log")
  test.equal(c.admin_password, null)

  c = icecast.server.config.parse("<icecast/>")
  test.equal(c.port, null)
  test.equal(c.logging.accesslog, null)
  test.equal(c.mounts, [])
  test.equal(c.default_mount.mount_name, null)

  # resolve: explicit parameters win over the config file, which wins over defaults
  c =
    icecast.server.config.parse(
      '<icecast>
         <listen-socket><port>9000</port></listen-socket>
         <authentication>
           <source-password>pw</source-password>
           <admin-password>apw</admin-password>
         </authentication>
         <paths><logdir>/logs</logdir></paths>
         <logging><accesslog>a.log</accesslog></logging>
       </icecast>'
    )
  def resolve(
    ~port=null,
    ~password=null,
    ~access_log=null,
    ~admin_password=null,
    ~from_file=true,
    parsed
  ) =
    icecast.server.config.resolve(
      port=port,
      password=password,
      access_log=access_log,
      playlist_log=null,
      admin_user=null,
      admin_password=admin_password,
      from_file=from_file,
      parsed
    )
  end
  r =
    resolve(
      port=8080,
      password="explicit",
      access_log="/tmp/x.log",
      admin_password="explicit",
      c
    )
  test.equal(r.port, 8080)
  test.equal(r.password, "explicit")
  test.equal(r.access_log, "/tmp/x.log")
  test.equal(r.admin_password, "explicit")
  r = resolve(c)
  test.equal(r.port, 9000)
  test.equal(r.password, "pw")
  test.equal(r.access_log, "/logs/a.log")
  test.equal(r.error_log, "/logs/error.log")
  test.equal(r.playlist_log, null)
  test.equal(r.admin_user, "admin")
  test.equal(r.admin_password, "apw")
  r = resolve(from_file=false, icecast.server.config.empty)
  test.equal(r.port, 8000)
  test.equal(r.password, "hackme")
  test.equal(r.access_log, null)
  test.equal(r.error_log, null)
  test.equal(r.admin_password, null)

  # Errors name the section
  def fails(doc) =
    try
      ignore(icecast.server.config.parse(doc))
      null
    catch err : [error.invalid] do
      err.message
    end
  end
  test.equal(
    string.contains(
      prefix="Invalid <limits> section in icecast config:",
      fails("<icecast><limits><sources>many</sources></limits></icecast>") ?? ""
    ),
    true
  )
  test.equal(
    string.contains(
      prefix="Invalid <mount> section in icecast config:",
      fails("<icecast><mount><burst-size>big</burst-size></mount></icecast>")
        ?? ""
    ),
    true
  )
  test.equal(
    string.contains(
      prefix="Invalid icecast config: root element",
      fails("<foo/>") ?? ""
    ),
    true
  )

  test.pass()
end

test.check(f)
