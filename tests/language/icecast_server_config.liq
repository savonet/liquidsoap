%include "../test.liq"
%include "../../src/libs/extra/icecast_server/settings.liq"
%include "../../src/libs/extra/icecast_server/headers.liq"
%include "../../src/libs/extra/icecast_server/addresses.liq"
%include "../../src/libs/extra/icecast_server/refusals.liq"
%include "../../src/libs/extra/icecast_server/parameter_checks.liq"
%include "../../src/libs/extra/icecast_server/parameters.liq"
%include "../../src/libs/extra/icecast_server/mount_parameters.liq"
%include "../../src/libs/extra/icecast_server/prerolls.liq"
%include "../../src/libs/extra/icecast_server/decisions.liq"
%include "../../src/libs/extra/icecast_server/url_auth_options.liq"
%include "../../src/libs/extra/icecast_server/url_auth_reply.liq"
%include "../../src/libs/extra/icecast_server/url_auth_request.liq"
%include "../../src/libs/extra/icecast_server/url_auth.liq"
%include "../../src/libs/extra/icecast_server/access.liq"
%include "../../src/libs/extra/icecast_server/source_url.liq"
%include "../../src/libs/extra/icecast_server/config/read.liq"
%include "../../src/libs/extra/icecast_server/config/listen.liq"
%include "../../src/libs/extra/icecast_server/config/limits.liq"
%include "../../src/libs/extra/icecast_server/config/paths.liq"
%include "../../src/libs/extra/icecast_server/config/headers.liq"
%include "../../src/libs/extra/icecast_server/config/events.liq"
%include "../../src/libs/extra/icecast_server/config/mount_auth.liq"
%include "../../src/libs/extra/icecast_server/config/mount_entry.liq"
%include "../../src/libs/extra/icecast_server/config/mounts.liq"
%include "../../src/libs/extra/icecast_server/config/load.liq"

# Translation of Icecast configuration files (icecast-config.md §12).
checks = ref(0)
log_path = file.temp("icecast-config", ".log")
settings.log.file.path := log_path
settings.log.file := true
settings.log.level := 4
directory = file.temp_dir(cleanup=true, "icecast-config")
fixtures = ref(0)
tls_factory_calls = ref([])
settings.icecast.server.tls_transport :=
  (
    "fake",
    fun (certificate, key) ->
      begin
        tls_factory_calls :=
          [(getter.get(certificate), getter.get(key)), ...tls_factory_calls()]
        http.transport.unix.{reload = fun () -> ()}
      end
  )

def check_equal(label, got, expected) =
  checks := checks() + 1
  if
    got != expected
  then
    test.fail(
      "#{label}: got #{got}, expected #{expected}"
    )
  end
end

# Fixture paths with whether translating them must warn, checked once at the
# end: `log.flush` waits for the logging thread.
expected_warnings = ref([])

def logged(text) =
  log.flush()
  list.exists(
    fun (line) -> string.contains(substring=text, line),
    file.lines(log_path)
  )
end

def write(name, content) =
  path = path.concat(directory, name)
  file.write(data=content, path)
  path
end

def load(~compatibility="icecast-2.5", ~warns=null, content) =
  fixtures := fixtures() + 1
  path = write("icecast-#{fixtures()}.xml", "<icecast>#{content}</icecast>")
  if
    null.defined(warns)
  then
    expected_warnings := [(path, null.get(warns)), ...expected_warnings()]
  end
  icecast.server.config.load(compatibility=compatibility, path)
end

def warns(content) =
  load(warns=true, content)
end

# Warnings of each translated fixture, from the "Translating" line `load` logs
# to the next one.
def check_warnings() =
  log.flush()
  counts = ref([])
  current = ref("")
  list.iter(
    fun (line) ->
      if
        string.contains(
          substring="] Translating ",
          line
        )
      then
        let (_, translated) =
          string.split.first(
            separator="] Translating ",
            line
          )
        current :=
          string.sub(translated, start=0, length=string.length(translated) - 1)
        counts := [(current(), 0), ...counts()]
      elsif
        string.contains(substring="[icecast.server.config:3]", line)
        and current() != ""
      then
        n = list.assoc(current(), counts())
        counts :=
          [(current(), n + 1), ...list.assoc.remove(current(), counts())]
      end,
    file.lines(log_path)
  )
  list.iter(
    fun (e) ->
      begin
        let (path, warns) = e
        n = list.assoc.nullable(path, counts())
        check_equal(
          "#{path} was translated",
          null.defined(n),
          true
        )
        check_equal(
          "#{path} warns",
          (n ?? 0) > 0,
          warns
        )
      end,
    expected_warnings()
  )
  check_equal(
    "fixtures translated",
    list.length(counts()) >= list.length(expected_warnings()),
    true
  )
end

def rejects(~compatibility="icecast-2.5", label, content) =
  rejected =
    try
      fixtures := fixtures() + 1
      ignore(
        icecast.server.config.load(
          compatibility=compatibility,
          write("icecast-#{fixtures()}.xml", content)
        )
      )
      false
    catch _ : [error.icecast.server.invalid_parameters] do
      true
    end
  check_equal(
    "#{label} is rejected",
    rejected,
    true
  )
end

def mount(p, path) =
  icecast.server.parameters.mount(icecast.server.parameters.server(p), path)
end

def listener_request(
  ~user="",
  ~password="",
  ~address="203.0.113.7",
  ~user_listeners=0
) =
  icecast.server.decision.request(
    server={hostname = "radio.example", port = 8000, instance = "uuid"},
    compatibility="icecast-2.5",
    address=address,
    listeners=0,
    user_listeners=fun (_) -> user_listeners,
    {
      id = 1,
      uri = "/live",
      query_string = "",
      protocol = "1.1",
      ip = address,
      headers =
        user == ""
          ? []
          : [
            (
              "authorization",
              "Basic #{string.base64.encode("#{user}:#{password}")}"
            )
          ]
    }
  )
end

def status(p, path, request) =
  decision = mount(p, path).listener_decision
  null.case(
    decision,
    {null},
    fun (decide) -> null.map(fun (r) -> r.status, decide(request).refuse)
  )
end

def check_failures() =
  rejects(
    "a root other than <icecast>",
    "<icecast2/>"
  )
  rejects(
    "malformed XML",
    "<icecast><port>"
  )
  rejects(
    "a non-numeric <port>",
    "<icecast><port>80a</port></icecast>"
  )
  rejects(
    "a non-numeric mount value",
    "<icecast><mount><mount-name>/a</mount-name><max-listeners>x</max-listeners></mount></icecast>"
  )
  rejects(
    compatibility="icecast-3",
    "an unknown compatibility",
    "<icecast/>"
  )
  rejects(
    "an unknown authentication type",
    '<icecast><mount><mount-name>/a</mount-name><authentication \
     type="command"/></mount></icecast>'
  )
  rejects(
    "role authentication",
    '<icecast><mount><mount-name>/a</mount-name><authentication><role \
     type="anonymous"/></authentication></mount></icecast>'
  )
  rejects(
    "an invalid record",
    "<icecast><port>70000</port></icecast>"
  )
  rejects(
    "a malformed auth_header",
    '<icecast><mount><mount-name>/a</mount-name><authentication \
     type="url"><option name="auth_header" value=": \
     1"/></authentication></mount></icecast>'
  )
end

def check_server_values() =
  p =
    load(
      "<hostname>radio.example</hostname><location>Here</location><admin>ops@example</admin><server-id>Box</server-id><authentication><source-password>src</source-password><admin-user>boss</admin-user><admin-password>adm</admin-password><health-password>hp</health-password></authentication><limits><sources>3</sources><clients>40</clients><client-timeout>12.5</client-timeout><source-timeout>4</source-timeout><burst-size>1000</burst-size></limits><shoutcast-mount>/sc</shoutcast-mount><paths><logdir>/logs</logdir></paths><logging><loglevel>warn</loglevel><logsize>10</logsize><logarchive>1</logarchive><playlistlog>play.log</playlistlog><errorlog>-</errorlog></logging>"
    )
  check_equal("compatibility", p.compatibility, "icecast-2.5")
  check_equal("hostname", p.hostname, "radio.example")
  check_equal("location", p.location, "Here")
  check_equal("admin", p.admin_contact, "ops@example")
  check_equal("server-id", p.server_id, "Box")
  check_equal("source-password", p.source_password, "src")
  check_equal("admin-user", p.admin_user, "boss")
  check_equal("admin-password", p.admin_password, "adm")
  check_equal("health-password", p.health_password, "hp")
  check_equal("sources", p.max_sources, 3)
  check_equal("clients", p.max_clients, 40)
  check_equal("client-timeout", p.client_timeout, 12.5)
  check_equal("source-timeout", p.source_timeout, 4.)
  check_equal("burst-size", p.burst_size, 1000)
  check_equal("shoutcast-mount", p.shoutcast_mount, "/sc")
  check_equal(
    "loglevel warn",
    p.log_level,
    3
  )
  check_equal(
    "logsize in KiB",
    p.log_size,
    10240
  )
  check_equal("logarchive", p.log_archive, true)
  check_equal(
    "accesslog default name",
    p.access_log,
    "/logs/access.log"
  )
  check_equal(
    "errorlog -",
    p.error_log,
    "-"
  )
  check_equal("playlistlog", p.playlist_log, "/logs/play.log")

  levels =
    [("1", 2), ("error", 2), ("3", 4), ("INFO", 4), ("4", 5), ("debug", 5)]
  list.iter(
    fun (level) ->
      check_equal(
        "loglevel #{fst(level)}",
        load("<logging><loglevel>#{fst(level)}</loglevel></logging>").log_level,
        snd(level)
      ),
    levels
  )
  p = warns("<logging><loglevel>loud</loglevel></logging>")
  check_equal(
    "unknown loglevel keeps the default",
    p.log_level,
    null
  )

  p = load("")
  check_equal(
    "defaults: port",
    p.port,
    null
  )
  check_equal(
    "defaults: access log needs a logdir",
    p.access_log,
    null
  )
  check_equal(
    "defaults: no playlist log",
    p.playlist_log,
    null
  )
  p = warns("<logging><accesslog>a.log</accesslog></logging>")
  check_equal(
    "a log name without logdir is ignored",
    p.access_log,
    null
  )

  p =
    load(
      "<source-password>kh</source-password><port>8200</port><bind-address>::1</bind-address>"
    )
  check_equal(
    "KH root source-password",
    p.source_password,
    "kh"
  )
  check_equal(
    "root port",
    p.port,
    8200
  )
  check_equal(
    "root bind-address",
    p.bind_address,
    "::1"
  )
  p =
    load(
      "<source-password>kh</source-password><authentication><source-password>main</source-password></authentication>"
    )
  check_equal(
    "authentication source-password wins",
    p.source_password,
    "main"
  )
end

def check_unsupported() =
  base = "<hostname>h</hostname><mount><mount-name>/a</mount-name></mount>"
  plain = load(warns=false, base)
  list.iter(
    fun (extra) ->
      begin
        p = warns("#{base}#{extra}")
        check_equal(
          "#{extra}: hostname unchanged",
          p.hostname,
          plain.hostname
        )
        check_equal(
          "#{extra}: port unchanged",
          p.port,
          plain.port
        )
        check_equal(
          "#{extra}: mount unchanged",
          mount(p, "/a").max_listeners,
          mount(plain, "/a").max_listeners
        )
      end,
    [
      "<fileserve>1</fileserve>",
      "<relay><server>x</server></relay>",
      "<master-server>m</master-server>",
      "<security><chroot>0</chroot></security>",
      "<directory><yp-url>u</yp-url></directory>",
      "<frobnicate/>",
      "<limits><queue-size>10</queue-size></limits>",
      "<paths><basedir>/b</basedir></paths>",
      "<logging><memorybacklog>5</memorybacklog></logging>",
      "<logging><accesslog_lines>5</accesslog_lines></logging>",
      "<authentication><relay-password>r</relay-password></authentication>"
    ]
  )
  p =
    warns(
      "<mount><mount-name>/b</mount-name><genre>jazz</genre><postroll-file>p</postroll-file><wait-time>3</wait-time><max-listeners>4</max-listeners></mount>"
    )
  check_equal(
    "mount still translated",
    mount(p, "/b").max_listeners,
    4
  )
  p =
    warns(
      '<mount type="other"><mount-name>/b</mount-name></mount>'
    )
  check_equal(
    "a mount of another type is ignored",
    null.defined(p.mount("/b")),
    false
  )
  p = warns("<mount><max-listeners>4</max-listeners></mount>")
  check_equal(
    "a mount without name is ignored",
    null.defined(p.mount("/b")),
    false
  )
end

def check_listen() =
  p =
    warns(
      "<listen-socket><port>9001</port><bind-address>127.0.0.1</bind-address><shoutcast-mount>/s1</shoutcast-mount></listen-socket><listen-socket><port>9002</port></listen-socket><port>9003</port>"
    )
  check_equal(
    "first listen-socket port",
    p.port,
    9001
  )
  check_equal(
    "first listen-socket address",
    p.bind_address,
    "127.0.0.1"
  )
  check_equal(
    "listen-socket shoutcast-mount",
    p.shoutcast_mount,
    "/s1"
  )

  certificate = "<paths><tls-certificate>/c.pem</tls-certificate></paths>"
  list.iter(
    fun (value) ->
      begin
        tls_factory_calls := []
        ignore(load("<listen-socket><tls>#{value}</tls></listen-socket>"))
        check_equal(
          "tls #{value} is plain HTTP",
          tls_factory_calls(),
          []
        )
      end,
    ["0", "disabled", "false", "no", "off"]
  )
  list.iter(
    fun (value) ->
      begin
        tls_factory_calls := []
        ignore(
          load(
            "<listen-socket><ssl>#{value}</ssl></listen-socket>#{certificate}"
          )
        )
        check_equal(
          "tls #{value} is TLS",
          tls_factory_calls(),
          [("/c.pem", null)]
        )
      end,
    ["1", "rfc2818", "true", "yes", "on"]
  )
  tls_factory_calls := []
  ignore(
    load(
      "<listen-socket><tls>1</tls></listen-socket><paths><tls-context><tls-certificate>/x.pem</tls-certificate><tls-key>/x.key</tls-key></tls-context></paths>"
    )
  )
  check_equal(
    "tls-context certificate and key",
    tls_factory_calls(),
    [("/x.pem", "/x.key")]
  )
  list.iter(
    fun (value) ->
      rejects(
        "tls #{string.quote(value)}",
        "<icecast><listen-socket><tls>#{value}</tls></listen-socket>#{
          certificate
        }</icecast>"
      ),
    ["auto", "auto_no_plain", "rfc2817", "maybe", ""]
  )
  rejects(
    "a certificate without tls mode",
    "<icecast>#{certificate}</icecast>"
  )
  rejects(
    "tls without certificate",
    "<icecast><listen-socket><tls>1</tls></listen-socket></icecast>"
  )

  p =
    load(
      '<listen-socket><port>1</port><trusted-proxy>edge</trusted-proxy></listen-socket><listen-socket \
       id="edge"><client-ip>10.0.0.0/8</client-ip></listen-socket><paths><x-forwarded-for>192.0.2.1</x-forwarded-for></paths>'
    )
  check_equal(
    "trusted-proxy and x-forwarded-for fill proxy_ips",
    p.proxy_ips,
    ["10.0.0.0/8", "192.0.2.1"]
  )
  check_equal(
    "no proxy element: proxy_ips unset",
    load("").proxy_ips,
    null
  )
  p =
    warns(
      "<listen-socket><port>1</port><trusted-proxy/><client-ip>0.0.0.0/0</client-ip></listen-socket>"
    )
  check_equal(
    "an empty trusted-proxy trusts nothing",
    p.proxy_ips,
    null
  )
  p =
    load(
      "<listen-socket><port>1</port><trusted-proxy>203.0.113.0/24</trusted-proxy></listen-socket>"
    )
  check_equal(
    "a trusted-proxy naming no socket is an address",
    p.proxy_ips,
    ["203.0.113.0/24"]
  )
end

def check_bursts() =
  p = load("<limits><burst-on-connect>0</burst-on-connect></limits>")
  check_equal(
    "burst-on-connect 0",
    p.burst_on_connect,
    false
  )
  p = load("<limits><burst-on-connect>1</burst-on-connect></limits>")
  check_equal(
    "burst-on-connect 1 keeps the default",
    p.burst_on_connect,
    null
  )
  check_equal(
    "burst-size 0",
    load("<limits><burst-size>0</burst-size></limits>").burst_size,
    0
  )
  check_equal(
    "burst unset",
    icecast.server.parameters.server(load("")).burst_size,
    65536
  )
  p =
    load(
      "<limits><burst-size>0</burst-size></limits><mount><mount-name>/a</mount-name><burst-size>500</burst-size></mount>"
    )
  check_equal(
    "mount burst-size beats a root 0",
    mount(p, "/a").burst_size,
    500
  )
  p =
    load(
      "<limits><burst-size>500</burst-size></limits><mount><mount-name>/a</mount-name><burst-size>0</burst-size></mount>"
    )
  check_equal(
    "a mount burst-size 0 beats the root",
    mount(p, "/a").burst_size,
    0
  )
end

def check_mount_values() =
  p =
    load(
      '<mount><mount-name>/a</mount-name><username>dj</username><password>pw</password><max-listeners>5</max-listeners><fallback-mount>/b</fallback-mount><fallback-when-full>1</fallback-when-full><linger-for>2.5</linger-for><hidden>1</hidden><max-listener-duration>60</max-listener-duration><max-history>300</max-history><health-password>mh</health-password><dump-file \
       interpolation="none">/d/50%.mp3</dump-file><dump-file-size-limit>99</dump-file-size-limit><dump-file-time-limit>9</dump-file-time-limit><on-connect>/bin/true</on-connect></mount><mount><mount-name>/k</mount-name><fallback><mount>/z</mount></fallback></mount>'
    )
  m = mount(p, "/a")
  check_equal("username", m.source_user, "dj")
  check_equal("password", m.source_password, "pw")
  check_equal("max-listeners", m.max_listeners, 5)
  check_equal("fallback-mount", m.fallback?.mount, "/b")
  check_equal("fallback-when-full", m.fallback_when_full, true)
  check_equal("linger-for", m.linger, 2.5)
  check_equal("hidden", m.hidden, true)
  check_equal("max-listener-duration", m.max_listener_duration, 60.)
  check_equal(
    "max-history out of range",
    m.max_history,
    256
  )
  check_equal(
    "mount health-password",
    m.health_password,
    "mh"
  )
  check_equal(
    "dump-file interpolation none",
    m.dump_file,
    "/d/50%%.mp3"
  )
  check_equal("dump-file-size-limit", m.dump_file_size_limit, 99)
  check_equal("dump-file-time-limit", m.dump_file_time_limit, 9.)
  check_equal("on-connect", null.defined(m.on_connect), true)
  check_equal(
    "on-disconnect unset",
    null.defined(m.on_disconnect),
    false
  )
  check_equal(
    "KH nested fallback",
    mount(p, "/k").fallback?.mount,
    "/z"
  )
  check_equal(
    "fallback-when-full unset",
    mount(
      load("<mount><mount-name>/a</mount-name></mount>"),
      "/a"
    ).fallback_when_full,
    false
  )
  list.iter(
    fun (value) ->
      ignore(
        load(
          warns=list.mem(value, ["0", "false", "none"]),
          "<mount><mount-name>/a</mount-name><fallback-override>#{
            value
          }</fallback-override></mount>"
        )
      ),
    ["0", "false", "none", "1", "true", "all", "own"]
  )
end

def check_selection() =
  file =
    '<mount><mount-name>/live</mount-name><max-listeners>1</max-listeners><http-headers><header \
     name="X-Own" value="own"/><header name="X-Both" \
     value="own"/></http-headers></mount><mount \
     type="default"><mount-name>/a*</mount-name><max-listeners>2</max-listeners></mount><mount \
     type="default"><max-listeners>3</max-listeners><hidden>1</hidden><burst-size>7</burst-size><http-headers><header \
     name="X-Both" value="default"/><header name="X-Default" \
     value="d"/></http-headers></mount><mount><mount-name>/b*</mount-name><priority>2</priority><max-listeners>20</max-listeners></mount><mount><mount-name>/b/*</mount-name><priority>1</priority><max-listeners>10</max-listeners></mount><mount><mount-name>/b/x</mount-name><max-listeners>30</max-listeners></mount><mount><mount-name>/hidden</mount-name><hidden>0</hidden></mount>'
  p = load(file)
  check_equal(
    "exact normal mount",
    mount(p, "/live").max_listeners,
    1
  )
  check_equal(
    "merged from the default mount",
    mount(p, "/live").burst_size,
    7
  )
  check_equal(
    "headers: own, then the default mount's",
    list.map(fun (h) -> (h.name, h?.value), mount(p, "/live").http_headers),
    [
      ("X-Own", "own"),
      ("X-Both", "own"),
      ("X-Both", "default"),
      ("X-Default", "d")
    ]
  )
  check_equal(
    "default mount pattern",
    mount(p, "/abc").max_listeners,
    2
  )
  check_equal(
    "* does not match /",
    mount(p, "/a/c").max_listeners,
    3
  )
  check_equal(
    "nameless default mount",
    mount(p, "/zzz").max_listeners,
    3
  )
  check_equal(
    "own hidden false wins",
    mount(p, "/hidden").hidden,
    false
  )
  check_equal(
    "normal names are exact outside kh",
    mount(p, "/b/y").max_listeners,
    3
  )
  k = load(compatibility="kh", file)
  check_equal(
    "kh: lowest priority wins",
    mount(k, "/b/y").max_listeners,
    10
  )
  check_equal(
    "kh: exact name wins",
    mount(k, "/b/x").max_listeners,
    30
  )
  check_equal(
    "kh: * matches /",
    mount(k, "/bz/y").max_listeners,
    20
  )
  check_equal(
    "kh: compatibility in the record",
    k.compatibility,
    "kh"
  )
  check_equal(
    "no mount matches",
    null.defined(
      load("<mount><mount-name>/a</mount-name></mount>").mount("/b")
    ),
    false
  )
  p = load("<limits><max-listeners>8</max-listeners></limits>")
  check_equal(
    "KH max-listeners is every mount's default",
    mount(p, "/any").max_listeners,
    8
  )
end

def check_url_auth() =
  options =
    [
      ("listener_add", "http://a/add"),
      ("client_add", "http://a/cadd"),
      ("listener_remove", "http://a/rm"),
      ("client_remove", "http://a/crm"),
      ("username", "u"),
      ("password", "p"),
      (
        "auth_header",
        "icecast-auth-user: 1"
      ),
      ("timelimit_header", "icecast-auth-timelimit:"),
      ("header_auth", "x-a"),
      ("header_timelimit", "x-t"),
      ("header_message", "x-m"),
      ("header_alter_action", "x-aa"),
      ("header_alter_argument", "x-ag"),
      ("action_add", "add"),
      ("action_remove", "rm"),
      ("headers", "user-agent"),
      ("header_prefix", "h-"),
      ("timeout", "2.5"),
      ("presume_innocent", "1"),
      ("on_error_wait", "4"),
      ("mount_add", "http://a/madd"),
      ("mount_remove", "http://a/mrm"),
      ("stream_auth", "http://a/sa")
    ]
  elements =
    string.concat(
      list.map(
        fun (o) ->
          '<option name="#{fst(o)}" value="#{snd(o)}"/>',
        options
      )
    )
  o =
    icecast.server.config.url_options(
      xml.tree("<authentication>#{elements}</authentication>")
    )
  read =
    [
      ("listener_add", o.listener_add),
      ("client_add", o.client_add),
      ("listener_remove", o.listener_remove),
      ("client_remove", o.client_remove),
      ("username", o.username),
      ("password", o.password),
      ("auth_header", o.auth_header),
      ("timelimit_header", o.timelimit_header),
      ("header_auth", o.header_auth),
      ("header_timelimit", o.header_timelimit),
      ("header_message", o.header_message),
      ("header_alter_action", o.header_alter_action),
      ("header_alter_argument", o.header_alter_argument),
      ("action_add", o.action_add),
      ("action_remove", o.action_remove),
      ("headers", o.headers),
      ("header_prefix", o.header_prefix),
      ("mount_add", o.mount_add),
      ("mount_remove", o.mount_remove),
      ("stream_auth", o.stream_auth)
    ]
  list.iter(
    fun (r) ->
      check_equal(
        "url option #{fst(r)}",
        snd(r),
        list.assoc.nullable(fst(r), options)
      ),
    read
  )
  check_equal(
    "url option timeout",
    o.timeout,
    2.5
  )
  check_equal(
    "url option presume_innocent",
    o.presume_innocent,
    true
  )
  check_equal(
    "url option on_error_wait",
    o.on_error_wait,
    4.
  )

  def auth(options) =
    "<mount><mount-name>/live</mount-name><authentication type=\"url\">#{
      options
    }</authentication></mount>"
  end

  unreachable = "http://127.0.0.1:1/auth"
  p =
    load(
      auth(
        '<option name="listener_add" value="#{unreachable}"/><option \
         name="timeout" value="1"/>'
      )
    )
  check_equal(
    "a failed listener_add refuses",
    status(p, "/live", listener_request()),
    401
  )
  p =
    load(
      auth(
        '<option name="listener_add" value="#{unreachable}"/><option \
         name="presume_innocent" value="1"/>'
      )
    )
  check_equal(
    "presume_innocent reaches the decision",
    status(p, "/live", listener_request()),
    null
  )
  p =
    load(
      auth(
        '<option name="mount_add" value="#{unreachable}"/><option \
         name="stream_auth" value="#{unreachable}"/>'
      )
    )
  m = mount(p, "/live")
  check_equal(
    "mount_add without listener_add: no listener decision",
    null.defined(m.listener_decision),
    false
  )
  check_equal(
    "mount_add gives on_connect",
    null.defined(m.on_connect),
    true
  )
  check_equal(
    "mount_add gives on_disconnect",
    null.defined(m.on_disconnect),
    true
  )
  check_equal(
    "stream_auth gives source_decision",
    null.defined(m.source_decision),
    true
  )
  ignore(
    warns(
      auth(
        '<option name="frob" value="1"/>'
      )
    )
  )
end

def check_access_and_htpasswd() =
  users = write("users", "bob:#{string.digest("pw")}\n")
  deny = write("deny", "198.51.100.*\n")
  def htpasswd(attributes, options) =
    '<paths><deny-ip>#{
      deny
    }</deny-ip></paths><mount><mount-name>/live</mount-name><authentication \
     type="htpasswd" #{attributes}><option name="filename" value="#{users}"/>#{
      options
    }</authentication></mount>'
  end

  p = load(htpasswd("", ""))
  check_equal(
    "htpasswd admits",
    status(p, "/live", listener_request(user="bob", password="pw")),
    null
  )
  check_equal(
    "htpasswd refuses a wrong password",
    status(p, "/live", listener_request(user="bob", password="no")),
    401
  )
  check_equal(
    "access lists come first",
    status(
      p,
      "/live",
      listener_request(user="bob", password="no", address="198.51.100.9")
    ),
    403
  )
  check_equal(
    "access lists apply to every mount",
    status(p, "/other", listener_request(address="198.51.100.9")),
    403
  )
  check_equal(
    "duplicates allowed by default",
    status(
      p,
      "/live",
      listener_request(user="bob", password="pw", user_listeners=1)
    ),
    null
  )
  p =
    load(
      htpasswd(
        "",
        '<option name="allow_duplicate_users" value="0"/>'
      )
    )
  check_equal(
    "allow_duplicate_users 0",
    status(
      p,
      "/live",
      listener_request(user="bob", password="pw", user_listeners=1)
    ),
    429
  )
  p = load(htpasswd('connections-per-user="2"', ""))
  check_equal(
    "connections-per-user under the limit",
    status(
      p,
      "/live",
      listener_request(user="bob", password="pw", user_listeners=1)
    ),
    null
  )
  check_equal(
    "connections-per-user at the limit",
    status(
      p,
      "/live",
      listener_request(user="bob", password="pw", user_listeners=2)
    ),
    429
  )
  p = load(htpasswd('connections-per-user="unlimited"', ""))
  check_equal(
    "connections-per-user unlimited",
    status(
      p,
      "/live",
      listener_request(user="bob", password="pw", user_listeners=9)
    ),
    null
  )
  rejects(
    "htpasswd without filename",
    '<icecast><mount><mount-name>/a</mount-name><authentication \
     type="htpasswd"/></mount></icecast>'
  )
end

def check_prerolls() =
  webroot = path.concat(directory, "web")
  file.mkdir(webroot)
  ignore(write("web/intro.bin", "INTRO"))
  ignore(write("web/pre.bin", "PRE"))
  p =
    load(
      "<paths><webroot>#{
        webroot
      }</webroot></paths><mount><mount-name>/live</mount-name><preroll>pre.bin</preroll></mount><mount \
       type=\"default\"><intro>intro.bin</intro></mount>"
    )
  decide = null.get(mount(p, "/live").listener_decision)
  check_equal(
    "intro first, from webroot",
    decide(listener_request()).preroll,
    "INTRO"
  )
  p =
    load(
      "<paths><webroot>#{
        webroot
      }</webroot></paths><mount><mount-name>/live</mount-name><preroll>pre.bin</preroll></mount>"
    )
  decide = null.get(mount(p, "/live").listener_decision)
  check_equal(
    "RSAS preroll",
    decide(listener_request()).preroll,
    "PRE"
  )
end

def check_headers() =
  def values(~compatibility="icecast-2.5", ~status=200, entries) =
    icecast.server.headers.reply(
      request={
        headers = [("origin", "http://o")],
        compatibility = compatibility
      },
      status=status,
      entries
    )
  end

  headers =
    '<http-headers><header name="Access-Control-Allow-Origin" \
     type="cors"/><header name="X-Static" value="s" status="200"/><header \
     name="X-Cors-Value" type="cors" value="v"/></http-headers>'
  p = load(headers)
  check_equal(
    "cors header under icecast-2.5",
    values(p.http_headers),
    [
      ("Access-Control-Allow-Origin", "http://o"),
      ("X-Static", "s"),
      ("X-Cors-Value", "v")
    ]
  )
  check_equal(
    "header status",
    values(status=404, p.http_headers),
    [("Access-Control-Allow-Origin", "null"), ("X-Cors-Value", "v")]
  )
  p = load(compatibility="icecast-2.4", warns=true, headers)
  check_equal(
    "cors type is static under icecast-2.4",
    values(compatibility="icecast-2.4", p.http_headers),
    [("X-Static", "s"), ("X-Cors-Value", "v")]
  )
  p =
    warns(
      '<http-headers><header name="X-None" type="cors"/></http-headers>'
    )
  check_equal(
    "a cors header outside the set is skipped",
    list.length(p.http_headers),
    0
  )
  p =
    warns(
      '<http-headers><header name="X-None"/></http-headers>'
    )
  check_equal(
    "a static header without value is skipped",
    list.length(p.http_headers),
    0
  )

  rsas =
    '<custom-headers><add-header name="X-Add" \
     value="a"/><cors><allow-origin>http://o</allow-origin><expose-headers>x-more</expose-headers></cors></custom-headers>'
  p = load(compatibility="kh", rsas)
  check_equal(
    "RSAS cors block under kh",
    values(compatibility="kh", p.http_headers),
    [
      ("X-Add", "a"),
      ("Access-Control-Allow-Origin", "http://o"),
      ("Access-Control-Allow-Methods", "GET"),
      (
        "Access-Control-Expose-Headers",
        "content-range, icy-br, icy-description, icy-genre, icy-name, icy-pub, \
         icy-url, x-more"
      ),
      ("Access-Control-Max-Age", "300"),
      (
        "Access-Control-Allow-Headers",
        "range, if-range"
      )
    ]
  )
  p =
    load(
      '<custom-headers><cors><allow-origin>http://elsewhere</allow-origin></cors></custom-headers>'
    )
  check_equal(
    "RSAS cors origins",
    list.assoc.nullable("Access-Control-Allow-Origin", values(p.http_headers)),
    null
  )
  ignore(
    warns("<listen-socket><port>1</port><custom-headers/></listen-socket>")
  )
end

def check_events() =
  p =
    load(
      '<event-bindings><event type="log" trigger="source-connect, \
       source-listeners-changed"><option name="prefix" value="EV \
       "/></event><event type="log" trigger="nope"/><event \
       trigger="source-connect"/></event-bindings><mount><mount-name>/m</mount-name><kartoffelsalat><event \
       type="log" trigger="source-listeners-is-zero"><option name="prefix" \
       value="ZERO "/></event></kartoffelsalat></mount>'
    )
  check_equal(
    "one binding per trigger",
    list.map(fun (b) -> (b.event, b.mount), p.event_bindings),
    [
      ("source_connect", null),
      ("listener_connect", null),
      ("listener_disconnect", null),
      ("listener_disconnect", "/m")
    ]
  )
  zero = list.nth(p.event_bindings, 3)
  zero.handler({event = "listener_disconnect", mount = "/m", listeners = 1})
  check_equal(
    "is-zero skips a non-zero count",
    logged(
      "ZERO source-listeners-is-zero"
    ),
    false
  )
  zero.handler({event = "listener_disconnect", mount = "/m", listeners = 0})
  check_equal(
    "is-zero fires at zero",
    logged(
      "ZERO source-listeners-is-zero on /m"
    ),
    true
  )
  check_equal(
    "an untranslated event warns",
    logged("trigger=\"nope\""),
    true
  )
end

def check_purity() =
  logdir = path.concat(directory, "logs")
  file.mkdir(logdir)
  content =
    "<paths><logdir>#{
      logdir
    }</logdir></paths><hostname>h</hostname><mount><mount-name>/a</mount-name><max-listeners>2</max-listeners></mount>"
  a = load(content)
  b = load(content)
  check_equal(
    "same file, same values",
    (a.hostname, a.access_log, mount(a, "/a").max_listeners),
    (b.hostname, b.access_log, mount(b, "/a").max_listeners)
  )
  check_equal(
    "load writes no log file",
    file.ls(logdir),
    []
  )
end

def f() =
  check_failures()
  check_server_values()
  check_unsupported()
  check_listen()
  check_bursts()
  check_mount_values()
  check_selection()
  check_url_auth()
  check_access_and_htpasswd()
  check_prerolls()
  check_headers()
  check_events()
  check_purity()
  check_warnings()
  if
    checks() < 200 or fixtures() < 60
  then
    test.fail(
      "only #{checks()} checks over #{fixtures()} fixtures ran"
    )
  end
  print(
    "#{checks()} checks over #{fixtures()} fixtures ran"
  )
  test.pass()
end

test.check(f)
