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
%include "../../src/libs/extra/icecast_server/access.liq"

checks = ref(0)
log_path = file.temp("icecast-decisions", ".log")
settings.log.file.path := log_path
settings.log.file := true

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

def listener(~headers=[], ~user="", ~password="", ~ip="203.0.113.7") =
  authorization =
    user == "" and password == ""
      ? []
      : [
        (
          "authorization",
          "Basic #{string.base64.encode("#{user}:#{password}")}"
        )
      ]
  {
    id = 42,
    uri = "/live.mp3",
    query_string = "token=x",
    protocol = "1.1",
    ip = ip,
    headers = [...authorization, ...headers]
  }
end

def request(
  ~compatibility="icecast-2.5",
  ~headers=[],
  ~user="",
  ~password="",
  ~address="203.0.113.7",
  ~user_listeners=0
) =
  icecast.server.decision.request(
    server={hostname = "radio.example", port = 8000, instance = "uuid"},
    compatibility=compatibility,
    address=address,
    listeners=3,
    user_listeners=fun (_) -> user_listeners,
    listener(headers=headers, user=user, password=password)
  )
end

def status(outcome) =
  null.map(fun (refuse) -> refuse.status, outcome.refuse)
end

def check_request() =
  r = request(user="bob", password="pw:x", user_listeners=2)
  check_equal(
    "request user",
    r.user,
    "bob"
  )
  check_equal(
    "request password keeps colons",
    r.password,
    "pw:x"
  )
  check_equal(
    "request mount",
    r.mount,
    "/live.mp3"
  )
  check_equal(
    "request query",
    r.query,
    "token=x"
  )
  check_equal(
    "request user_listeners",
    r.user_listeners(r.user),
    2
  )
  check_equal(
    "request server",
    r.server.instance,
    "uuid"
  )
  r = request(user_listeners=5)
  check_equal(
    "no user: user_listeners 0",
    r.user_listeners(r.user),
    0
  )
  check_equal(
    "no user: empty password",
    r.password,
    ""
  )
end

def check_all() =
  released = ref([])
  def admit(name, outcome) =
    fun (_) ->
      {
        refuse = null,
        time_limit = outcome.time_limit,
        preroll = outcome.preroll,
        exempt = outcome.exempt,
        on_release =
          fun (release) ->
            released :=
              [...released(), (name, release.duration, release.listeners)]
      }
  end

  def refuse(code) =
    fun (_) ->
      {
        refuse = icecast.server.refusal(code),
        time_limit = null,
        preroll = null,
        exempt = false,
        on_release = fun (_) -> released := [...released(), ("refuser", 0., 0)]
      }
  end

  calls = ref(0)
  def counted(_) =
    calls := calls() + 1
    {
      refuse = null,
      time_limit = null,
      preroll = null,
      exempt = false,
      on_release = fun (_) -> ()
    }
  end

  a = admit("a", {time_limit = 10., preroll = "", exempt = false})
  b = admit("b", {time_limit = 20., preroll = "B", exempt = true})
  c = admit("c", {time_limit = null, preroll = null, exempt = false})

  decide = icecast.server.decision.all([c, a, b])
  o = decide(request())
  check_equal(
    "first time limit wins",
    o.time_limit,
    10.
  )
  check_equal(
    "first preroll wins, empty included",
    o.preroll,
    ""
  )
  check_equal(
    "exempt if any",
    o.exempt,
    true
  )
  decide = icecast.server.decision.all([b, a])
  check_equal(
    "exempt kept after a later admission",
    decide(request()).exempt,
    true
  )
  check_equal(
    "admitted: no refusal",
    status(o),
    null
  )
  check_equal(
    "nothing released at admission",
    released(),
    []
  )
  release = {duration = 95., bytes_sent = 10, listeners = 4, metadata = []}
  on_release = null.get(o.on_release)
  on_release(release)
  check_equal(
    "every on_release called, in order",
    released(),
    [("c", 95., 4), ("a", 95., 4), ("b", 95., 4)]
  )

  released := []
  decide = icecast.server.decision.all([a, refuse(401), counted])
  o = decide(request())
  check_equal(
    "first refusal wins",
    status(o),
    401
  )
  check_equal(
    "decisions after a refusal not called",
    calls(),
    0
  )
  check_equal(
    "earlier admission released with duration 0, listeners without it",
    released(),
    [("a", 0., 3)]
  )
  check_equal(
    "refusal has no on_release",
    null.defined(o.on_release),
    false
  )
  check_equal(
    "refusal has no time limit",
    o.time_limit,
    null
  )

  released := []
  def raises(_) =
    error.raise(error.invalid, "boom")
    counted(())
  end
  failed =
    try
      decide = icecast.server.decision.all([a, raises])
      ignore(decide(request()))
      false
    catch _ do
      true
    end
  check_equal(
    "a raising decision raises",
    failed,
    true
  )
  check_equal(
    "earlier admission released when a later one raises",
    released(),
    [("a", 0., 3)]
  )

  decide = icecast.server.decision.all([])
  o = decide(request())
  check_equal(
    "empty composition admits",
    status(o),
    null
  )
  check_equal(
    "empty composition: no release",
    null.defined(o.on_release),
    false
  )
end

def glob_matches(glob, value) =
  list.exists(fun (r) -> r.test(value), icecast.server.access.pattern(glob))
end

def check_access_lists(directory) =
  deny = path.concat(directory, "deny")
  allow = path.concat(directory, "allow")
  agents = path.concat(directory, "agents")
  file.write(
    data="# comment\n\n192.0.2.1\n10.0.*\n",
    deny
  )
  file.write(data="192.0.2.*\n10.0.0.[1-3]\n", allow)
  file.write(data="*Bot*\n", agents)
  decide =
    icecast.server.decision.access_lists(
      check_interval=0.,
      deny=deny,
      allow=allow,
      agents=agents
    )
  def outcome(~agent=null, address) =
    headers = null.defined(agent) ? [("user-agent", null.get(agent))] : []
    status(decide(request(headers=headers, address=address)))
  end

  check_equal(
    "deny list refuses",
    outcome("192.0.2.1"),
    403
  )
  check_equal(
    "deny beats allow",
    outcome("10.0.0.2"),
    403
  )
  check_equal(
    "allow list admits",
    outcome("192.0.2.9"),
    null
  )
  check_equal(
    "allow list closes",
    outcome("198.51.100.1"),
    403
  )
  check_equal(
    "agent refused without case",
    outcome(agent="GoogleBOT/2", "192.0.2.9"),
    403
  )
  check_equal(
    "other agent admitted",
    outcome(agent="VLC/3.0", "192.0.2.9"),
    null
  )

  only_deny = icecast.server.decision.access_lists(check_interval=0., deny=deny)
  check_equal(
    "no allow list admits the rest",
    status(only_deny(request(address="198.51.100.1"))),
    null
  )
  check_equal(
    "refusal has no challenge",
    null.get(only_deny(request(address="192.0.2.1")).refuse).headers,
    []
  )

  check_equal(
    "comment line ignored",
    status(
      only_deny(
        request(
          address="# comment"
        )
      )
    ),
    null
  )

  # Two writes can share an mtime tick: give the new one its own.
  file.write(data="198.51.100.1\n", deny)
  ignore(
    process.run(
      "touch -d @1000000000 #{process.quote(deny)}"
    )
  )
  check_equal(
    "changed file read again",
    status(only_deny(request(address="198.51.100.1"))),
    403
  )
  check_equal(
    "old entry gone",
    status(only_deny(request(address="192.0.2.1"))),
    null
  )

  check_equal(
    "pattern literal dot",
    glob_matches("10.0.0.1", "10x0x0x1"),
    false
  )
  check_equal(
    "pattern negated class",
    glob_matches("10.0.0.[!1]", "10.0.0.1"),
    false
  )
  check_equal(
    "pattern question mark",
    glob_matches("10.0.0.?", "10.0.0.7"),
    true
  )
end

def check_htpasswd(directory) =
  users = path.concat(directory, "htpasswd")
  file.write(
    data="# users\nbob:#{string.digest("secret")}\ncarol:$6$salt$hash\n",
    users
  )
  decide = icecast.server.decision.htpasswd(users)
  def outcome(~compatibility="icecast-2.5", ~user, password) =
    decide(request(compatibility=compatibility, user=user, password=password))
  end

  check_equal(
    "right password admits",
    status(outcome(user="bob", "secret")),
    null
  )
  check_equal(
    "MD5 under icecast-2.4",
    status(outcome(compatibility="icecast-2.4", user="bob", "secret")),
    null
  )
  check_equal(
    "wrong password",
    status(outcome(user="bob", "nope")),
    401
  )
  check_equal(
    "unknown user",
    status(outcome(user="dave", "secret")),
    401
  )
  check_equal(
    "no credentials",
    status(outcome(user="", "")),
    401
  )
  check_equal(
    "401 carries the default challenge",
    null.get(outcome(user="bob", "nope").refuse).headers,
    [
      (
        "WWW-Authenticate",
        "Basic realm=\"Icecast2 Server\""
      )
    ]
  )
  check_equal(
    "crypt hash never matches",
    status(outcome(user="carol", "hash")),
    401
  )

  composed =
    icecast.server.decision.all(
      [decide, icecast.server.connections_per_user(1)]
    )
  check_equal(
    "wrong password is 401 while the user is connected",
    status(composed(request(user="bob", password="nope", user_listeners=1))),
    401
  )
end

def check_connections_per_user() =
  def outcome(~compatibility="icecast-2.5", ~user="bob", n, count) =
    decide = icecast.server.connections_per_user(n)
    status(
      decide(
        request(
          compatibility=compatibility,
          user=user,
          password="x",
          user_listeners=count
        )
      )
    )
  end

  check_equal(
    "under the limit",
    outcome(2, 1),
    null
  )
  check_equal(
    "at the limit: 429 under icecast-2.5",
    outcome(2, 2),
    429
  )
  check_equal(
    "429 under rsas",
    outcome(compatibility="rsas", 1, 1),
    429
  )
  check_equal(
    "403 under icecast-2.4",
    outcome(compatibility="icecast-2.4", 1, 1),
    403
  )
  check_equal(
    "403 under kh",
    outcome(compatibility="kh", 1, 3),
    403
  )
  check_equal(
    "0 is no limit",
    outcome(0, 5),
    null
  )
  check_equal(
    "no user never limited",
    outcome(user="", 1, 0),
    null
  )
end

def check_replies() =
  cache =
    {backend = fun (location) -> "LOADED:#{location}", preroll_max_size = 8}
  def outcome(
    ~compatibility="icecast-2.5",
    ~options={},
    ~body="",
    status,
    headers
  ) =
    icecast.server.url_auth.outcome(
      cache=cache,
      config=icecast.server.url_auth.config(compatibility, options),
      request(compatibility=compatibility, user="bob", password="pw"),
      {status = status, headers = headers, body = body}
    )
  end

  def code(outcome) =
    status(outcome)
  end

  check_equal(
    "old style admits on 200",
    code(outcome(200, [("icecast-auth-user", "1")])),
    null
  )
  check_equal(
    "old style admits whatever the status",
    code(
      outcome(
        500,
        [
          (
            "icecast-auth-user",
            " 1 "
          )
        ]
      )
    ),
    null
  )
  check_equal(
    "new style admits on 2xx",
    code(outcome(204, [("x-icecast-auth-result", "OK")])),
    null
  )
  check_equal(
    "new style needs 2xx",
    code(outcome(403, [("x-icecast-auth-result", "ok")])),
    401
  )
  check_equal(
    "nothing refuses with 401",
    code(outcome(200, [])),
    401
  )
  check_equal(
    "401 has the default challenge",
    null.get(outcome(200, []).refuse).headers,
    [
      (
        "WWW-Authenticate",
        "Basic realm=\"Icecast2 Server\""
      )
    ]
  )
  check_equal(
    "auth_header disables the new style",
    code(
      outcome(
        options={
          auth_header =
            "x-ok: yes"
        },
        200,
        [("x-icecast-auth-result", "ok")]
      )
    ),
    401
  )
  check_equal(
    "auth_header admits by its line, without case",
    code(
      outcome(
        options={
          auth_header =
            "X-OK: Yes"
        },
        200,
        [("x-ok", "yes")]
      )
    ),
    null
  )
  check_equal(
    "header_auth disables the old style",
    code(
      outcome(
        options={header_auth = "x-result"},
        200,
        [("icecast-auth-user", "1")]
      )
    ),
    401
  )
  check_equal(
    "withintro does not admit outside kh",
    code(outcome(200, [("icecast-auth-user", "withintro")])),
    401
  )
  check_equal(
    "kh: any value but 0 admits",
    code(outcome(compatibility="kh", 200, [("icecast-auth-user", "yes")])),
    null
  )
  check_equal(
    "kh: 0 refuses",
    code(outcome(compatibility="kh", 200, [("icecast-auth-user", "0")])),
    401
  )

  admitted = [("icecast-auth-user", "1")]
  check_equal(
    "time limit",
    outcome(200, [...admitted, ("icecast-auth-timelimit", "1800")]).time_limit,
    1800.
  )
  check_equal(
    "time limit 0",
    outcome(200, [...admitted, ("icecast-auth-timelimit", "0")]).time_limit,
    0.
  )
  check_equal(
    "invalid time limit ignored",
    outcome(200, [...admitted, ("icecast-auth-timelimit", "soon")]).time_limit,
    null
  )
  check_equal(
    "a time limit too large for an integer is ignored",
    outcome(
      200,
      [...admitted, ("icecast-auth-timelimit", "99999999999999999999")]
    ).time_limit,
    null
  )
  check_equal(
    "kh: invalid time limit is 60",
    outcome(
      compatibility="kh",
      200,
      [...admitted, ("icecast-auth-timelimit", "-5")]
    ).time_limit,
    60.
  )
  check_equal(
    "new-style time limit on 2xx",
    outcome(200, [...admitted, ("x-icecast-auth-timelimit", "30")]).time_limit,
    30.
  )
  check_equal(
    "old-style time limit whatever the status",
    outcome(500, [...admitted, ("icecast-auth-timelimit", "7")]).time_limit,
    7.
  )
  check_equal(
    "timelimit_header with trailing colon",
    outcome(
      options={timelimit_header = "x-limit:"},
      200,
      [...admitted, ("x-limit", "9")]
    ).time_limit,
    9.
  )

  def redirect(outcome) =
    refuse = null.get(outcome.refuse)
    (refuse.status, list.assoc(default="", "Location", refuse.headers))
  end
  alter = [("x-icecast-auth-alter-argument", "http://other/")]
  check_equal(
    "alter redirect is 303",
    redirect(
      outcome(
        200,
        [...admitted, ("x-icecast-auth-alter-action", "redirect"), ...alter]
      )
    ),
    (303, "http://other/")
  )
  check_equal(
    "redirect_see_other",
    redirect(
      outcome(
        200,
        [("x-icecast-auth-alter-action", "redirect_see_other"), ...alter]
      )
    ),
    (303, "http://other/")
  )
  check_equal(
    "redirect_temporary",
    redirect(
      outcome(
        200,
        [("x-icecast-auth-alter-action", "redirect_temporary"), ...alter]
      )
    ),
    (307, "http://other/")
  )
  check_equal(
    "redirect_permanent",
    redirect(
      outcome(
        200,
        [("x-icecast-auth-alter-action", "redirect_permanent"), ...alter]
      )
    ),
    (308, "http://other/")
  )
  check_equal(
    "alter redirect needs 2xx",
    code(
      outcome(
        500,
        [...admitted, ("x-icecast-auth-alter-action", "redirect"), ...alter]
      )
    ),
    null
  )
  check_equal(
    "noop does nothing",
    code(
      outcome(
        200,
        [...admitted, ("x-icecast-auth-alter-action", "noop"), ...alter]
      )
    ),
    null
  )
  check_equal(
    "action without argument ignored",
    code(
      outcome(200, [...admitted, ("x-icecast-auth-alter-action", "redirect")])
    ),
    null
  )
  check_equal(
    "auth redirect is 303",
    redirect(outcome(200, [("icecast-auth-redirect", "http://r/")])),
    (303, "http://r/")
  )
  check_equal(
    "auth redirect ignored when admitted",
    code(outcome(200, [...admitted, ("icecast-auth-redirect", "http://r/")])),
    null
  )
  check_equal(
    "Location ignored outside kh",
    code(outcome(200, [...admitted, ("location", "http://l/")])),
    null
  )
  check_equal(
    "kh: Location redirects with 302 first",
    redirect(
      outcome(
        compatibility="kh",
        200,
        [
          ...admitted,
          ("location", "http://l/"),
          ("x-icecast-auth-alter-action", "redirect"),
          ...alter
        ]
      )
    ),
    (302, "http://l/")
  )
  check_equal(
    "redirect discards the time limit",
    outcome(
      200,
      [("icecast-auth-redirect", "http://r/"), ("icecast-auth-timelimit", "5")]
    ).time_limit,
    null
  )

  check_equal(
    "resp status",
    code(
      outcome(
        200,
        [("icecast-resp-status", "402"), ("icecast-resp-reason", "Pay")]
      )
    ),
    402
  )
  check_equal(
    "resp reason",
    null.get(
      outcome(
        200,
        [("icecast-resp-status", "402"), ("icecast-resp-reason", "Pay")]
      ).refuse
    ).reason,
    "Pay"
  )
  check_equal(
    "resp status out of range ignored",
    code(outcome(200, [("icecast-resp-status", "200")])),
    401
  )
  check_equal(
    "403 gets no default challenge",
    null.get(outcome(200, [("icecast-resp-status", "403")]).refuse).headers,
    []
  )
  check_equal(
    "backend challenge",
    null.get(
      outcome(
        200,
        [
          (
            "icecast-www-authenticate",
            "Basic realm=\"x\""
          )
        ]
      ).refuse
    ).headers,
    [
      (
        "WWW-Authenticate",
        "Basic realm=\"x\""
      )
    ]
  )
  check_equal(
    "403 reply is 401 outside kh",
    code(outcome(403, [])),
    401
  )
  check_equal(
    "kh: 403 reply refuses with 403",
    code(outcome(compatibility="kh", 403, [])),
    403
  )
  kh_message =
    outcome(
      compatibility="kh",
      200,
      [
        (
          "icecast-auth-message",
          "403 Go away"
        )
      ]
    )
  check_equal(
    "kh: 403 message",
    code(kh_message),
    403
  )
  check_equal(
    "kh: 403 message reason",
    null.get(kh_message.refuse).reason,
    "Go away"
  )
  check_equal(
    "kh: a valid icecast-resp-status beats the 403 rule",
    code(outcome(compatibility="kh", 403, [("icecast-resp-status", "402")])),
    402
  )
  check_equal(
    "kh: 403 rule reads icecast-auth-message only",
    code(
      outcome(
        compatibility="kh",
        200,
        [
          (
            "x-icecast-auth-message",
            "403 Go away"
          )
        ]
      )
    ),
    401
  )

  check_equal(
    "preroll header loaded by the cache",
    outcome(200, [...admitted, ("preroll", "http://ad/")]).preroll,
    "LOADED:http://ad/"
  )
  check_equal(
    "no preroll header",
    outcome(200, admitted).preroll,
    null
  )
  check_equal(
    "refusal loads no preroll",
    outcome(200, [("preroll", "http://ad/")]).preroll,
    null
  )
  check_equal(
    "kh: withintro body is the preroll",
    outcome(
      compatibility="kh",
      body="INTRO",
      200,
      [("icecast-auth-user", "withintro"), ("preroll", "http://ad/")]
    ).preroll,
    "INTRO"
  )
  check_equal(
    "kh: body above preroll_max_size is empty",
    outcome(
      compatibility="kh",
      body="TOO LONG BODY",
      200,
      [("icecast-auth-user", "withintro")]
    ).preroll,
    ""
  )
  check_equal(
    "kh: preroll header without withintro",
    outcome(
      compatibility="kh",
      200,
      [...admitted, ("preroll", "http://ad/")]
    ).preroll,
    "LOADED:http://ad/"
  )

  check_equal(
    "icecast-slave exempts",
    outcome(200, [...admitted, ("icecast-slave", "1")]).exempt,
    true
  )
  check_equal(
    "icecast-slave 0 does not",
    outcome(200, [...admitted, ("icecast-slave", "0")]).exempt,
    false
  )
  check_equal(
    "ice-username renames",
    outcome(200, [...admitted, ("ice-username", "alice")]).user,
    "alice"
  )
  check_equal(
    "no rename without ice-username",
    outcome(200, admitted).user,
    null
  )

  config = icecast.server.url_auth.config("kh", {})
  check_equal(
    "kh header_prefix",
    config.header_prefix,
    "ClientHeader-"
  )
  check_equal(
    "kh on_error_wait",
    config.on_error_wait,
    30.
  )
  config =
    icecast.server.url_auth.config(
      "icecast-2.4",
      {client_add = "http://a/", client_remove = "http://r/"}
    )
  check_equal(
    "client_add alias",
    config.listener_add,
    "http://a/"
  )
  check_equal(
    "client_remove alias",
    config.listener_remove,
    "http://r/"
  )
  check_equal(
    "default header_prefix",
    config.header_prefix,
    ""
  )
  check_equal(
    "default on_error_wait",
    config.on_error_wait,
    0.
  )
  check_equal(
    "default timeout",
    config.timeout,
    6.
  )
end

def check_rename() =
  seen = ref([])
  def rename(_) =
    {refuse = null, user = ("alice" : string?)}
  end

  def record_user(request) =
    seen := [...seen(), request.user]
    {refuse = null, user = ("carol" : string?)}
  end

  def user_listeners(user) =
    user == "alice" ? 1 : 0
  end

  r =
    icecast.server.decision.request(
      server={hostname = "radio.example", port = 8000, instance = "uuid"},
      compatibility="icecast-2.5",
      address="203.0.113.7",
      listeners=3,
      user_listeners=user_listeners,
      listener(user="bob", password="pw")
    )
  decide = icecast.server.decision.all([rename, record_user])
  o = decide(r)
  check_equal(
    "later decision sees the renamed user",
    seen(),
    ["alice"]
  )
  check_equal(
    "first rename wins",
    o.user,
    "alice"
  )
  limited =
    icecast.server.decision.all(
      [rename, icecast.server.connections_per_user(1)]
    )
  check_equal(
    "per-user limit counts the renamed user",
    status(limited(r)),
    429
  )
  unrenamed =
    icecast.server.decision.all([icecast.server.connections_per_user(1)])
  check_equal(
    "original user under the limit",
    status(unrenamed(r)),
    null
  )

  def refuse_only(_) =
    {refuse = icecast.server.refusal(403)}
  end

  def full(_) =
    {
      refuse = null,
      time_limit = 5.,
      preroll = "P",
      exempt = true,
      user = (null : string?),
      on_release = fun (_) -> ()
    }
  end

  mixed = icecast.server.decision.all([full, refuse_only])
  check_equal(
    "mixed shapes: refusal",
    status(mixed(r)),
    403
  )
  mixed = icecast.server.decision.all([refuse_only, full])
  check_equal(
    "mixed shapes: refusal first",
    status(mixed(r)),
    403
  )
  admits = icecast.server.decision.all([fun (_) -> {refuse = null}, full])
  check_equal(
    "mixed shapes: merged time limit",
    admits(r).time_limit,
    5.
  )
end

def check_unreadable(directory) =
  missing = path.concat(directory, "missing")
  allow = icecast.server.decision.access_lists(check_interval=0., allow=missing)
  deny = icecast.server.decision.access_lists(check_interval=0., deny=missing)
  check_equal(
    "missing allow list refuses",
    status(allow(request())),
    403
  )
  check_equal(
    "missing allow list refuses again",
    status(allow(request())),
    403
  )
  check_equal(
    "missing deny list refuses nobody",
    status(deny(request())),
    null
  )
  log.flush()
  logged =
    list.length(
      list.filter(
        fun (line) ->
          string.contains(
            substring="Cannot read #{missing}",
            line
          ),
        file.lines(log_path)
      )
    )
  check_equal(
    "an unreadable file is logged once per list",
    logged,
    2
  )

  bad = path.concat(directory, "bad")
  file.write(data="10.0.0.[1\n192.0.2.1\n", bad)
  decide = icecast.server.decision.access_lists(check_interval=0., deny=bad)
  check_equal(
    "bad pattern skipped, next line kept",
    status(decide(request(address="192.0.2.1"))),
    403
  )
  file.remove(bad)
  check_equal(
    "a file gone keeps its last entries",
    status(decide(request(address="192.0.2.1"))),
    403
  )
end

def f() =
  directory = file.temp_dir(cleanup=true, "icecast-decisions")
  check_request()
  check_all()
  check_access_lists(directory)
  check_htpasswd(directory)
  check_connections_per_user()
  check_replies()
  check_rename()
  check_unreadable(directory)
  if
    checks() < 125
  then
    test.fail(
      "only #{checks()} checks ran"
    )
  end
  print(
    "#{checks()} checks ran"
  )
  test.pass()
end

test.check(f)
