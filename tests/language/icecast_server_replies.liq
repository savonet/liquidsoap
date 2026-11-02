%include "../test.liq"
%include "../../src/libs/extra/icecast_server/settings.liq"
%include "../../src/libs/extra/icecast_server/headers.liq"
%include "../../src/libs/extra/icecast_server/refusals.liq"

checks = ref(0)

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

def request(~origin=null, compatibility) =
  {
    headers = null.defined(origin) ? [("origin", null.get(origin))] : [],
    compatibility = compatibility
  }
end

def reply(~origin=null, ~compatibility="icecast-2.5", status, entries) =
  icecast.server.headers.reply(
    request=request(origin=origin, compatibility),
    status=status,
    entries
  )
end

def check_refusals() =
  challenge =
    (
      "WWW-Authenticate",
      "Basic realm=\"Icecast2 Server\""
    )
  check_equal(
    "401",
    icecast.server.refusal(401),
    {
      status = 401,
      reason =
        "Authentication Required",
      headers = [challenge]
    }
  )
  chosen = ("www-authenticate", "Bearer")
  check_equal(
    "401 with a chosen challenge",
    icecast.server.refusal(headers=[chosen], 401).headers,
    [chosen]
  )
  check_equal(
    "403",
    icecast.server.refusal(403),
    {status = 403, reason = "Forbidden", headers = []}
  )
  check_equal(
    "403 with a challenge",
    icecast.server.refusal(headers=[chosen], 403).headers,
    [chosen]
  )
  check_equal(
    "reason",
    icecast.server.refusal(
      reason="Go away",
      451
    ).reason,
    "Go away"
  )
  check_equal(
    "redirect",
    icecast.server.redirect(307, "http://h/b"),
    {
      status = 307,
      reason =
        "Temporary Redirect",
      headers = [("Location", "http://h/b")]
    }
  )

  list.iter(
    fun (row) ->
      check_equal(
        "limit under #{fst(row)}",
        icecast.server.refusal.limit(fst(row)).status,
        snd(row)
      ),
    [("icecast-2.5", 503), ("icecast-2.4", 403), ("kh", 403), ("rsas", 503)]
  )
end

def check_precedence() =
  calls = ref([])
  def compute(name, value) =
    {
      name = name,
      compute =
        fun (request, status) ->
          begin
            calls := [(request.compatibility, status), ...calls()]
            value
          end
    }
  end

  decision = icecast.server.headers.of_pairs([("X-A", "decision")])
  identity =
    icecast.server.headers.of_pairs([("icy-name", "Live"), ("x-a", "identity")])
  server =
    [
      {name = "ICY-NAME", value = "server"},
      compute("X-B", "server"),
      {name = "X-C", value = "server"}
    ]
  mount =
    [
      {name = "x-b", value = "mount"},
      compute("X-C", "mount"),
      {name = "X-D", value = "mount"}
    ]
  check_equal(
    "first name wins",
    reply(
      200,
      [
        ...decision,
        ...identity,
        ...list.map(icecast.server.headers.entry, server),
        ...list.map(icecast.server.headers.entry, mount)
      ]
    ),
    [
      ("X-A", "decision"),
      ("icy-name", "Live"),
      ("X-B", "server"),
      ("X-C", "server"),
      ("X-D", "mount")
    ]
  )
  check_equal(
    "compute input",
    list.hd(calls()),
    ("icecast-2.5", 200)
  )

  check_equal(
    "null computes let a later entry win",
    reply(200, [compute("X-A", null), {name = "x-a", value = "later"}]),
    [("x-a", "later")]
  )
  check_equal(
    "status filter",
    reply(
      404,
      [{name = "X-A", value = "ok", status = 200}, {name = "X-A", value = "any"}
      ]
    ),
    [("X-A", "any")]
  )
  check_equal(
    "status filter applies",
    reply(
      200,
      [{name = "X-A", value = "ok", status = 200}, {name = "X-A", value = "any"}
      ]
    ),
    [("X-A", "ok")]
  )
  check_equal(
    "computed value with a line break is dropped",
    reply(
      200,
      [
        compute(
          "X-A",
          "a\r\nX-Injected: 1"
        ),
        {name = "X-A", value = "safe"}
      ]
    ),
    [("X-A", "safe")]
  )
  check_equal(
    "computed header with an invalid name is dropped",
    reply(
      200,
      [
        compute(
          "X A",
          "v"
        )
      ]
    ),
    []
  )
  check_equal(
    "computed value",
    reply(200, [compute("X-A", "v")]),
    [("X-A", "v")]
  )
  raising =
    {
      name = "X-A",
      compute =
        fun (_, _) ->
          error.raise(
            error.invalid,
            "no value"
          )
    }
  check_equal(
    "raising compute is dropped",
    reply(200, [raising, {name = "x-a", value = "later"}]),
    [("x-a", "later")]
  )
end

def check_cors() =
  cors = icecast.server.headers.cors()
  icecast_2xx =
    [
      ("Access-Control-Allow-Origin", "http://o"),
      ("Access-Control-Allow-Methods", "GET"),
      (
        "Access-Control-Expose-Headers",
        "content-range, icy-br, icy-description, icy-genre, icy-name, icy-pub, \
         icy-url"
      ),
      ("Access-Control-Max-Age", "300"),
      (
        "Access-Control-Allow-Headers",
        "range, if-range"
      )
    ]
  icecast_error =
    [
      ("Access-Control-Allow-Origin", "null"),
      (
        "Access-Control-Expose-Headers",
        "content-range, icy-br, icy-description, icy-genre, icy-name, icy-pub, \
         icy-url"
      ),
      ("Access-Control-Max-Age", "300"),
      (
        "Access-Control-Allow-Headers",
        "range, if-range"
      )
    ]
  list.iter(
    fun (compatibility) ->
      begin
        check_equal(
          "#{compatibility} 200",
          reply(origin="http://o", compatibility=compatibility, 200, cors),
          icecast_2xx
        )
        check_equal(
          "#{compatibility} 404",
          reply(origin="http://o", compatibility=compatibility, 404, cors),
          icecast_error
        )
        check_equal(
          "#{compatibility} 503",
          reply(origin="http://o", compatibility=compatibility, 503, cors),
          icecast_error
        )
        check_equal(
          "#{compatibility} 302",
          reply(origin="http://o", compatibility=compatibility, 302, cors),
          list.tl(icecast_error)
        )
        check_equal(
          "#{compatibility} without Origin",
          reply(compatibility=compatibility, 200, cors),
          []
        )
      end,
    ["icecast-2.5", "icecast-2.4", "kh"]
  )

  rsas_expose =
    "Content-Type, icy-metaint, icy-br, ice-audio-info, icy-description, \
     icy-genre, icy-name, icy-samplerate, icy-url"
  check_equal(
    "rsas 404",
    reply(origin="http://o", compatibility="rsas", 404, cors),
    [
      ("Access-Control-Allow-Origin", "http://o"),
      ("Access-Control-Expose-Headers", rsas_expose)
    ]
  )
  check_equal(
    "rsas without Origin",
    reply(compatibility="rsas", 200, cors),
    [("Access-Control-Expose-Headers", rsas_expose)]
  )

  restricted =
    icecast.server.headers.cors(origins=["http://a"], expose=["x-extra"])
  list.iter(
    fun (compatibility) ->
      begin
        allowed =
          reply(origin="http://a", compatibility=compatibility, 200, restricted)
        other =
          reply(origin="http://b", compatibility=compatibility, 200, restricted)
        check_equal(
          "#{compatibility} allowed origin",
          list.assoc.nullable("Access-Control-Allow-Origin", allowed),
          "http://a"
        )
        check_equal(
          "#{compatibility} other origin",
          list.assoc.nullable("Access-Control-Allow-Origin", other),
          null
        )
        expose = list.assoc(default="", "Access-Control-Expose-Headers", other)
        check_equal(
          "#{compatibility} expose appended",
          string.contains(
            suffix=", x-extra",
            expose
          ),
          true
        )
      end,
    ["icecast-2.5", "icecast-2.4", "kh", "rsas"]
  )
end

def f() =
  check_refusals()
  check_precedence()
  check_cors()
  if
    checks() < 40
  then
    test.fail(
      "only #{checks()} checks ran"
    )
  end
  test.pass()
end

test.check(f)
