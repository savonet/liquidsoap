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

# The URL authentication wire exchange against a fake backend: the request
# fields and credentials, failures, `on_error_wait` and the `listener_remove`
# notification. Port 18154 has no listener, for connection failures.
port = 18150
base = "http://127.0.0.1:#{port}"
closed = "http://127.0.0.1:18154/auth"
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

received = ref([])
reply = ref({status = 200, headers = [("icecast-auth-user", "1")], delay = 0.})

def backend(request, response) =
  r = reply()
  entry =
    {
      path = request.path,
      headers =
        list.map(
          fun (h) -> (string.case(lower=true, fst(h)), snd(h)),
          request.headers
        ),
      fields = url.split_args(request.body())
    }
  atomic(fun () -> received := [...received(), entry])
  if r.delay > 0. then thread.pause(r.delay) end
  response.status_code(r.status)
  list.iter(fun (h) -> response.header(fst(h), snd(h)), r.headers)
  response.data("")
end
harbor.http.register(port=port, method="POST", "/auth", backend)
harbor.http.register(port=port, method="POST", "/remove", backend)
harbor.http.register(
  port=port,
  method="GET",
  "/ad",
  fun (_, response) -> response.data("AD")
)

def requests_to(path) =
  list.filter(fun (entry) -> entry.path == path, received())
end

def last_request() =
  list.last(received())
end

def wait_for(label, path, count) =
  deadline = time() + 10.
  while list.length(requests_to(path)) < count and time() < deadline do
    thread.pause(0.05)

  end
  check_equal(label, list.length(requests_to(path)), count)
end

cache = icecast.server.preroll_cache()
instance = "0f7c2d9e-6a41-4b0e-9b52-1c3a8e5d7f10"

def request(~compatibility="icecast-2.5", ~headers=[], ~user="", ~password="") =
  authorization =
    user == "" and password == ""
      ? []
      : [
        (
          "authorization",
          "Basic #{string.base64.encode("#{user}:#{password}")}"
        )
      ]
  icecast.server.decision.request(
    server={hostname = "radio.example", port = 8000, instance = instance},
    compatibility=compatibility,
    address="203.0.113.7",
    listeners=3,
    user_listeners=fun (_) -> 0,
    {
      id = 42,
      uri = "/live.mp3",
      query_string = "token=x",
      protocol = "1.1",
      headers = [...authorization, ("user-agent", "VLC/3.0"), ...headers]
    }
  )
end

def status(outcome) =
  null.map(fun (refuse) -> refuse.status, outcome.refuse)
end

def authorization(entry) =
  list.assoc(default="", "authorization", entry.headers)
end

def basic(user, password) =
  "Basic #{string.base64.encode("#{user}:#{password}")}"
end

def check_worked_example() =
  reply :=
    {
      status = 200,
      headers =
        [
          ("Icecast-Auth-User", "1"),
          ("icecast-auth-timelimit", "1800"),
          ("preroll", "#{base}/ad")
        ],
      delay = 0.
    }
  decide =
    icecast.server.decision.url_auth(
      cache=cache,
      {listener_add = "#{base}/auth", username = "radio", password = "s3cret"}
    )
  outcome = decide(request())
  check_equal(
    "example: admitted",
    status(outcome),
    null
  )
  check_equal(
    "example: time limit",
    outcome.time_limit,
    1800.
  )
  check_equal(
    "example: backend preroll loaded",
    outcome.preroll,
    "AD"
  )
  entry = last_request()
  check_equal(
    "example: content type",
    list.assoc(default="", "content-type", entry.headers),
    "application/x-www-form-urlencoded"
  )
  check_equal(
    "example: option credentials",
    authorization(entry),
    "Basic cmFkaW86czNjcmV0"
  )
  check_equal(
    "example: fields and order",
    entry.fields,
    [
      ("action", "listener_add"),
      ("server", "radio.example"),
      ("port", "8000"),
      ("server-instance", instance),
      ("client", "42"),
      ("mount", "/live.mp3?token=x"),
      ("user", ""),
      ("pass", ""),
      ("ip", "203.0.113.7"),
      ("agent", "VLC/3.0"),
      ("protocol", "http"),
      ("referrer", ""),
      ("listener-count", "3"),
      ("referer", ""),
      ("listeners", "3")
    ]
  )
end

def check_credentials() =
  reply := {status = 200, headers = [("icecast-auth-user", "1")], delay = 0.}
  def sent(~user="", ~password="", options) =
    decide = icecast.server.decision.url_auth(cache=cache, options)
    ignore(decide(request(user=user, password=password)))
    last_request()
  end

  entry = sent(user="bob", password="pw", {listener_add = "#{base}/auth"})
  check_equal(
    "listener credentials",
    authorization(entry),
    basic("bob", "pw")
  )
  check_equal(
    "user field",
    list.assoc(default="", "user", entry.fields),
    "bob"
  )
  check_equal(
    "pass field",
    list.assoc(default="", "pass", entry.fields),
    "pw"
  )
  entry =
    sent(
      user="bob",
      password="pw",
      {listener_add = "#{base}/auth", username = "radio"}
    )
  check_equal(
    "username alone: listener credentials",
    authorization(entry),
    basic("bob", "pw")
  )
  entry =
    sent(
      user="bob",
      password="pw",
      {
        listener_add = "http://u:p@127.0.0.1:#{port}/auth",
        username = "radio",
        password = "s3cret"
      }
    )
  check_equal(
    "URL credentials only",
    authorization(entry),
    basic("u", "p")
  )
  entry = sent({listener_add = "#{base}/auth"})
  check_equal(
    "no credentials",
    authorization(entry),
    ""
  )

  headers = [("x-token", "abc"), ("referer", "http://page/")]
  decide =
    icecast.server.decision.url_auth(
      cache=cache,
      {
        listener_add = "#{base}/auth",
        headers =
          "X-Token, Missing"
      }
    )
  ignore(decide(request(headers=headers)))
  fields = last_request().fields
  check_equal(
    "forwarded header after the fixed fields",
    list.nth(default=("", ""), fields, 15),
    ("X-Token", "abc")
  )
  check_equal(
    "absent header not forwarded",
    list.length(fields),
    16
  )
  check_equal(
    "referrer",
    list.assoc(default="", "referrer", fields),
    "http://page/"
  )
  check_equal(
    "referer",
    list.assoc(default="", "referer", fields),
    "http://page/"
  )
  ignore(decide(request(compatibility="kh", headers=headers)))
  check_equal(
    "kh header prefix",
    list.assoc(default="", "ClientHeader-X-Token", last_request().fields),
    "abc"
  )
end

def check_failures() =
  challenge =
    [
      (
        "WWW-Authenticate",
        "Basic realm=\"Icecast2 Server\""
      )
    ]
  def decide(~compatibility="icecast-2.5", options) =
    d = icecast.server.decision.url_auth(cache=cache, options)
    d(request(compatibility=compatibility))
  end

  outcome = decide({listener_add = closed})
  check_equal(
    "unreachable backend refuses",
    status(outcome),
    401
  )
  check_equal(
    "failure has the default challenge",
    null.get(outcome.refuse).headers,
    challenge
  )
  outcome = decide({listener_add = closed, presume_innocent = true})
  check_equal(
    "presume_innocent admits on failure",
    status(outcome),
    null
  )
  check_equal(
    "presumed admission has no time limit",
    outcome.time_limit,
    null
  )

  reply :=
    {status = 500, headers = [("icecast-auth-timelimit", "5")], delay = 0.}
  outcome = decide({listener_add = "#{base}/auth", presume_innocent = true})
  check_equal(
    "presume_innocent admits a 5xx reply",
    status(outcome),
    null
  )
  check_equal(
    "presumed 5xx has no time limit",
    outcome.time_limit,
    null
  )
  outcome = decide({listener_add = "#{base}/auth"})
  check_equal(
    "5xx without admission refuses",
    status(outcome),
    401
  )

  reply := {status = 200, headers = [("icecast-auth-user", "1")], delay = 3.}
  outcome = decide({listener_add = "#{base}/auth", timeout = 0.5})
  check_equal(
    "timeout refuses",
    status(outcome),
    401
  )

  reply :=
    {
      status = 302,
      headers = [("icecast-auth-user", "1"), ("Location", "http://elsewhere/")],
      delay = 0.
    }
  outcome = decide({listener_add = "#{base}/auth"})
  check_equal(
    "backend redirect is a failure",
    status(outcome),
    401
  )
  outcome = decide(compatibility="kh", {listener_add = "#{base}/auth"})
  check_equal(
    "kh: Location redirects",
    null.map(fun (refuse) -> (refuse.status, refuse.headers), outcome.refuse),
    (302, [("Location", "http://elsewhere/")])
  )

  reply := {status = 301, headers = [], delay = 0.}
  def waits(~compatibility="icecast-2.5", options) =
    d = icecast.server.decision.url_auth(cache=cache, options)
    ignore(d(request(compatibility=compatibility)))
    before = list.length(requests_to("/auth"))
    reply := {status = 200, headers = [("icecast-auth-user", "1")], delay = 0.}
    outcome = d(request(compatibility=compatibility))
    reply := {status = 301, headers = [], delay = 0.}
    {
      skipped = list.length(requests_to("/auth")) == before,
      status = status(outcome)
    }
  end

  transport_failure =
    try
      ignore(
        icecast.server.url_auth.send(
          config=icecast.server.url_auth.config("icecast-2.5", {timeout = 2.}),
          fields=[],
          {user = "", password = ""},
          closed
        )
      )
      "no error"
    catch err : [error.http] do
      "error.http"
    end
  check_equal(
    "a transport failure is an error, so on_error_wait starts",
    transport_failure,
    "error.http"
  )
  waited = waits({listener_add = "#{base}/auth", on_error_wait = 60.})
  check_equal(
    "on_error_wait: no request during the wait",
    waited.skipped,
    true
  )
  check_equal(
    "on_error_wait: failure outcome meanwhile",
    waited.status,
    401
  )
  waited =
    waits(
      {
        listener_add = "#{base}/auth",
        on_error_wait = 60.,
        presume_innocent = true
      }
    )
  check_equal(
    "on_error_wait with presume_innocent admits",
    waited.status,
    null
  )
  waited = waits(compatibility="kh", {listener_add = "#{base}/auth"})
  check_equal(
    "kh: 30 s wait by default",
    waited.skipped,
    true
  )
  waited = waits({listener_add = "#{base}/auth"})
  check_equal(
    "no wait by default",
    waited.skipped,
    false
  )
  check_equal(
    "no wait: the reply decides",
    waited.status,
    null
  )

  reply :=
    {
      status = 200,
      headers = [("icecast-auth-user", "1"), ("preroll", "#{base}/missing")],
      delay = 0.
    }
  outcome = decide({listener_add = "#{base}/auth"})
  check_equal(
    "failed backend preroll is empty",
    outcome.preroll,
    ""
  )
end

def check_release() =
  release =
    {
      duration = 95.7,
      bytes_sent = 1234,
      listeners = 4,
      metadata = [("artist", "A"), ("title", "T")]
    }
  reply :=
    {
      status = 200,
      headers = [("icecast-auth-user", "1"), ("ice-username", "alice")],
      delay = 0.
    }
  decide =
    icecast.server.decision.url_auth(
      cache=cache,
      {
        listener_add = "#{base}/auth",
        listener_remove = "#{base}/remove",
        username = "radio",
        password = "s3cret"
      }
    )
  outcome = decide(request(user="bob", password="pw"))
  check_equal(
    "outcome carries the rename",
    outcome.user,
    "alice"
  )
  on_release = null.get(outcome.on_release)
  on_release(release)
  wait_for(
    "one listener_remove per release",
    "/remove",
    1
  )
  entry = list.hd(requests_to("/remove"))
  check_equal(
    "release credentials",
    authorization(entry),
    basic("radio", "s3cret")
  )
  check_equal(
    "release fields and order",
    entry.fields,
    [
      ("action", "listener_remove"),
      ("server", "radio.example"),
      ("port", "8000"),
      ("server-instance", instance),
      ("client", "42"),
      ("mount", "/live.mp3?token=x"),
      ("user", "alice"),
      ("pass", "pw"),
      ("ip", "203.0.113.7"),
      ("agent", "VLC/3.0"),
      ("listener-count", "4"),
      (
        "song",
        "A - T"
      ),
      ("sent", "1234"),
      ("duration", "95")
    ]
  )

  reply := {status = 200, headers = [], delay = 0.}
  outcome = decide(request())
  check_equal(
    "refused listener has no on_release",
    null.defined(outcome.on_release),
    false
  )

  remove_only =
    icecast.server.decision.url_auth(
      cache=cache,
      {listener_remove = "#{base}/remove", action_remove = "bye"}
    )
  sent = list.length(requests_to("/auth"))
  outcome = remove_only(request())
  check_equal(
    "listener_remove only: admitted",
    status(outcome),
    null
  )
  check_equal(
    "listener_remove only: no request",
    list.length(requests_to("/auth")),
    sent
  )
  on_release = null.get(outcome.on_release)
  on_release({...release, metadata = [("title", "T")]})
  wait_for(
    "listener_remove only: release notified",
    "/remove",
    2
  )
  fields =
    list.nth(
      default=list.hd(requests_to("/remove")),
      requests_to("/remove"),
      1
    ).fields
  check_equal("action_remove", list.assoc(default="", "action", fields), "bye")
  check_equal(
    "song without artist",
    list.assoc(default="", "song", fields),
    "T"
  )

  unreachable =
    icecast.server.decision.url_auth(cache=cache, {listener_remove = closed})
  outcome = unreachable(request())
  on_release = null.get(outcome.on_release)
  started = time()
  on_release(release)
  check_equal(
    "failed notification does not block",
    time() - started < 1.,
    true
  )
end

def f() =
  check_worked_example()
  check_credentials()
  check_failures()
  check_release()
  if
    checks() < 45
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
