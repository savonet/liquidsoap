%include "../test.liq"
%include "../../src/libs/extra/icecast_server/settings.liq"
%include "../../src/libs/extra/icecast_server/headers.liq"
%include "../../src/libs/extra/icecast_server/addresses.liq"
%include "../../src/libs/extra/icecast_server/refusals.liq"
%include "../../src/libs/extra/icecast_server/parameter_checks.liq"
%include "../../src/libs/extra/icecast_server/parameters.liq"
%include "../../src/libs/extra/icecast_server/mount_parameters.liq"

rejected = ref(0)

def expect_invalid(label, ~field, f) =
  try
    f()
    test.fail(
      "#{label}: accepted"
    )
  catch err : [error.icecast.server.invalid_parameters] do
    if
      not string.contains(substring=field, err.message)
    then
      test.fail(
        "#{label}: message #{string.quote(err.message)} does not name #{field}"
      )
    end
    rejected := rejected() + 1
  end
end

def server(p) =
  ignore(icecast.server.parameters.server(p))
end

def mount(m) =
  p = icecast.server.parameters.server({mount = fun (_) -> m})
  ignore(icecast.server.parameters.mount(p, "/live"))
end

def check_equal(label, got, expected) =
  if
    got != expected
  then
    test.fail(
      "#{label}: got #{got}, expected #{expected}"
    )
  end
end

def f() =
  # Defaults, and a field set to its default behaves as a missing one.
  d = icecast.server.parameters.server({})
  check_equal("name", d.name, "icecast.server")
  check_equal("port", d.port, 8000)
  check_equal("compatibility", d.compatibility, "icecast-2.5")
  check_equal("hostname", d.hostname(), "localhost")
  check_equal("source_password", d.source_password(), "hackme")
  check_equal("shoutcast_mount", d.shoutcast_mount(), "/stream")
  check_equal("admin_user", d.admin_user(), "admin")
  check_equal("admin_password", d.admin_password(), null)
  check_equal("max_history", d.max_history(), 10)
  check_equal("location", d.location(), "Earth")
  check_equal("max_clients", d.max_clients(), null)
  check_equal("source_timeout", d.source_timeout(), 10.)
  check_equal("proxy_ips", d.proxy_ips(), null)
  check_equal("status_page", d.status_page(), true)
  check_equal("metrics", d.metrics(), false)
  check_equal("log_level", d.log_level(), 4)
  check_equal("log_size", d.log_size(), 10240000)
  check_equal("client_timeout", d.client_timeout, 30.)
  check_equal("burst_on_connect", d.burst_on_connect, true)
  check_equal("burst_size", d.burst_size, 65536)
  check_equal("forwarded_address", null.defined(d.forwarded_address), false)
  check_equal(
    "address_hash",
    d.address_hash("1.2.3.4"),
    icecast.server.addresses.hash("1.2.3.4")
  )
  check_equal("mount", d.mount("/a") == null, true)

  e =
    icecast.server.parameters.server(
      {
        port = 8000,
        max_clients = null,
        forwarded_address = null,
        proxy_ips = null
      }
    )
  check_equal(
    "port set to default",
    e.port,
    d.port
  )
  check_equal(
    "max_clients set to null",
    e.max_clients(),
    null
  )
  check_equal(
    "forwarded_address set to null",
    null.defined(e.forwarded_address),
    false
  )

  dm = icecast.server.parameters.mount(d, "/live")
  check_equal(
    "mount source_user",
    dm.source_user,
    "source"
  )
  check_equal(
    "mount source_password",
    dm.source_password,
    "hackme"
  )
  check_equal(
    "mount max_listeners",
    dm.max_listeners,
    null
  )
  check_equal(
    "mount fallback",
    null.defined(dm.fallback),
    false
  )
  check_equal(
    "mount burst_size",
    dm.burst_size,
    65536
  )

  list.iter(
    fun (compatibility) ->
      check_equal(
        "compatibility #{compatibility}",
        icecast.server.parameters.server(
          {compatibility = compatibility}
        ).compatibility,
        compatibility
      ),
    ["icecast-2.5", "icecast-2.4", "rsas", "kh"]
  )

  # Getters are evaluated at each use, and validated there.
  limit = ref(3)
  g = icecast.server.parameters.server({max_clients = fun () -> limit()})
  check_equal("getter", g.max_clients(), 3)
  limit := 5
  check_equal(
    "getter changed",
    g.max_clients(),
    5
  )
  limit := -1
  expect_invalid(
    "getter turned invalid",
    field="max_clients",
    fun () -> ignore(g.max_clients())
  )
  expect_invalid(
    "getter invalid from the start",
    field="max_clients",
    {server({max_clients = fun () -> -1})}
  )
  ips = ref(["10.0.0.0/8"])
  h = icecast.server.parameters.server({proxy_ips = fun () -> ips()})
  ips := ["10.0.0.0/8/9"]
  expect_invalid(
    "proxy_ips getter turned invalid",
    field="proxy_ips",
    fun () -> ignore(h.proxy_ips())
  )
  expect_invalid(
    "proxy_ips entry",
    field="proxy_ips",
    {server({proxy_ips = ["10.0.0.1", "proxy.example"]})}
  )

  # A mount's own value beats the server's, which beats the default.
  p =
    icecast.server.parameters.server(
      {
        source_password = "server",
        max_history = 20,
        health_password = "health",
        burst_size = 0,
        mount =
          fun (path) ->
            path == "/own"
              ? {
                source_password = "own",
                max_history = 5,
                burst_size = 100,
                max_listeners = 2
              }
              : null
      }
    )
  own = icecast.server.parameters.mount(p, "/own")
  inherited = icecast.server.parameters.mount(p, "/other")
  check_equal(
    "mount password",
    own.source_password,
    "own"
  )
  check_equal(
    "server password",
    inherited.source_password,
    "server"
  )
  check_equal(
    "mount max_history",
    own.max_history,
    5
  )
  check_equal(
    "server max_history",
    inherited.max_history,
    20
  )
  check_equal(
    "server health_password",
    inherited.health_password,
    "health"
  )
  check_equal(
    "mount burst over server 0",
    own.burst_size,
    100
  )
  check_equal(
    "server burst 0",
    inherited.burst_size,
    0
  )
  q = icecast.server.parameters.server({mount = fun (_) -> {burst_size = 0}})
  check_equal(
    "mount burst 0 over server",
    icecast.server.parameters.mount(q, "/m").burst_size,
    0
  )

  # A fallback with both fields uses the source.
  s = blank()
  both =
    icecast.server.parameters.server(
      {mount = fun (_) -> {fallback = {mount = "/b", source = s}}}
    )
  fb = null.get(icecast.server.parameters.mount(both, "/a").fallback)
  check_equal(
    "fallback both: mount",
    fb.mount,
    null
  )
  check_equal(
    "fallback both: source",
    null.defined(fb.source),
    true
  )
  by_mount =
    icecast.server.parameters.server(
      {mount = fun (_) -> {fallback = {mount = "/b"}}}
    )
  check_equal(
    "fallback mount",
    null.get(icecast.server.parameters.mount(by_mount, "/a").fallback).mount,
    "/b"
  )

  # Invalid server records.
  expect_invalid(
    "compatibility",
    field="compatibility",
    {server({compatibility = "icecast-3"})}
  )
  expect_invalid(
    "negative port",
    field="port",
    {server({port = -1})}
  )
  expect_invalid(
    "port too large",
    field="port",
    {server({port = 65536})}
  )
  expect_invalid(
    "max_clients",
    field="max_clients",
    {server({max_clients = -1})}
  )
  expect_invalid(
    "max_sources",
    field="max_sources",
    {server({max_sources = -1})}
  )
  expect_invalid(
    "source_timeout",
    field="source_timeout",
    {server({source_timeout = -1.})}
  )
  expect_invalid(
    "source_timeout infinite",
    field="source_timeout",
    {server({source_timeout = infinity})}
  )
  expect_invalid(
    "source_timeout nan",
    field="source_timeout",
    {server({source_timeout = nan})}
  )
  expect_invalid(
    "client_timeout",
    field="client_timeout",
    {server({client_timeout = -0.5})}
  )
  expect_invalid(
    "log_level 0",
    field="log_level",
    {server({log_level = 0})}
  )
  expect_invalid(
    "log_level 6",
    field="log_level",
    {server({log_level = 6})}
  )
  expect_invalid("log_size", field="log_size", {server({log_size = -1})})
  expect_invalid(
    "max_history 0",
    field="max_history",
    {server({max_history = 0})}
  )
  expect_invalid(
    "max_history 257",
    field="max_history",
    {server({max_history = 257})}
  )
  expect_invalid("burst_size", field="burst_size", {server({burst_size = -1})})
  expect_invalid(
    "event binding",
    field="event_bindings",
    {server({event_bindings = [{event = "on_air", handler = fun (_) -> ()}]})}
  )
  expect_invalid(
    "header value and compute",
    field="http_headers",
    {
      server(
        {
          http_headers =
            [{name = "X", value = "v", compute = fun (_, _) -> null}]
        }
      )
    }
  )
  expect_invalid(
    "header without value",
    field="http_headers",
    {server({http_headers = [{name = "X"}]})}
  )
  expect_invalid(
    "header empty name",
    field="http_headers",
    {server({http_headers = [{name = "", value = "v"}]})}
  )
  expect_invalid(
    "header name not a token",
    field="http_headers",
    {
      server(
        {
          http_headers =
            [
              {
                name =
                  "X Y",
                value = "v"
              }
            ]
        }
      )
    }
  )
  expect_invalid(
    "header value with CR LF",
    field="http_headers",
    {
      server(
        {
          http_headers =
            [
              {
                name = "X",
                value =
                  "a\r\nB: c"
              }
            ]
        }
      )
    }
  )
  expect_invalid(
    "header status 99",
    field="http_headers",
    {server({http_headers = [{name = "X", value = "v", status = 99}]})}
  )
  expect_invalid(
    "header status 600",
    field="http_headers",
    {server({http_headers = [{name = "X", value = "v", status = 600}]})}
  )
  server(
    {
      http_headers =
        [
          {name = "X", value = "v", status = 200},
          {name = "Y", compute = fun (_, _) -> null}
        ]
    }
  )

  # Invalid mount records name the mount.
  expect_invalid(
    "mount max_history",
    field="mount /live: max_history",
    {mount({max_history = 0})}
  )
  expect_invalid(
    "mount max_listeners",
    field="mount /live: max_listeners",
    {mount({max_listeners = -1})}
  )
  expect_invalid(
    "mount linger",
    field="mount /live: linger",
    {mount({linger = -1.})}
  )
  expect_invalid(
    "mount max_listener_duration",
    field="max_listener_duration",
    {mount({max_listener_duration = infinity})}
  )
  expect_invalid(
    "mount dump_file_size_limit",
    field="dump_file_size_limit",
    {mount({dump_file_size_limit = -1})}
  )
  expect_invalid(
    "mount burst_size",
    field="mount /live: burst_size",
    {mount({burst_size = -1})}
  )
  expect_invalid(
    "mount http_headers",
    field="mount /live: http_headers",
    {mount({http_headers = [{name = "X:", value = "v"}]})}
  )
  expect_invalid(
    "fallback mount without slash",
    field="fallback.mount",
    {mount({fallback = {mount = "b"}})}
  )

  if
    rejected() != 35
  then
    test.fail(
      "expected 35 rejected records, got #{rejected()}"
    )
  end
  test.pass()
end

test.check(f)
