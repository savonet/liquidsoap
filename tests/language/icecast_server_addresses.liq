%include "../test.liq"
%include "../../src/libs/extra/icecast_server/settings.liq"
%include "../../src/libs/extra/icecast_server/addresses.liq"

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

def request(~forwarded=null, ip) =
  {
    ip = ip,
    headers =
      null.defined(forwarded) ? [("x-forwarded-for", null.get(forwarded))] : [],
    protocol = "HTTP/1.1",
    uri = "/live"
  }
end

def walk(~forwarded=null, proxy_ips, ip) =
  icecast.server.addresses.resolve(
    proxy_ips=proxy_ips,
    forwarded_address=null,
    request(forwarded=forwarded, ip)
  )
end

def check_ranges() =
  in_range = icecast.server.addresses.in_range
  check_equal(
    "exact v4",
    in_range("10.0.0.1", "10.0.0.1"),
    true
  )
  check_equal(
    "other v4",
    in_range("10.0.0.1", "10.0.0.2"),
    false
  )
  check_equal("/8", in_range("10.0.0.0/8", "10.200.3.4"), true)
  check_equal(
    "/8 outside",
    in_range("10.0.0.0/8", "11.0.0.1"),
    false
  )
  check_equal(
    "/20 inside",
    in_range("192.168.16.0/20", "192.168.31.255"),
    true
  )
  check_equal(
    "/20 outside",
    in_range("192.168.16.0/20", "192.168.32.0"),
    false
  )
  check_equal("/0", in_range("0.0.0.0/0", "8.8.8.8"), true)
  check_equal("/32", in_range("1.2.3.4/32", "1.2.3.5"), false)
  check_equal(
    "v6 compressed",
    in_range("::1", "0:0:0:0:0:0:0:1"),
    true
  )
  check_equal(
    "v6 /32",
    in_range("2001:db8::/32", "2001:db8:ffff::1"),
    true
  )
  check_equal(
    "v6 /32 outside",
    in_range("2001:db8::/32", "2001:db9::1"),
    false
  )
  check_equal(
    "v6 /127",
    in_range("fe80::/127", "fe80::1"),
    true
  )
  check_equal(
    "v6 /127 outside",
    in_range("fe80::/127", "fe80::2"),
    false
  )
  check_equal(
    "v4 vs v6",
    in_range("0.0.0.0/0", "::1"),
    false
  )
  check_equal(
    "v4 in v6 notation",
    in_range("::ffff:10.0.0.0/104", "::ffff:10.1.2.3"),
    true
  )
  check_equal(
    "invalid entry",
    in_range("proxy.example", "10.0.0.1"),
    false
  )
  check_equal(
    "prefix too long",
    in_range("10.0.0.0/33", "10.0.0.0"),
    false
  )
  check_equal(
    "invalid octet",
    in_range("10.0.0.256", "10.0.0.256"),
    false
  )
  check_equal(
    "invalid v6",
    in_range("1::2::3", "1::2::3"),
    false
  )
  check_equal(
    "two prefixes",
    in_range("10.0.0.0/8/9", "10.0.0.1"),
    false
  )
  is_range = icecast.server.addresses.is_range
  check_equal(
    "is_range address",
    is_range("::1"),
    true
  )
  check_equal(
    "is_range CIDR",
    is_range("10.0.0.0/8"),
    true
  )
  check_equal(
    "is_range two prefixes",
    is_range("10.0.0.0/8/9"),
    false
  )
  check_equal(
    "is_range empty prefix",
    is_range("10.0.0.0/"),
    false
  )
  check_equal(
    "is_range name",
    is_range("proxy.example"),
    false
  )
end

def check_walk() =
  chain =
    "203.0.113.7, 10.0.0.2, 10.0.0.3"
  check_equal(
    "unset list",
    walk(forwarded=chain, null, "10.0.0.1"),
    "10.0.0.1"
  )
  check_equal(
    "empty list trusts the rightmost entry",
    walk(forwarded=chain, [], "198.51.100.1"),
    "10.0.0.3"
  )
  check_equal(
    "untrusted peer",
    walk(forwarded=chain, ["10.0.0.0/8"], "198.51.100.1"),
    "198.51.100.1"
  )
  check_equal(
    "trusted chain",
    walk(forwarded=chain, ["10.0.0.0/8"], "10.0.0.1"),
    "203.0.113.7"
  )
  check_equal(
    "forged left entry",
    walk(
      forwarded="1.1.1.1, 203.0.113.7, 10.0.0.2",
      ["10.0.0.0/8"],
      "10.0.0.1"
    ),
    "203.0.113.7"
  )
  check_equal(
    "exact address entries",
    walk(
      forwarded="203.0.113.7, 10.0.0.2",
      ["10.0.0.1", "10.0.0.2"],
      "10.0.0.1"
    ),
    "203.0.113.7"
  )
  check_equal(
    "absent header",
    walk(["10.0.0.0/8"], "10.0.0.1"),
    "10.0.0.1"
  )
  check_equal(
    "only proxies",
    walk(
      forwarded="10.0.0.3, 10.0.0.2",
      ["10.0.0.0/8"],
      "10.0.0.1"
    ),
    "10.0.0.1"
  )
  check_equal(
    "a proxy's own header line comes after the client's",
    icecast.server.addresses.resolve(
      proxy_ips=["10.0.0.0/8"],
      forwarded_address=null,
      {
        ip = "10.0.0.1",
        headers =
          [("x-forwarded-for", "6.6.6.6"), ("x-forwarded-for", "198.51.100.7")],
        protocol = "HTTP/1.1",
        uri = "/live"
      }
    ),
    "198.51.100.7"
  )
  check_equal(
    "malformed entry",
    walk(forwarded="not-an-address", ["10.0.0.0/8"], "10.0.0.1"),
    "10.0.0.1"
  )
  check_equal(
    "empty header",
    walk(forwarded="", [], "10.0.0.1"),
    "10.0.0.1"
  )
  check_equal(
    "v6 client",
    walk(forwarded="2001:db8::7", ["::1"], "::1"),
    "2001:db8::7"
  )
end

def check_forwarded_address() =
  seen = ref([])
  def forwarded_address(request) =
    seen := request.proxy_ips ?? ["unset"]
    request.ip == "10.0.0.1" ? "192.0.2.9" : null
  end

  def resolve(proxy_ips, ip) =
    icecast.server.addresses.resolve(
      proxy_ips=proxy_ips,
      forwarded_address=forwarded_address,
      request(forwarded="203.0.113.7", ip)
    )
  end

  check_equal(
    "replaces the walk",
    resolve(["10.0.0.0/8"], "10.0.0.1"),
    "192.0.2.9"
  )
  check_equal(
    "receives proxy_ips",
    seen(),
    ["10.0.0.0/8"]
  )
  check_equal(
    "null gives the peer",
    resolve(["10.0.0.0/8"], "10.0.0.5"),
    "10.0.0.5"
  )
  check_equal(
    "runs with proxy_ips unset",
    resolve(null, "10.0.0.1"),
    "192.0.2.9"
  )
  check_equal(
    "receives unset proxy_ips",
    seen(),
    ["unset"]
  )

  def failing(_) =
    error.raise(
      error.invalid,
      "backend down"
    )
  end
  check_equal(
    "error gives the peer",
    icecast.server.addresses.resolve(
      proxy_ips=[],
      forwarded_address=failing,
      request(forwarded="203.0.113.7", "10.0.0.1")
    ),
    "10.0.0.1"
  )
end

def check_hash() =
  hash = icecast.server.addresses.hash("203.0.113.7")
  check_equal(
    "hash length",
    string.length(hash),
    16
  )
  check_equal(
    "hash is hexadecimal",
    r/^[0-9a-f]{16}$/.test(hash),
    true
  )
  check_equal(
    "hash is unkeyed MD5",
    hash,
    string.sub(string.digest("203.0.113.7"), start=0, length=16)
  )
  check_equal(
    "hash differs per address",
    hash != icecast.server.addresses.hash("203.0.113.8"),
    true
  )
end

def f() =
  check_ranges()
  check_walk()
  check_forwarded_address()
  check_hash()
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
