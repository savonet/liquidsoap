# Client addresses of icecast.server (listeners.md §8 and §9). The server calls
# `resolve` with the request of every client, listener, source client or admin
# request alike, and the parameters read at that use; `hash` is the default
# `address_hash`, applied before observers see an address.
let icecast.server.addresses = ()

# Bytes of an IPv4 address, `null` when it is not one.
# @flag hidden
def icecast.server.addresses.ipv4_bytes(address) =
  parts = string.split(separator="\\.", address)
  def byte(part) =
    r/^[0-9]{1,3}$/.test(part) ? int_of_string(part) : 256
  end
  bytes = list.map(byte, parts)
  if
    list.length(bytes) == 4 and list.for_all(fun (b) -> b <= 255, bytes)
  then
    bytes
  else
    null
  end
end

# Bytes of an IPv6 address, `null` when it is not one. The last 32 bits may be
# written as an IPv4 address.
# @flag hidden
def icecast.server.addresses.ipv6_bytes(address) =
  def group_bytes(group) =
    if
      r/^[0-9a-fA-F]{1,4}$/.test(group)
    then
      n = int_of_string("0x#{group}")
      [n / 256, n mod 256]
    elsif
      string.contains(substring=".", group)
    then
      icecast.server.addresses.ipv4_bytes(group) ?? [-1]
    else
      [-1]
    end
  end

  def groups_bytes(groups) =
    groups == [""] ? [] : (list.flatten(list.map(group_bytes, groups)))
  end

  def side(text) =
    groups_bytes(string.split(separator=":", text))
  end

  sides = string.split(separator="::", address)
  bytes =
    if
      list.length(sides) == 1
    then
      side(address)
    elsif
      list.length(sides) == 2 and string.contains(substring="::", address)
    then
      left = side(list.nth(default="", sides, 0))
      right = side(list.nth(default="", sides, 1))
      zeros = 16 - list.length(left) - list.length(right)
      zeros > 0 ? [...left, ...list.init(zeros, fun (_) -> 0), ...right] : [-1]
    else
      [-1]
    end
  if
    list.length(bytes) == 16 and not list.mem(-1, bytes)
  then
    bytes
  else
    null
  end
end

# Bytes of an IPv4 or IPv6 address, `null` when it is neither.
# @flag hidden
def icecast.server.addresses.bytes(address) =
  icecast.server.addresses.ipv4_bytes(address)
    ?? (icecast.server.addresses.ipv6_bytes(address))
end

# Network bytes and prefix length of `entry`, an address or a CIDR range,
# `null` when it is neither.
# @flag hidden
def icecast.server.addresses.range(entry) =
  groups = r/^([^\/]+)(\/([0-9]{1,3}))?$/.exec(entry)
  network = icecast.server.addresses.bytes(list.assoc(default="", 1, groups))
  if
    not null.defined(network)
  then
    null
  else
    network = null.get(network)
    width = 8 * list.length(network)
    prefix = list.assoc.nullable(3, groups)
    bits = null.defined(prefix) ? int_of_string(null.get(prefix)) : width
    bits <= width ? {network = network, bits = bits} : null
  end
end

# Whether `entry` is an address or a CIDR range.
# @flag hidden
def icecast.server.addresses.is_range(entry) =
  null.defined(icecast.server.addresses.range(entry))
end

# Whether `address` is in the range `entry`. An entry that is neither an
# address nor a CIDR range matches nothing.
# @flag hidden
def icecast.server.addresses.in_range(entry, address) =
  range = icecast.server.addresses.range(entry)
  address = icecast.server.addresses.bytes(address)
  def rec same(bits, network, address) =
    if
      bits <= 0 or network == []
    then
      true
    else
      divisor = bits >= 8 ? 1 : (lsl(1, 8 - bits))
      list.hd(network) / divisor == list.hd(address) / divisor
      and same(bits - 8, list.tl(network), list.tl(address))
    end
  end

  if
    not null.defined(range) or not null.defined(address)
  then
    false
  else
    range = null.get(range)
    address = null.get(address)
    list.length(address) == list.length(range.network)
    and same(range.bits, range.network, address)
  end
end

# Whether `address` is one of the trusted `proxy_ips`.
# @flag hidden
def icecast.server.addresses.trusted(proxy_ips, address) =
  list.exists(
    fun (entry) -> icecast.server.addresses.in_range(entry, address),
    proxy_ips
  )
end

# The client's address behind the trusted `proxy_ips`: the rightmost
# `X-Forwarded-For` entry that is not a proxy, when the peer itself is one.
# @flag hidden
def icecast.server.addresses.walk(~proxy_ips, request) =
  forwarded = list.assoc.nullable("x-forwarded-for", request.headers)
  if
    not null.defined(forwarded)
    or (
      proxy_ips != []
      and not icecast.server.addresses.trusted(proxy_ips, request.ip)
    )
  then
    request.ip
  else
    entries =
      list.rev(
        list.map(string.trim, string.split(separator=",", null.get(forwarded)))
      )
    client =
      list.find(
        default="",
        fun (entry) -> not icecast.server.addresses.trusted(proxy_ips, entry),
        entries
      )
    null.defined(icecast.server.addresses.bytes(client)) ? client : (request.ip)
  end
end

# The address of the client that sent `request` (`{ip, headers, protocol,
# uri}`): `forwarded_address`'s answer when it is set, else the walk of
# `proxy_ips` when it is set, else the peer address.
# @flag hidden
def icecast.server.addresses.resolve(~proxy_ips, ~forwarded_address, request) =
  if
    null.defined(forwarded_address)
  then
    forwarded_address = null.get(forwarded_address)
    try
      forwarded_address(
        {
          ip = request.ip,
          headers = request.headers,
          protocol = request.protocol,
          uri = request.uri,
          proxy_ips = proxy_ips
        }
      )
        ?? request.ip
    catch err do
      log.important(
        label="icecast.server",
        "forwarded_address failed, using the peer address: #{err.message}"
      )
      request.ip
    end
  elsif
    null.defined(proxy_ips)
  then
    icecast.server.addresses.walk(proxy_ips=null.get(proxy_ips), request)
  else
    request.ip
  end
end

# Default `address_hash` of `icecast.server`: an unkeyed MD5 digest of the
# address, 16 hexadecimal characters. Stable across runs, and reversible by
# guessing an address: pass a keyed one when that matters.
# @flag hidden
def icecast.server.addresses.hash(address) =
  string.sub(string.digest(address), start=0, length=16)
end
