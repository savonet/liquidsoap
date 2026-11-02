# The listening socket of an Icecast file (icecast-config.md §3.1): the first
# `<listen-socket>`, else the root's `port` and `bind-address`.

# Whether a `<tls>` or `<ssl>` element asks for TLS, `null` when it is absent.
# @flag hidden
def icecast.server.config.tls_mode(node) =
  null.case(
    node,
    {null},
    fun (node) ->
      begin
        value = xml.string(node) ?? ""
        mode = string.case(lower=true, string.trim(value))
        if
          list.mem(mode, ["0", "disabled", "false", "no", "off"])
        then
          false
        elsif
          list.mem(mode, ["1", "rfc2818", "true", "yes", "on"])
        then
          true
        elsif
          list.mem(mode, ["auto", "auto_no_plain", "rfc2817"])
        then
          icecast.server.config.reject(
            "<tls> #{string.quote(value)}: the server cannot serve TLS and plain \
             HTTP on one port, nor upgrade a connection in place. Use rfc2818 \
             for TLS or disabled for plain HTTP."
          )
        else
          icecast.server.config.reject(
            "<tls> #{string.quote(value)} is not a TLS mode."
          )
        end
      end
  )
end

# `port`, `bind_address`, `tls` (nullable), `shoutcast_mount` and the
# `trusted-proxy` addresses of the socket the server listens on.
# @flag hidden
def icecast.server.config.listen(root) =
  sockets = xml.children("listen-socket", root)
  if
    list.length(sockets) > 1
  then
    icecast.server.config.warn(
      "Using the first of #{list.length(sockets)} <listen-socket> elements: one \
       server listens on one port."
    )
  end
  socket = list.case(sockets, null, fun (socket, _) -> socket)

  # A trusted proxy names listen sockets by `id`, whose `client-ip` values
  # are the proxies' addresses, or is itself an address or a CIDR range.
  def trusted(proxy) =
    name = string.trim(xml.string(proxy) ?? "")
    named =
      list.filter(
        fun (s) -> xml.string(attribute="id", s) == (name : string?),
        sockets
      )
    if
      name == ""
    then
      icecast.server.config.warn(
        "Ignoring an empty <trusted-proxy>."
      )
      []
    elsif
      list.is_empty(named)
    then
      [name]
    else
      list.flatten(
        list.map(
          fun (s) -> list.filter_map(xml.string, xml.children("client-ip", s)),
          named
        )
      )
    end
  end

  if
    null.defined(socket)
  then
    icecast.server.config.warn_children(
      known=[
        "port",
        "bind-address",
        "tls",
        "ssl",
        "shoutcast-mount",
        "trusted-proxy",
        "client-ip"
      ],
      ignored=[
        (
          "shoutcast-compat",
          "incompatible, the Shoutcast protocol is always served on port + 1"
        ),
        (
          "custom-headers",
          "not translated, use a compute entry of http_headers"
        )
      ],
      null.get(socket)
    )
  end
  {
    port =
      null.defined(socket)
        ? xml.int(xml.child("port", socket))
        : (xml.int(xml.child("port", root))),
    bind_address =
      null.defined(socket)
        ? xml.string(xml.child("bind-address", socket))
        : (xml.string(xml.child("bind-address", root))),
    tls =
      icecast.server.config.tls_mode(
        xml.child("tls", socket) ?? xml.child("ssl", socket)
      ),
    shoutcast_mount =
      xml.string(xml.child("shoutcast-mount", socket))
        ?? xml.string(xml.child("shoutcast-mount", root)),
    trusted_proxies =
      list.flatten(list.map(trusted, xml.children("trusted-proxy", socket)))
  }
end
