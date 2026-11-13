# The root `<authentication>` and `<limits>` of an Icecast file
# (icecast-config.md §4).

# Rejects an `<authentication>` with `<role>` elements: they protect listeners
# in Icecast and are not translated (icecast-config.md §5.4).
# @flag hidden
def icecast.server.config.reject_roles(node) =
  if
    not list.is_empty(xml.children("role", node))
  then
    icecast.server.config.reject(
      "<role> authentication is not supported: ignoring it would open what it \
       protects."
    )
  end
end

# @flag hidden
def icecast.server.config.authentication(root) =
  node = xml.child("authentication", root)
  icecast.server.config.reject_roles(node)
  if
    null.defined(node)
  then
    icecast.server.config.warn_children(
      known=[
        "source-password",
        "admin-user",
        "admin-password",
        "health-password"
      ],
      ignored=[
        (
          "relay-user",
          "future work, relays"
        ),
        (
          "relay-password",
          "future work, relays"
        )
      ],
      null.get(node)
    )
  end
  def text(tag) =
    xml.string(xml.child(tag, node))
  end

  {
    source_password = text("source-password"),
    admin_user = text("admin-user"),
    admin_password = text("admin-password"),
    health_password = text("health-password")
  }
end

# @flag hidden
def icecast.server.config.limits(root) =
  node = xml.child("limits", root)
  if
    null.defined(node)
  then
    icecast.server.config.warn_children(
      known=[
        "sources",
        "clients",
        "max-listeners",
        "client-timeout",
        "source-timeout",
        "burst-size",
        "burst-on-connect"
      ],
      ignored=[
        (
          "queue-size",
          "incompatible, the server manages listener buffering itself"
        ),
        (
          "body-timeout",
          "incompatible, request bodies are read by the HTTP layer"
        ),
        (
          "max-bandwidth",
          "future work"
        ),
        (
          "workers",
          "not implemented"
        ),
        (
          "listen-backlog",
          "not implemented"
        ),
        (
          "connections-per-user",
          "not implemented, the per-user limit is a mount's authentication"
        )
      ],
      null.get(node)
    )
  end
  def child(tag) =
    xml.child(tag, node)
  end

  {
    max_sources = xml.int(child("sources")),
    max_clients = xml.int(child("clients")),
    max_listeners = xml.int(child("max-listeners")),
    client_timeout = xml.float(child("client-timeout")),
    source_timeout = xml.float(child("source-timeout")),
    burst_size = xml.int(child("burst-size")),
    burst_on_connect =
      xml.bool(child("burst-on-connect")) == false ? false : null
  }
end
