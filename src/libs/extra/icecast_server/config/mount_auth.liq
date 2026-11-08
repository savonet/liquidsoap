# A `<mount>`'s `<authentication>` (icecast-config.md §5.1, §5.4), built once
# at load: `listener`, its listener decisions in composition order (`null` when
# it gives the mount none of its own), `source_decision`, and `notify`, the
# `mount_add` and `mount_remove` callbacks.

# The url-auth options of url-auth.md §1 and sources.md §7 from the
# `<option>` children of `node`, unset ones `null`; an unknown option warns.
# @flag hidden
def icecast.server.config.url_options(node) =
  numeric = ["timeout", "presume_innocent", "on_error_wait"]
  strings =
    [
      "listener_add",
      "client_add",
      "listener_remove",
      "client_remove",
      "username",
      "password",
      "auth_header",
      "timelimit_header",
      "header_auth",
      "header_timelimit",
      "header_message",
      "header_alter_action",
      "header_alter_argument",
      "action_add",
      "action_remove",
      "headers",
      "header_prefix",
      "mount_add",
      "mount_remove",
      "stream_auth"
    ]
  list.iter(
    fun (option) ->
      if
        not list.mem(fst(option), [...strings, ...numeric])
      then
        icecast.server.config.warn(
          "Ignoring url authentication option #{string.quote(fst(option))}: \
           unknown option."
        )
      end,
    icecast.server.config.options(node)
  )

  def option(name) =
    icecast.server.config.option(name, node)
  end

  def text(name) =
    xml.string(attribute="value", option(name))
  end

  auth_header = text("auth_header")
  if
    null.defined(auth_header)
    and string.trim(
      fst(string.split.first(separator=":", "#{null.get(auth_header)}:"))
    ) ==
      ""
  then
    icecast.server.config.reject(
      "url authentication option auth_header #{
        string.quote(null.get(auth_header))
      } has no header name."
    )
  end
  {
    listener_add = text("listener_add"),
    client_add = text("client_add"),
    listener_remove = text("listener_remove"),
    client_remove = text("client_remove"),
    username = text("username"),
    password = text("password"),
    auth_header = auth_header,
    timelimit_header = text("timelimit_header"),
    header_auth = text("header_auth"),
    header_timelimit = text("header_timelimit"),
    header_message = text("header_message"),
    header_alter_action = text("header_alter_action"),
    header_alter_argument = text("header_alter_argument"),
    action_add = text("action_add"),
    action_remove = text("action_remove"),
    headers = text("headers"),
    header_prefix = text("header_prefix"),
    timeout = xml.float(attribute="value", option("timeout")),
    presume_innocent = xml.bool(attribute="value", option("presume_innocent")),
    on_error_wait = xml.float(attribute="value", option("on_error_wait")),
    mount_add = text("mount_add"),
    mount_remove = text("mount_remove"),
    stream_auth = text("stream_auth")
  }
end

# The per-user limits of an `<authentication>` and its `<acl>`'s
# `connections-per-user`, as decisions.
# @flag hidden
def icecast.server.config.per_user_limits(node) =
  def limit(holder) =
    value = xml.string(attribute="connections-per-user", holder)
    if
      not null.defined(value) or string.trim(null.get(value)) == "unlimited"
    then
      []
    else
      n = xml.int(attribute="connections-per-user", holder) ?? 0
      n > 0 ? [icecast.server.connections_per_user(n)] : []
    end
  end

  [...limit(node), ...limit(xml.child("acl", node))]
end

# @flag hidden
def icecast.server.config.htpasswd(~options, node) =
  def option(name) =
    list.assoc.nullable(name, options)
  end

  list.iter(
    fun (option) ->
      if
        fst(option) == "drop_existing_listener"
      then
        icecast.server.config.warn(
          "Ignoring htpasswd option drop_existing_listener: not implemented."
        )
      elsif
        not list.mem(fst(option), ["filename", "allow_duplicate_users"])
      then
        icecast.server.config.warn(
          "Ignoring htpasswd option #{string.quote(fst(option))}: unknown \
           option."
        )
      end,
    options
  )
  filename = option("filename")
  if
    not null.defined(filename)
  then
    icecast.server.config.reject(
      "htpasswd authentication without a filename option."
    )
  end
  duplicates = option("allow_duplicate_users")
  single_user =
    null.defined(duplicates) and string.trim(null.get(duplicates)) == "0"
  [
    icecast.server.decision.htpasswd(null.get(filename)),
    ...(single_user ? [icecast.server.connections_per_user(1)] : []),
    ...icecast.server.config.per_user_limits(node)
  ]
end

# @flag hidden
def icecast.server.config.mount_authentication(~cache, mount) =
  node = xml.child("authentication", mount)
  icecast.server.config.reject_roles(node)
  icecast.server.config.warn_children(known=["option", "role", "acl"], node)
  kind = xml.string(attribute="type", node)
  options = icecast.server.config.options(node)
  url = icecast.server.config.url_options(kind == "url" ? node : null)
  listens =
    list.exists(
      fun (name) -> list.assoc.mem(name, options),
      ["listener_add", "client_add", "listener_remove", "client_remove"]
    )
  notifies = null.defined(url.mount_add) or null.defined(url.mount_remove)
  if
    null.defined(node) and kind != "url" and kind != "htpasswd"
  then
    icecast.server.config.reject(
      "<authentication type=#{string.quote(kind ?? "")}> is not supported: \
       ignoring it would open the mount to every listener."
    )
  end
  {
    listener =
      if
        kind == "htpasswd"
      then
        icecast.server.config.htpasswd(options=options, node)
      elsif
        kind == "url" and listens
      then
        [
          icecast.server.decision.url_auth(cache=cache, url),
          ...icecast.server.config.per_user_limits(node)
        ]
      else
        null
      end,
    source_decision =
      null.defined(url.stream_auth)
        ? icecast.server.source_decision.stream_auth(url)
        : null,
    notify = notifies ? icecast.server.source_callbacks.url(url) : null
  }
end
