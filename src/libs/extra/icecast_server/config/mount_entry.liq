# One `<mount>` of an Icecast file (icecast-config.md §5), read once at load
# into an entry: `kind` (`"normal"` or `"default"`), `name` (`""` when
# unset), `priority`,
# `values` (its own mount parameters, unset ones `null`), `listener` (its
# authentication's decisions, `null` when it has none of its own), `intro`,
# `preroll` and `bindings`. mounts.liq selects and merges entries.

# @flag hidden
let icecast.server.config.mount_ignored =
  [
    (
      "wait-time",
      "not implemented"
    ),
    (
      "max-bandwidth",
      "future work"
    ),
    (
      "postroll*",
      "future work"
    ),
    (
      "redirect",
      "incompatible, listener redirection is a listener decision"
    ),
    (
      "redirect-to",
      "incompatible, listener redirection is a listener decision"
    ),
    (
      "no-mount",
      "incompatible, the server does not relay"
    ),
    (
      "on-demand",
      "incompatible, the server does not relay"
    ),
    ...list.map(
      fun (tag) ->
        (
          tag,
          "not implemented, directory registration"
        ),
      [
        "public",
        "stream-name",
        "stream-description",
        "stream-url",
        "genre",
        "bitrate",
        "type",
        "subtype",
        "listenurl",
        "cluster-password"
      ]
    ),
    ...list.map(
      fun (tag) ->
        (
          tag,
          "not implemented, delivery tuning"
        ),
      [
        "charset",
        "mp3-metadata-interval",
        "metadata-interval",
        "ogg-passthrough",
        "admin_comments_only",
        "allow-url-ogg-metadata",
        "allow-chunked",
        "max-send-size",
        "so-sndbuf",
        "file-seekable",
        "queue-size",
        "min-queue-size",
        "skip-accesslog",
        "accesslog",
        "preroll-log",
        "ban-client",
        "filter-theora",
        "limit-rate",
        "priority",
        "source-timeout",
        "intro-skip-replay",
        "hijack",
        "allow-direct-access",
        "max-stream-duration"
      ]
    )
  ]

# @flag hidden
let icecast.server.config.mount_known =
  [
    "mount-name",
    "username",
    "password",
    "max-listeners",
    "burst-size",
    "fallback-mount",
    "fallback",
    "fallback-when-full",
    "fallback-override",
    "linger-for",
    "hidden",
    "on-connect",
    "on-disconnect",
    "http-headers",
    "custom-headers",
    "max-listener-duration",
    "max-history",
    "intro",
    "preroll",
    "authentication",
    "dump-file",
    "dump-file-size-limit",
    "dump-file-time-limit",
    "health-password",
    "event-bindings",
    "kartoffelsalat"
  ]

# The fallback of a mount: `<fallback-mount>`, or KH's `<fallback>` holding a
# `<mount>`.
# @flag hidden
def icecast.server.config.fallback(mount) =
  nested = xml.child("fallback", mount)
  if
    null.defined(nested)
  then
    icecast.server.config.warn_children(
      known=["mount"],
      ignored=[
        (
          "rate",
          "not implemented"
        )
      ],
      null.get(nested)
    )
  end
  override = xml.string(xml.child("fallback-override", mount))
  if
    null.defined(override)
  then
    value = string.case(lower=true, string.trim(null.get(override)))
    if
      list.mem(value, ["0", "false", "none"])
    then
      icecast.server.config.warn(
        "Ignoring <fallback-override> #{string.quote(value)}: listeners always \
         return to the mount's own source when it comes back."
      )
    elsif
      not list.mem(value, ["1", "true", "all", "own"])
    then
      icecast.server.config.warn(
        "Ignoring <fallback-override> #{string.quote(value)}: unknown value."
      )
    end
  end
  name =
    xml.string(xml.child("fallback-mount", mount))
      ?? xml.string(xml.child("mount", nested))
  null.map(fun (name) -> {mount = string.trim(name)}, name)
end

# The dump file name, its `%` doubled when `interpolation="none"`.
# @flag hidden
def icecast.server.config.dump_file(mount) =
  node = xml.child("dump-file", mount)
  interpolation = xml.string(attribute="interpolation", node) ?? "strftime"
  name = xml.string(node)
  if
    interpolation == "uuid"
  then
    icecast.server.config.warn(
      "<dump-file interpolation=\"uuid\"> is not implemented: using the name as \
       a strftime pattern."
    )
  elsif
    interpolation != "strftime" and interpolation != "none"
  then
    icecast.server.config.warn(
      "Ignoring <dump-file> interpolation #{string.quote(interpolation)}: \
       unknown value."
    )
  end
  interpolation == "none"
    ? null.map(fun (name) -> r/%/g.replace(fun (_) -> "%%", name), name)
    : name
end

# A source callback running `command` in the background with the mount as
# argument, then `notify`.
# @flag hidden
def icecast.server.config.source_callback(command, notify) =
  if
    not null.defined(command) and not null.defined(notify)
  then
    null
  else
    fun (record) ->
      begin
        if
          null.defined(command)
        then
          ignore(
            process.run(
              process.quote.command(args=[record.mount], null.get(command))
            )
          )
        end
        if
          null.defined(notify)
        then
          null.get(notify)
          (record)
        end
      end
  end
end

# @flag hidden
def icecast.server.config.mount_values(
  ~compatibility,
  ~source_decision,
  ~notify,
  mount
) =
  def child(tag) =
    xml.child(tag, mount)
  end

  def text(tag) =
    xml.string(child(tag))
  end

  max_history = xml.int(child("max-history"))
  {
    source_user = text("username"),
    source_password = text("password"),
    source_decision = source_decision,
    on_connect =
      icecast.server.config.source_callback(
        text("on-connect"),
        notify?.on_connect
      ),
    on_disconnect =
      icecast.server.config.source_callback(
        text("on-disconnect"),
        notify?.on_disconnect
      ),
    linger = xml.float(child("linger-for")),
    max_listeners = xml.int(child("max-listeners")),
    max_listener_duration = xml.float(child("max-listener-duration")),
    fallback = icecast.server.config.fallback(mount),
    fallback_when_full = xml.bool(child("fallback-when-full")),
    hidden = xml.bool(child("hidden")),
    max_history = null.map(fun (n) -> n < 1 or n > 256 ? 256 : n, max_history),
    health_password = text("health-password"),
    burst_size = xml.int(child("burst-size")),
    http_headers =
      icecast.server.config.headers(compatibility=compatibility, mount),
    dump_file = icecast.server.config.dump_file(mount),
    dump_file_size_limit = xml.int(child("dump-file-size-limit")),
    dump_file_time_limit = xml.float(child("dump-file-time-limit"))
  }
end

# The entry of a `<mount>`, `null` when it is ignored.
# @flag hidden
def icecast.server.config.mount_entry(~compatibility, ~cache, mount) =
  kh = compatibility == "kh"
  kind = xml.string(attribute="type", mount) ?? "normal"
  name = null.map(string.trim, xml.string(xml.child("mount-name", mount)))
  if
    kind != "normal" and kind != "default"
  then
    icecast.server.config.warn(
      "Ignoring <mount type=#{string.quote(kind)}>: unknown type."
    )
    null
  elsif
    kind == "normal" and not null.defined(name)
  then
    icecast.server.config.warn(
      "Ignoring a <mount> without <mount-name>."
    )
    null
  else
    icecast.server.config.warn_children(
      known=kh
        ? [...icecast.server.config.mount_known, "priority"]
        : (icecast.server.config.mount_known),
      ignored=icecast.server.config.mount_ignored,
      mount
    )
    auth = icecast.server.config.mount_authentication(cache=cache, mount)
    bindings = icecast.server.config.event_bindings(mount=name, mount)
    if
      kind == "default" and not list.is_empty(bindings)
    then
      icecast.server.config.warn(
        "Ignoring <event-bindings> of a default mount: a binding belongs to one \
         mount."
      )
    end
    {
      kind = kind,
      name = null.get(default="", name),
      priority = xml.int(xml.child("priority", mount)) ?? max_int,
      patterns =
        null.case(
          name,
          {[]},
          fun (name) ->
            icecast.server.access.pattern(star_matches_slash=kh, name)
        ),
      values =
        icecast.server.config.mount_values(
          compatibility=compatibility,
          source_decision=auth.source_decision,
          notify=auth.notify,
          mount
        ),
      listener = auth.listener,
      intro = xml.string(xml.child("intro", mount)),
      preroll = xml.string(xml.child("preroll", mount)),
      bindings = kind == "default" ? [] : bindings
    }
  end
end
