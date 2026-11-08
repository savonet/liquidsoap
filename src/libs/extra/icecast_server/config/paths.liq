# The `<paths>` and `<logging>` of an Icecast file (icecast-config.md §6).

# @flag hidden
def icecast.server.config.paths(root) =
  node = xml.child("paths", root)
  context = xml.child("tls-context", node)
  if
    null.defined(node)
  then
    icecast.server.config.warn_children(
      known=[
        "logdir",
        "webroot",
        "tls-certificate",
        "ssl-certificate",
        "ssl_certificate",
        "tls-key",
        "ssl-private-key",
        "tls-context",
        "x-forwarded-for",
        "allow-ip",
        "deny-ip",
        "ban-file",
        "banfile",
        "deny-agents"
      ],
      ignored=[
        (
          "basedir",
          "not implemented"
        ),
        (
          "pidfile",
          "not implemented"
        ),
        (
          "adminroot",
          "not implemented"
        ),
        (
          "mime-types",
          "not implemented"
        ),
        (
          "nulldevice",
          "not implemented"
        ),
        (
          "ssl-allowed-ciphers",
          "not implemented, the TLS stack's defaults apply"
        ),
        (
          "tls-allowed-ciphers",
          "not implemented, the TLS stack's defaults apply"
        ),
        (
          "ssl-cafile",
          "not implemented, the TLS stack's defaults apply"
        ),
        (
          "alias",
          "incompatible, the server has no static namespace"
        )
      ],
      null.get(node)
    )
  end
  if
    null.defined(context)
  then
    icecast.server.config.warn_children(
      known=["tls-certificate", "tls-key"],
      ignored=[
        (
          "tls-allowed-ciphers",
          "not implemented, the TLS stack's defaults apply"
        )
      ],
      null.get(context)
    )
  end
  def text(tag) =
    xml.string(xml.child(tag, node))
  end

  def texts(tag) =
    list.filter_map(xml.string, xml.children(tag, node))
  end

  {
    logdir = text("logdir"),
    webroot = text("webroot"),
    certificate =
      text("tls-certificate")
        ?? (
          text("ssl-certificate")
            ?? (
              text("ssl_certificate")
                ?? xml.string(xml.child("tls-certificate", context))
            )
        ),
    key =
      text("tls-key")
        ?? (
          text("ssl-private-key") ?? xml.string(xml.child("tls-key", context))
        ),
    forwarded_for = texts("x-forwarded-for"),
    deny = [...texts("deny-ip"), ...texts("ban-file"), ...texts("banfile")],
    allow = text("allow-ip"),
    agents = text("deny-agents")
  }
end

# The path of a log named by `node`, or `default` when the element is absent.
# `-` is standard error; another name is joined to `logdir`, and ignored with
# a warning without one.
# @flag hidden
def icecast.server.config.log_path(~logdir, ~default, node) =
  name = xml.string(node)
  chosen = null.defined(name) ? name : default
  if
    not null.defined(chosen) or chosen == "-"
  then
    chosen
  elsif
    null.defined(logdir)
  then
    path.concat(null.get(logdir), null.get(chosen))
  else
    if
      null.defined(name)
    then
      let (tag, _) = null.get(node)
      icecast.server.config.warn(
        "Ignoring <#{tag}> #{string.quote(null.get(name))}: no <logdir> to place \
         it in."
      )
    end
    null
  end
end

# @flag hidden
def icecast.server.config.logging(~logdir, root) =
  node = xml.child("logging", root)
  if
    null.defined(node)
  then
    icecast.server.config.warn_children(
      known=[
        "accesslog",
        "errorlog",
        "playlistlog",
        "loglevel",
        "logsize",
        "logarchive"
      ],
      ignored=[
        (
          "memorybacklog",
          "not implemented"
        ),
        (
          "accesslog_*",
          "not implemented"
        ),
        (
          "errorlog_*",
          "not implemented"
        ),
        (
          "playlistlog_lines",
          "not implemented"
        )
      ],
      null.get(node)
    )
  end
  def child(tag) =
    xml.child(tag, node)
  end

  level = xml.string(child("loglevel"))
  log_level = null.map(icecast.server.config.log_level, level)
  if
    null.defined(level) and not null.defined(log_level)
  then
    icecast.server.config.warn(
      "Ignoring <loglevel> #{string.quote(null.get(level))}: not a log level."
    )
  end
  {
    access_log =
      icecast.server.config.log_path(
        logdir=logdir,
        default="access.log",
        child("accesslog")
      ),
    error_log =
      icecast.server.config.log_path(
        logdir=logdir,
        default="error.log",
        child("errorlog")
      ),
    playlist_log =
      icecast.server.config.log_path(
        logdir=logdir,
        default=null,
        child("playlistlog")
      ),
    log_level = log_level,
    log_size = null.map(fun (kib) -> kib * 1024, xml.int(child("logsize"))),
    log_archive = xml.bool(child("logarchive"))
  }
end
