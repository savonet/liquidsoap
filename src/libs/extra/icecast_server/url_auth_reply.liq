# Trimmed value of the reply header `name`, `null` when absent.
# @flag hidden
def icecast.server.url_auth.header(reply, name) =
  null.map(string.trim, list.assoc.nullable(name, reply.headers))
end

# The refusal of a reply that neither admits nor redirects (url-auth.md
# §5.3).
# @flag hidden
def icecast.server.url_auth.refusal(~config, request, reply) =
  def header(name) =
    icecast.server.url_auth.header(reply, name)
  end

  messages =
    list.filter_map(
      header,
      [config.header_message, "x-icecast-auth-message", "icecast-auth-message"]
    )
  message = list.case(messages, "", fun (message, _) -> message)
  log.info(
    label="icecast.server",
    "Listener #{request.id} on #{request.mount} refused by URL authentication \
     (status #{reply.status}): #{message}"
  )
  challenge = header("icecast-www-authenticate")
  headers =
    null.defined(challenge) ? [("WWW-Authenticate", null.get(challenge))] : []
  status = int_of_string(default=0, header("icecast-resp-status") ?? "")
  kh_message = header("icecast-auth-message") ?? ""
  kh_forbidden = string.residual(prefix="403", kh_message)
  if
    status >= 300 and status <= 599
  then
    icecast.server.refusal(
      reason=header("icecast-resp-reason"),
      headers=headers,
      status
    )
  elsif
    config.kh and (reply.status == 403 or null.defined(kh_forbidden))
  then
    reason = string.trim(kh_forbidden ?? "")
    icecast.server.refusal(
      reason=reason == "" ? null : reason,
      headers=headers,
      403
    )
  else
    icecast.server.refusal(headers=headers, 401)
  end
end

# The time limit of an admitting reply (url-auth.md §5.1).
# @flag hidden
def icecast.server.url_auth.time_limit(~config, reply) =
  is_2xx = reply.status >= 200 and reply.status < 300
  old_value = icecast.server.url_auth.header(reply, config.old_timelimit)
  value =
    if
      config.old_timelimit != "" and null.defined(old_value)
    then
      old_value
    elsif
      is_2xx and config.new_timelimit != ""
    then
      icecast.server.url_auth.header(reply, config.new_timelimit)
    else
      null
    end
  if
    not null.defined(value)
  then
    null
  elsif
    r/^[0-9]+$/.test(null.get(value))
    and int_of_string(default=-1, null.get(value)) >= 0
  then
    float(int_of_string(default=0, null.get(value)))
  elsif
    config.kh
  then
    60.
  else
    log.important(
      label="icecast.server",
      "Ignoring URL authentication time limit #{string.quote(null.get(value))}."
    )
    null
  end
end

# The redirect of a 2xx reply's alter action, `null` when it has none.
# @flag hidden
def icecast.server.url_auth.alter_redirect(~config, reply) =
  action =
    icecast.server.url_auth.lower(
      icecast.server.url_auth.header(reply, config.header_alter_action)
        ?? "noop"
    )
  argument = icecast.server.url_auth.header(reply, config.header_alter_argument)
  status =
    list.assoc(
      default=0,
      action,
      [
        ("redirect", 303),
        ("redirect_see_other", 303),
        ("redirect_temporary", 307),
        ("redirect_permanent", 308)
      ]
    )
  if
    status == 0 and action != "noop"
  then
    log.important(
      label="icecast.server",
      "Ignoring URL authentication alter action #{string.quote(action)}."
    )
  end
  status > 0 and null.defined(argument)
    ? icecast.server.redirect(status, null.get(argument))
    : null
end

# Whether a reply admits (url-auth.md §4): the `auth_header` line whatever the
# status, or `header_auth: ok` on a 2xx reply.
# @flag hidden
def icecast.server.url_auth.admits(~config, reply) =
  lower = icecast.server.url_auth.lower
  old_auth = config.old_auth
  old_value =
    old_auth.name == ""
      ? null
      : (icecast.server.url_auth.header(reply, old_auth.name))
  old_admits =
    if
      not null.defined(old_value)
    then
      false
    elsif
      config.kh
    then
      not string.contains(prefix="0", null.get(old_value))
    else
      lower(null.get(old_value)) == old_auth.value
    end
  old_admits
  or (
    reply.status >= 200
    and reply.status < 300
    and config.new_auth != ""
    and lower(icecast.server.url_auth.header(reply, config.new_auth) ?? "") ==
      "ok"
  )
end

# The admission a URL authentication reply `{status, headers, body}`, header
# names lowercased, gives (url-auth.md §4 to §6): `{refuse, time_limit,
# preroll, exempt, user}`, `user` being the backend's rename of the listener,
# `null` without one. The preroll is loaded with `cache` only for an admitted
# listener.
# @flag hidden
def icecast.server.url_auth.outcome(~cache, ~config, request, reply) =
  lower = icecast.server.url_auth.lower
  is_2xx = reply.status >= 200 and reply.status < 300

  def header(name) =
    icecast.server.url_auth.header(reply, name)
  end

  old_auth = config.old_auth
  old_value = old_auth.name == "" ? null : header(old_auth.name)
  withintro = config.kh and lower(old_value ?? "") == "withintro"
  admitted = icecast.server.url_auth.admits(config=config, reply)

  location = config.kh ? header("location") : null
  alter =
    null.defined(location) or not is_2xx
      ? null
      : (icecast.server.url_auth.alter_redirect(config=config, reply))
  auth_redirect = header("icecast-auth-redirect")

  # The precedence of url-auth.md §6.
  refuse =
    if
      null.defined(location)
    then
      icecast.server.redirect(302, null.get(location))
    elsif
      null.defined(alter)
    then
      alter
    elsif
      admitted
    then
      null
    elsif
      null.defined(auth_redirect)
    then
      icecast.server.redirect(303, null.get(auth_redirect))
    else
      icecast.server.url_auth.refusal(config=config, request, reply)
    end

  if
    null.defined(refuse)
  then
    {
      refuse = refuse,
      time_limit = null,
      preroll = null,
      exempt = false,
      user = null
    }
  else
    slave = header("icecast-slave")
    {
      refuse = null,
      time_limit = icecast.server.url_auth.time_limit(config=config, reply),
      preroll =
        if
          withintro
        then
          string.bytes.length(reply.body) <= cache.preroll_max_size
            ? reply.body
            : ""
        else
          null.map(cache.backend, header("preroll"))
        end,
      exempt = null.defined(slave) and null.get(slave) != "0",
      user = header("ice-username")
    }
  end
end
