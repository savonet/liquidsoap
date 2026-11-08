# Options of URL authentication (url-auth.md §1, §10).
# `icecast.server.decision.url_auth` resolves them per request with
# `config(compatibility, options)`, since their defaults depend on the
# request's `compatibility`; url_auth_request.liq and url_auth_reply.liq read
# the resolved record.
let icecast.server.url_auth = ()

# @flag hidden
def icecast.server.url_auth.lower(text) =
  string.case(lower=true, string.trim(text))
end

# The options of `icecast.server.decision.url_auth` with their defaults under
# `compatibility`, an unset name or URL being `""`. Setting either style of the
# admitting or of the time limit header disables the other style's default.
# @flag hidden
def icecast.server.url_auth.config(compatibility, options) =
  kh = compatibility == "kh"
  lower = icecast.server.url_auth.lower

  def header_line(line) =
    let (name, value) =
      string.contains(substring=":", line)
        ? string.split.first(separator=":", line)
        : (line, "")
    {name = lower(name), value = lower(value)}
  end

  def header_name(name) =
    lower(r/:\s*$/.replace(fun (_) -> "", name))
  end

  def chosen(old, new, old_default, new_default) =
    if
      old == "" and new == ""
    then
      {old = old_default, new = new_default}
    else
      {old = old, new = new}
    end
  end

  auth =
    chosen(
      options?.auth_header ?? "",
      options?.header_auth ?? "",
      kh
        ? "icecast-auth-user:"
        : "icecast-auth-user: 1",
      "x-icecast-auth-result"
    )
  timelimit =
    chosen(
      options?.timelimit_header ?? "",
      options?.header_timelimit ?? "",
      "icecast-auth-timelimit:",
      "x-icecast-auth-timelimit"
    )
  {
    kh = kh,
    listener_add = options?.listener_add ?? (options?.client_add ?? ""),
    listener_remove =
      options?.listener_remove ?? (options?.client_remove ?? ""),
    username = options?.username ?? "",
    password = options?.password ?? "",
    old_auth = header_line(auth.old),
    new_auth = lower(auth.new),
    old_timelimit = header_name(timelimit.old),
    new_timelimit = lower(timelimit.new),
    header_message = lower(options?.header_message ?? "x-icecast-auth-message"),
    header_alter_action =
      lower(options?.header_alter_action ?? "x-icecast-auth-alter-action"),
    header_alter_argument =
      lower(options?.header_alter_argument ?? "x-icecast-auth-alter-argument"),
    action_add = options?.action_add ?? "listener_add",
    action_remove = options?.action_remove ?? "listener_remove",
    headers =
      list.filter(
        fun (name) -> name != "",
        list.map(
          string.trim,
          string.split(separator=",", options?.headers ?? "")
        )
      ),
    header_prefix = options?.header_prefix ?? (kh ? "ClientHeader-" : ""),
    timeout = options?.timeout ?? 6.,
    presume_innocent = options?.presume_innocent ?? false,
    on_error_wait = options?.on_error_wait ?? (kh ? 30. : 0.)
  }
end
