# Ready-made access decisions of icecast.server (listeners.md §7.1 to §7.3):
# `icecast.server.decision.access_lists` and `icecast.server.decision.htpasswd`
# are public; `icecast.server.connections_per_user` is the per-user limit the
# configuration translation composes after authentication.
let icecast.server.access = ()

# A function returning `parse(lines)` of the file at `path`, without blank and
# `#` lines. The modification time is checked at most every `interval`
# seconds and the file read again when it changed; a file that cannot be read
# keeps the lines last read, none at first, and is logged once.
# @flag hidden
def icecast.server.access.watch(~interval, parse, path) =
  state =
    ref(
      {
        checked_at = 0. - infinity,
        mtime = -1.,
        parsed = parse([]),
        failed = false
      }
    )

  def lines() =
    list.filter(
      fun (line) -> line != "" and not string.contains(prefix="#", line),
      list.map(string.trim, file.lines(path))
    )
  end

  def check() =
    s = state()
    try
      mtime = file.mtime(path)
      parsed = mtime == s.mtime ? s.parsed : (parse(lines()))
      state :=
        {checked_at = time(), mtime = mtime, parsed = parsed, failed = false}
    catch err do
      if
        not s.failed
      then
        log.important(
          label="icecast.server",
          "Cannot read #{path}, keeping its last lines: #{err.message}"
        )
      end
      state := {...s, checked_at = time(), failed = true}
    end
  end

  fun () ->
    begin
      if time() - state().checked_at >= interval then check() end
      state().parsed
    end
end

# The regular expression of a wildcard pattern: `*`, `?` and `[...]` (`[!...]`
# negated), every other character literal, as a list: empty, logged, for a
# pattern that is not one, such as an unclosed `[`.
# @flag hidden
def icecast.server.access.pattern(~ignore_case=false, glob) =
  escaped = r/[\\.^$|()+{}\/]/g.replace(fun (c) -> "\\#{c}", glob)
  translated =
    r/\[!/g.replace(
      fun (_) -> "[^",
      r/\?/g.replace(fun (_) -> ".", r/\*/g.replace(fun (_) -> ".*", escaped))
    )
  try
    [regexp(flags=ignore_case ? ["i"] : [], "^#{translated}$")]
  catch err do
    log.important(
      label="icecast.server",
      "Ignoring access list entry #{string.quote(glob)}: #{err.message}"
    )
    []
  end
end

# Whether `value` matches one of the patterns of the nullable `patterns`.
# @flag hidden
def icecast.server.access.matches(patterns, value) =
  if
    null.defined(patterns)
  then
    patterns = null.get(patterns)
    list.exists(fun (r) -> r.test(value), patterns())
  else
    false
  end
end

# A listener decision refusing with `403` by address and by `User-Agent`. An
# address in the deny list is refused; otherwise, when an allow list is given,
# an address not in it is refused. Entries are addresses or wildcard patterns
# (`*`, `?`, `[...]`), one per line, `#` lines ignored; a file is read again
# when it changes.
# @category Internet
# @flag extra
# @param ~deny File of addresses to refuse.
# @param ~allow File of the only addresses to admit.
# @param ~agents File of `User-Agent` patterns to refuse, matched without case.
# @param ~check_interval Seconds between checks of a file's modification time.
def icecast.server.decision.access_lists(
  ~deny=null,
  ~allow=null,
  ~agents=null,
  ~check_interval=10.
) =
  def patterns(~ignore_case=false, path) =
    icecast.server.access.watch(
      interval=check_interval,
      fun (lines) ->
        list.flatten(
          list.map(
            fun (line) ->
              icecast.server.access.pattern(ignore_case=ignore_case, line),
            lines
          )
        ),
      path
    )
  end

  deny = null.map(fun (path) -> patterns(path), deny)
  allow = null.map(fun (path) -> patterns(path), allow)
  agents = null.map(fun (path) -> patterns(ignore_case=true, path), agents)
  matches = icecast.server.access.matches
  fun (request) ->
    begin
      agent = list.assoc.nullable("user-agent", request.headers)
      refused =
        matches(deny, request.address)
        or (null.defined(allow) and not matches(allow, request.address))
        or (null.defined(agent) and matches(agents, null.get(agent)))
      {refuse = refused ? icecast.server.refusal(403) : null}
    end
end

# Whether `password` matches the htpasswd `hash` of `user`. A hash other than
# a 32-hexadecimal-digit MD5 never matches.
# @flag hidden
def icecast.server.access.verify(~user, password, hash) =
  if
    r/^[0-9a-fA-F]{32}$/.test(hash)
  then
    string.case(lower=true, hash) == string.digest(password)
  else
    log.important(
      label="icecast.server",
      "Unsupported htpasswd hash format for user #{string.quote(user)}."
    )
    false
  end
end

# A listener decision authenticating HTTP Basic credentials against a file of
# `user:hash` lines, read again when it changes. A missing or wrong credential
# refuses with `401` and the default challenge.
# @category Internet
# @flag extra
# @param ~check_interval Seconds between checks of the file's modification time.
# @param users_file htpasswd file.
def icecast.server.decision.htpasswd(~check_interval=10., users_file) =
  def parse(lines) =
    list.filter_map(
      fun (line) ->
        string.contains(substring=":", line)
          ? string.split.first(separator=":", line)
          : null,
      lines
    )
  end

  users =
    icecast.server.access.watch(interval=check_interval, parse, users_file)
  fun (request) ->
    begin
      hash = list.assoc.nullable(request.user, users())
      valid =
        request.user != ""
        and null.defined(hash)
        and icecast.server.access.verify(
          user=request.user,
          request.password,
          null.get(hash)
        )
      {refuse = valid ? null : (icecast.server.refusal(401))}
    end
end

# Per-user connection limit: refuses a listener whose user already has `n`
# admitted listeners on the mount, with `429` under `"icecast-2.5"` and
# `"rsas"`, `403` under the others. `0` is no limit.
# @flag hidden
def icecast.server.connections_per_user(n) =
  fun (request) ->
    begin
      status =
        list.mem(request.compatibility, ["icecast-2.5", "rsas"]) ? 429 : 403
      limited = n > 0 and request.user_listeners(request.user) >= n
      {refuse = limited ? icecast.server.refusal(status) : null}
    end
end
