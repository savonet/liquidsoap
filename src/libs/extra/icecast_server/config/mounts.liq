# The `mount` callback of a translated file (icecast-config.md §5): a lookup
# among the entries mount_entry.liq read at load. A path takes the normal
# entry `select` picks, completed field by field with the matching default
# entry, and every mount composes the file's access lists first.

# Whether a pattern of `entry`'s `mount-name` matches `path`.
# @flag hidden
def icecast.server.config.matches(entry, path) =
  list.exists(fun (r) -> r.test(path), entry.patterns)
end

# The normal and the default entry of `path`, each `null` when none applies.
# Under `"kh"` normal names are patterns too: the exact name wins, then the
# lowest `priority`, then file order.
# @flag hidden
def icecast.server.config.select(~kh, entries, (path:string)) =
  def first(entries) =
    list.case(entries, null, fun (entry, _) -> entry)
  end

  normals = list.filter(fun (e) -> e.kind == "normal", entries)
  exact = first(list.filter(fun (e) -> e.name == path, normals))
  def lowest(best, entry) =
    null.defined(best) and null.get(best).priority <= entry.priority
      ? best
      : entry
  end

  normal =
    if
      kh and not null.defined(exact)
    then
      list.fold(
        lowest,
        null,
        list.filter(fun (e) -> icecast.server.config.matches(e, path), normals)
      )
    else
      exact
    end
  default =
    first(
      list.filter(
        fun (e) ->
          e.kind == "default"
          and (e.name == "" or icecast.server.config.matches(e, path)),
        entries
      )
    )
  {normal = normal, default = default}
end

# `decision` with every outcome field present, so that decisions of several
# helpers share a type.
# @flag hidden
def icecast.server.config.decision(decision) =
  fun (request) ->
    begin
      outcome = decision(request)
      {
        refuse = outcome?.refuse,
        time_limit = outcome?.time_limit,
        preroll = outcome?.preroll,
        exempt = outcome?.exempt,
        on_release = outcome?.on_release,
        user = outcome?.user
      }
    end
end

# The mount parameters of a normal entry completed with a default entry, both
# nullable; `decisions` come first in its listener decision.
# @flag hidden
def icecast.server.config.merge(
  ~cache,
  ~decisions,
  ~max_listeners,
  own,
  default
) =
  def preroll(location) =
    null.case(
      location,
      {[]},
      fun (location) -> [fun (_) -> {preroll = cache(location)}]
    )
  end

  # The request type of `icecast.server.decision.request`, written out: two
  # loaded records otherwise take the typechecker minutes to unify.
  def listener_decision(decisions) =
    (icecast.server.decision.all(decisions) :
      (
        {
          id: int,
          mount: string,
          query: string,
          user: string,
          password: string,
          address: string,
          headers: [(string * string)],
          protocol: string,
          listeners: int,
          user_listeners: (string)->int,
          server: {hostname: string, port: int, instance: string},
          compatibility: string
        }
      )->_
    )
  end

  decision = icecast.server.config.decision
  decisions =
    [
      ...list.map(decision, decisions),
      ...list.map(decision, own?.listener ?? (default?.listener ?? [])),
      ...list.map(decision, preroll(own?.intro ?? default?.intro)),
      ...list.map(decision, preroll(own?.preroll ?? default?.preroll))
    ]
  own = own?.values
  default = default?.values
  {
    source_user = own?.source_user ?? default?.source_user,
    source_password = own?.source_password ?? default?.source_password,
    source_decision = own?.source_decision ?? default?.source_decision,
    on_connect = own?.on_connect ?? default?.on_connect,
    on_disconnect = own?.on_disconnect ?? default?.on_disconnect,
    linger = own?.linger ?? default?.linger,
    max_listeners =
      own?.max_listeners ?? (default?.max_listeners ?? max_listeners),
    listener_decision =
      list.is_empty(decisions) ? null : (listener_decision(decisions)),
    max_listener_duration =
      own?.max_listener_duration ?? default?.max_listener_duration,
    fallback = own?.fallback ?? default?.fallback,
    fallback_when_full = own?.fallback_when_full ?? default?.fallback_when_full,
    hidden = own?.hidden ?? default?.hidden,
    max_history = own?.max_history ?? default?.max_history,
    health_password = own?.health_password ?? default?.health_password,
    burst_size = own?.burst_size ?? default?.burst_size,
    http_headers =
      [...(own?.http_headers ?? []), ...(default?.http_headers ?? [])],
    dump_file = own?.dump_file ?? default?.dump_file,
    dump_file_size_limit =
      own?.dump_file_size_limit ?? default?.dump_file_size_limit,
    dump_file_time_limit =
      own?.dump_file_time_limit ?? default?.dump_file_time_limit
  }
end

# The `mount` callback over `entries`. A path no entry applies to gets only
# the access lists and the root's `max-listeners`, or `null` without them.
# @flag hidden
def icecast.server.config.mount_callback(
  ~kh,
  ~cache,
  ~decisions,
  ~max_listeners,
  entries
) =
  fun (path) ->
    begin
      selected = icecast.server.config.select(kh=kh, entries, path)
      if
        not null.defined(selected.normal)
        and not null.defined(selected.default)
        and list.is_empty(decisions)
        and not null.defined(max_listeners)
      then
        null
      else
        icecast.server.config.merge(
          cache=cache,
          decisions=decisions,
          max_listeners=max_listeners,
          selected.normal,
          selected.default
        )
      end
    end
end
