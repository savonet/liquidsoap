# Listener decisions of icecast.server (listeners.md §2, §5, §7). For each
# listener request the server builds the decision's input with
# `icecast.server.decision.request` and calls the mount's `listener_decision`
# with it. An outcome that admitted is released exactly once: the server calls
# `icecast.server.decision.release` with the served session's record, or with
# `icecast.server.decision.unserved` when the admission ended before the
# listener was served. `icecast.server.decision.all` composes decisions.
let icecast.server.decision = ()

# `(user, password)` of a request's HTTP Basic credentials, `null` when it has
# none or they are malformed.
# @flag hidden
def icecast.server.decision.credentials(headers) =
  encoded =
    string.residual(
      prefix="Basic ",
      list.assoc(default="", "authorization", headers)
    )
  decoded =
    if
      null.defined(encoded)
    then
      try
        string.base64.decode(null.get(encoded))
      catch _ do
        ""
      end
    else
      ""
    end
  string.contains(substring=":", decoded)
    ? (string.split.first(separator=":", decoded) : (string * string)?)
    : null
end

# The listener request handed to a decision, from the harbor listener record:
# `address` is the resolved address, `listeners` the mount's count without
# this listener, and `user_listeners(user)` the count of the user's admitted
# listeners on the mount, called only for a non-empty user name.
# @flag hidden
def icecast.server.decision.request(
  ~server,
  ~compatibility,
  ~address,
  ~listeners,
  ~user_listeners,
  listener
) =
  let (user, password) =
    icecast.server.decision.credentials(listener.headers) ?? ("", "")
  {
    id = listener.id,
    mount = listener.uri,
    query = listener.query_string,
    user = user,
    password = password,
    address = address,
    headers = listener.headers,
    protocol = listener.protocol,
    listeners = listeners,
    user_listeners = fun (user) -> user == "" ? 0 : (user_listeners(user)),
    server = server,
    compatibility = compatibility
  }
end

# Release record of an admission that ended before its listener was served.
# @flag hidden
def icecast.server.decision.unserved(~listeners, ~metadata) =
  {duration = 0., bytes_sent = 0, listeners = listeners, metadata = metadata}
end

# Calls an outcome's nullable `on_release` with `release`, logging a failure.
# @flag hidden
def icecast.server.decision.release(on_release, release) =
  if
    null.defined(on_release)
  then
    on_release = null.get(on_release)
    try
      on_release(release)
    catch err do
      log.important(
        label="icecast.server",
        "Listener release failed: #{err.message}"
      )
    end
  end
end

# A listener decision composing `decisions` in order. The first that refuses
# or redirects wins, and those that admitted before it are released with a
# duration of 0. When all admit, the first `time_limit`, `preroll` and `user`
# given win, an empty preroll included, `exempt` is set if any set it, and
# every `on_release` is called. A decision after a rename sees the request with
# the renamed `user`.
# @category Internet
# @flag extra
# @param decisions Listener decisions, called in order.
def icecast.server.decision.all(decisions) =
  fun (request) ->
    begin
      time_limit = ref(null)
      preroll = ref(null)
      exempt = ref(false)
      user = ref(null)
      releases = ref([])

      # A decision does not know the mount's metadata: the release of an
      # earlier admission carries none.
      def release_admitted() =
        unserved =
          icecast.server.decision.unserved(
            listeners=request.listeners,
            metadata=[]
          )
        list.iter(
          fun (f) -> icecast.server.decision.release(f, unserved),
          releases()
        )
      end

      def admit(outcome) =
        if
          not null.defined(time_limit())
        then
          time_limit := outcome?.time_limit
        end
        if not null.defined(preroll()) then preroll := outcome?.preroll end
        if not null.defined(user()) then user := outcome?.user end
        if outcome?.exempt ?? false then exempt := true end
        if
          null.defined(outcome?.on_release)
        then
          releases := [...releases(), outcome?.on_release]
        end
      end

      def rec decide(decisions) =
        list.case(
          decisions,
          null,
          fun (decision, rest) ->
            begin
              outcome =
                try
                  decision(request.{user = user() ?? request.user})
                catch err do
                  release_admitted()
                  error.raise(err)
                end
              if
                null.defined(outcome?.refuse)
              then
                release_admitted()
                outcome?.refuse
              else
                admit(outcome)
                decide(rest)
              end
            end
        )
      end

      def on_release(release) =
        list.iter(
          fun (f) -> icecast.server.decision.release(f, release),
          releases()
        )
      end

      refuse = decide(decisions)
      admitted = not null.defined(refuse)
      {
        refuse = refuse,
        time_limit = admitted ? time_limit() : null,
        preroll = admitted ? preroll() : null,
        exempt = admitted and exempt(),
        user = admitted ? user() : null,
        on_release =
          admitted and not list.is_empty(releases()) ? on_release : null
      }
    end
end
