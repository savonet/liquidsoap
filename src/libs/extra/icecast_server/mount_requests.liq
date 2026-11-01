# Listener requests for a path without a mount output (mounts.md §1.1 "A
# configured mount with no output", listeners.md §1 step 0). The server
# registers `icecast.server.mount_requests.register` on its port: a mount
# output's literal route beats this regexp route, so it sees only the paths
# that have no output, and those whose output is still starting.
let icecast.server.mount_requests = ()

# The first feeder of the fallback `chain`; a Liquidsoap source ending it is
# not a template, since its encoding is unknown.
# @flag hidden
def icecast.server.mount_requests.chain_feeder(state, chain) =
  def rec first(mounts) =
    list.case(
      mounts,
      null,
      fun (path, rest) ->
        begin
          entry = icecast.server.mounts.find(state, path)
          feeder = null.defined(entry) ? null.get(entry).feeder.get() : null
          null.defined(feeder) ? feeder : first(rest)
        end
    )
  end

  first(chain.mounts)
end

# Waits until the mount output of `entry` serves, for at most 5 seconds.
# @flag hidden
def icecast.server.mount_requests.wait_started(entry) =
  deadline = time() + 5.
  def rec wait() =
    if
      not entry.output.is_started() and time() < deadline
    then
      thread.pause(0.05)
      wait()
    end
  end

  wait()
end

# Creates the output of `path` through its live fallback, under the lock:
# `past_bound` tells whether its chain goes past 10 mounts, `created` whether
# the mount has an output.
# @flag hidden
def icecast.server.mount_requests.create(state, path) =
  icecast.server.locked(
    state,
    fun () ->
      begin
        p = state.params.get()
        chain = icecast.server.mounts.chain(state, p, path)
        feeder = icecast.server.mount_requests.chain_feeder(state, chain)
        if
          not null.defined(icecast.server.mounts.find(state, path))
          and null.defined(feeder)
          and not (state.compatibility == "rsas" and chain.past_bound)
          and not icecast.server.sources.is_reserved(path)
        then
          state.mounts.add(
            icecast.server.mounts.create(state, p, path, null.get(feeder))
          )
          icecast.server.mounts.refresh(state)
        end
        {
          past_bound = chain.past_bound,
          created = null.defined(icecast.server.mounts.find(state, path))
        }
      end
  )
end

# The reply to a listener request on a path without a serving output: the
# mount is created through its live fallback and the listener redirected to
# it (mounts.md §1.1).
# @flag hidden
def icecast.server.mount_requests.handle(state, request) =
  path = request.path
  outcome = icecast.server.mount_requests.create(state, path)
  if
    state.compatibility == "rsas" and outcome.past_bound
  then
    http.response(
      status_code=503,
      status_message="Maximum fallback recursion limit reached"
    )
  elsif
    not outcome.created
  then
    http.response(status_code=404)
  else
    entry = icecast.server.mounts.find(state, path)
    if
      null.defined(entry)
    then
      icecast.server.mount_requests.wait_started(null.get(entry))
    end
    query = request.query_string == "" ? "" : "?#{request.query_string}"
    http.response(status_code=307, headers=[("Location", "#{path}#{query}")])
  end
end

# @flag hidden
def icecast.server.mount_requests.register(~transport, state) =
  harbor.http.register.simple.regexp(
    port=state.port,
    transport=transport,
    method="GET",
    r/^\/.*$/,
    fun (request) -> icecast.server.mount_requests.handle(state, request)
  )
end
