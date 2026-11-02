# The fallback of the mount at `path` under the parameters `p`: the run-time
# overlay's, else its parameters', `null` when it has none or its parameters
# are invalid.
# @flag hidden
def icecast.server.mounts.fallback_of(state, p, path) =
  overlay = list.assoc.nullable(path, state.overlay.get())
  if
    null.defined(overlay)
  then
    mount = null.get(overlay)
    mount == "" ? null : {mount = (mount : string?), source = null}
  else
    try
      icecast.server.parameters.mount(p, path).fallback
    catch err do
      icecast.server.log.important(
        state,
        "Ignoring the fallback of #{path}: #{err.message}"
      )
      null
    end
  end
end

# The fallback chain of `path` (mounts.md §6.1): `mounts`, the fallback mounts
# in order, at most 10 and stopping at a loop; `source`, a Liquidsoap source
# that ends it; `past_bound`, whether it goes on past 10 mounts.
# @flag hidden
def icecast.server.mounts.chain(state, p, path) =
  def rec walk(mounts, current) =
    fallback = icecast.server.mounts.fallback_of(state, p, current)
    next = null.defined(fallback) ? null.get(fallback).mount : null
    if
      not null.defined(fallback)
    then
      {mounts = mounts, source = null, past_bound = false}
    elsif
      null.defined(null.get(fallback).source)
    then
      {mounts = mounts, source = null.get(fallback).source, past_bound = false}
    elsif
      list.mem(null.get(next), [path, ...mounts])
    then
      {mounts = mounts, source = null, past_bound = false}
    elsif
      list.length(mounts) >= 10
    then
      {mounts = mounts, source = null, past_bound = true}
    else
      walk([...mounts, null.get(next)], null.get(next))
    end
  end

  walk([], path)
end

# What the mount `entry` follows while it has no feeder (mounts.md §6.1): the
# source and identity of the first element of its chain that has something to
# send and matches its format, `null` when there is none.
# @flag hidden
def icecast.server.mounts.resolve(state, p, entry) =
  chain = icecast.server.mounts.chain(state, p, entry.path)
  def rec first(mounts) =
    list.case(
      mounts,
      null.map(fun (s) -> {source = s, identity = []}, chain.source),
      fun (path, rest) ->
        begin
          target = icecast.server.mounts.find(state, path)
          feeder = null.defined(target) ? null.get(target).feeder.get() : null
          if
            not null.defined(feeder)
          then
            first(rest)
          elsif
            icecast.server.mounts.matches(entry.format, null.get(feeder).format)
          then
            {
              source = null.get(feeder).source,
              identity = null.get(feeder).identity
            }
          else
            icecast.server.log.info(
              state,
              "Mount #{entry.path}: skipping fallback #{path} of another format."
            )
            first(rest)
          end
        end
    )
  end

  first(chain.mounts)
end

# The first mount of the fallback chain of `path` with an output and below its
# own `max_listeners` (listeners.md §4), as `{path}`, `null` when there is
# none; a full mount without `fallback_when_full` ends the search.
# @flag hidden
def icecast.server.mounts.full_target(state, p, path) =
  def rec first(mounts) =
    list.case(
      mounts,
      null,
      fun (target, rest) ->
        begin
          entry = icecast.server.mounts.find(state, target)
          mount =
            try
              icecast.server.parameters.mount(p, target)
            catch err do
              icecast.server.log.debug(
                state,
                "Mount #{target} has no listener limit: #{err.message}"
              )
              null
            end
          full =
            null.defined(entry)
            and null.defined(mount)
            and null.defined(null.get(mount).max_listeners)
            and null.get(entry).stats.current() >=
              null.get(null.get(mount).max_listeners)
          if
            null.defined(entry) and not full
          then
            {path = target}
          elsif
            full and not null.get(mount).fallback_when_full
          then
            null
          else
            first(rest)
          end
        end
    )
  end

  first(icecast.server.mounts.chain(state, p, path).mounts)
end

# Points every mount at its fallback, then shuts down the mounts left with
# nothing to send: no feeder, no fallback, no linger. Called under the lock.
# @flag hidden
def icecast.server.mounts.refresh(state) =
  p = state.params.get()
  list.iter(
    fun (entry) ->
      entry.fallback.set(icecast.server.mounts.resolve(state, p, entry)),
    state.mounts.all()
  )
  def is_idle(entry) =
    not null.defined(entry.feeder.get())
    and not null.defined(entry.fallback.get())
    and not null.defined(entry.lingering.get())
  end

  idle = state.mounts.remove(is_idle)
  if
    state.compatibility != "kh"
  then
    state.overlay.set(
      list.filter(
        fun (o) -> not list.exists(fun (e) -> e.path == fst(o), idle),
        state.overlay.get()
      )
    )
  end
  list.iter(
    fun (entry) ->
      begin
        icecast.server.log.info(
          state,
          "Mount #{entry.path} has nothing to send, shutting it down."
        )
        entry.output.shutdown()
      end,
    idle
  )
end
