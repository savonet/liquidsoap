# The statistics snapshot of observability.md §5.
# @flag hidden
def icecast.server.snapshot(state) =
  p = state.params.get()
  def mount_stats(entry) =
    mount =
      try
        icecast.server.parameters.mount(p, entry.path)
      catch err do
        icecast.server.log.debug(
          state,
          "Mount #{entry.path} has no parameters in stats: #{err.message}"
        )
        null
      end
    fallback = icecast.server.mounts.fallback_of(state, p, entry.path)
    icecast.server.stats.of_mount(
      listener=fun (entry, l) ->
        icecast.server.listeners.record(state, p, entry, l),
      listeners=icecast.server.listeners.current(entry),
      hidden=null.defined(mount) and null.get(mount).hidden,
      fallback=null.defined(fallback)
        ? (null.get(fallback).mount ?? "(source)")
        : null,
      entry
    )
  end

  state.counters.get().{mounts = list.map(mount_stats, state.mounts.all())}
end
