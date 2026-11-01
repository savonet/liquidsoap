# Logs the warning of sources.md §3.3 while the server's or a live mount's
# source password is the default one.
# @flag hidden
def icecast.server.warn_default_password(state, p) =
  def is_default(entry) =
    try
      icecast.server.parameters.mount(p, entry.path).source_password == "hackme"
    catch err do
      log.debug(
        label=state.name,
        "Mount #{entry.path} skipped by the password check: #{err.message}"
      )
      false
    end
  end

  if
    getter.get(p.source_password) == "hackme"
    or list.exists(is_default, state.mounts.all())
  then
    log.severe(
      label=state.name,
      "The source password is the default \"hackme\": set source_password."
    )
  end
end

# Replaces the parameters of the running server (parameters.md §8), under the
# server's lock: the new record and the live mounts' records are validated
# before anything changes, and reloads run one at a time.
# @flag hidden
def icecast.server.reload(state, ~transport_reload, parameters) =
  def apply() =
    p = icecast.server.parameters.server(parameters)
    list.iter(
      fun (entry) -> ignore(icecast.server.parameters.mount(p, entry.path)),
      state.mounts.all()
    )
    old = state.params.get()
    state.params.set(p)
    state.counters.reloaded()
    list.iter(
      fun (change) ->
        if
          snd(change)
        then
          log.important(
            label=state.name,
            "Parameter #{fst(change)} changed: it is read at startup and is not \
             applied."
          )
        end,
      [
        ("name", old.name != p.name),
        ("port", old.port != p.port),
        ("bind_address", old?.bind_address != p?.bind_address),
        ("compatibility", old.compatibility != p.compatibility)
      ]
    )
    if
      null.defined(transport_reload)
    then
      reload = null.get(transport_reload)
      try
        reload()
      catch err do
        log.important(
          label=state.name,
          "Transport reload failed, keeping its certificate: #{err.message}"
        )
      end
    end
    state.overlay.set([])
    try
      icecast.server.mounts.refresh(state)
    catch err do
      log.important(
        label=state.name,
        "Reload: following the new fallbacks failed: #{err.message}"
      )
    end
    icecast.server.warn_default_password(state, p)
  end

  icecast.server.locked(state, apply)
end
