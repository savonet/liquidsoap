# The snapshot includes hidden mounts: a public page leaves them out.
def public_mounts(stats) =
  list.filter(fun (m) -> not m.hidden, stats.mounts)
end

icecast.server(
  {
    source_password = "change-me",
    status_html =
      fun (stats) ->
        "<!DOCTYPE html><html><body>#{list.length(public_mounts(stats))} streams \
         on air.</body></html>",
    status_json =
      fun (stats) ->
        begin
          mounts =
            list.map(
              fun (m) ->
                {
                  mount = m.mount,
                  name = m.name,
                  listeners = list.length(m.listeners)
                },
              public_mounts(stats)
            )
          json.stringify(compact=true, {mounts = mounts})
        end
  }
)
