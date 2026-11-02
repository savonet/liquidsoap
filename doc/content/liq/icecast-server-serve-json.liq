icecast.server(
  {
    source_password = "change-me",
    status_html =
      fun (_) ->
        "<!DOCTYPE html><html><body>Not found.</body></html>",
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
              stats.mounts
            )
          json.stringify(compact=true, {mounts = mounts})
        end
  }
)
