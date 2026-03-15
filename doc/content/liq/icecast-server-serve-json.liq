icecast.server(
  serve_html=fun (_) -> "<!DOCTYPE html><html><body>Not found.</body></html>",
  serve_json=fun (stats) ->
    begin
      mounts =
        list.map(
          fun ((mount, s)) ->
            {
              mount = mount,
              name = s.name,
              listeners = list.length(s.listeners)
            },
          stats
        )
      json.stringify(compact=true, {mounts = mounts})
    end
)
