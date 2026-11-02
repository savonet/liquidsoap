def parameters() =
  cache = icecast.server.preroll_cache(directory="/srv/radio/prerolls")
  auth =
    icecast.server.decision.url_auth(
      cache=cache,
      {
        listener_add = "http://localhost:8080/listener/add",
        listener_remove = "http://localhost:8080/listener/remove"
      }
    )
  {source_password = "change-me", mount = fun (_) -> {listener_decision = auth}}
end

server = icecast.server(parameters())
