server = icecast.server({source_password = "change-me"})

server.on_source_connect(
  fun (r) ->
    log.important(
      "Source connected on mount #{r.mount}"
    )
)

server.on_source_disconnect(
  fun (r) ->
    log.important(
      "Source disconnected from mount #{r.mount}"
    )
)
