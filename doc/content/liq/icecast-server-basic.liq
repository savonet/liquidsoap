server = icecast.server()

# React when a source connects
server.on_connect(
  fun (r) ->
    log.important(
      "Source connected on mount #{r.mount}"
    )
)

# React when a source disconnects
server.on_disconnect(
  fun (r) ->
    log.important(
      "Source disconnected from mount #{r.mount}"
    )
)
