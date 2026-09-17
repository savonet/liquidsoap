server = icecast.server(password="hackme")

server.on_listener_connect(
  fun (listener) ->
    log.important(
      "Listener #{listener.id} (#{listener.ip}) tuned in to #{listener.mount}"
    )
)

server.on_listener_disconnect(
  fun (listener) ->
    log.important(
      "Listener #{listener.id} left #{listener.mount} after #{
        int(listener.duration)
      }s and #{listener.bytes_sent} bytes"
    )
)

server.on_metadata(
  fun (update) ->
    log.important(
      "Now playing on #{update.mount} for #{update.listeners} listener(s): #{
        update.metadata["title"]
      }"
    )
)
