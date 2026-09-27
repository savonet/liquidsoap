radio = mksafe(playlist("~/music"))

o =
  output.icecast(
    %mp3,
    host="icecast.example.org",
    password="hackme",
    mount="/radio.mp3",
    radio
  )

o.on_connect(
  synchronous=false,
  fun () ->
    log.important(
      "Connected to icecast"
    )
)

o.on_disconnect(
  synchronous=false,
  fun () ->
    log.important(
      "Disconnected from icecast"
    )
)

o.on_error(
  synchronous=true,
  fun (~restart_in, err) ->
    begin
      log.important(
        "Icecast error: #{error.message(err)}, reconnecting in 10 seconds"
      )
      restart_in(10.)
    end
)
