transport = http.transport.ssl(certificate="./ssl.cert", key="./ssl.key")

port = 1443

def on_wake_up() =
  s = sine()

  output.icecast(
    port=port,
    mount="ssl_test",
    connection_timeout=0.1,
    timeout=0.1,
    transport=transport,
    %vorbis,
    s
  )
end

i = input.harbor(buffer=2., port=port, transport=transport, "ssl_test")

i.on_wake_up(on_wake_up)

i = source.on_track(i, fun (_) -> test.pass())

output.dummy(fallible=true, i)
