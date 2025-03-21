log.level := 4
settings.icecast.prefer_address := "ipv4"

transport = http.transport.tls(certificate="./ssl.cert", key="./ssl.key")

port = 1441

test.skip()

def on_wake_up() =
  s = sine()

  output.icecast(
    port=port,
    mount="tls_test",
    connection_timeout=0.1,
    timeout=0.1,
    transport=transport,
    %vorbis,
    s
  )
end

i = input.harbor(buffer=2., port=port, transport=transport, "tls_test")

i.on_wake_up(on_wake_up)

i = source.on_track(i, fun (_) -> test.pass())

output.dummy(fallible=true, i)
