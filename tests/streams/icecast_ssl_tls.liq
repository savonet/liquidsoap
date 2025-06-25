log.level := 4
settings.icecast.prefer_address := "ipv4"

tls = http.transport.tls(certificate="./ssl.cert", key="./ssl.key")

ssl = http.transport.ssl(certificate="./ssl.cert", key="./ssl.key")

port = 1444

test.skip()

def on_wake_up() =
  s = sine()

  output.icecast(
    port=port,
    mount="tls_test",
    connection_timeout=0.1,
    timeout=0.1,
    transport=ssl,
    %vorbis,
    s
  )
end

i = input.harbor(buffer=2., port=port, transport=tls, "tls_test")

i.on_wake_up(on_wake_up)

i.on_track(synchronous=true, fun (_) -> test.pass())

output.dummy(fallible=true, i)
