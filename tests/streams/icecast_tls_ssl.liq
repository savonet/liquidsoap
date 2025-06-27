log.level := 4
settings.icecast.prefer_address := "ipv4"

test.skip()

tls = http.transport.tls(certificate="./ssl.cert", key="./ssl.key")

ssl = http.transport.ssl(certificate="./ssl.cert", key="./ssl.key")

port = 1442

test.skip()

def on_wake_up() =
  s = sine()

  output.icecast(
    port=port,
    mount="tls_test",
    connection_timeout=0.1,
    timeout=0.1,
    transport=tls,
    %vorbis,
    s
  )
end

i = input.harbor(buffer=2., port=port, transport=ssl, "tls_test")

i.on_wake_up(synchronous=true, on_wake_up)

i.on_track(synchronous=true, fun (_) -> test.pass())

output.dummy(fallible=true, i)
