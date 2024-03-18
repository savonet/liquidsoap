log.level := 4

tls = http.transport.tls(certificate="./ssl.cert", key="./ssl.key")

ssl = http.transport.ssl(certificate="./ssl.cert", key="./ssl.key")

port = 1442

s = sine()

output.icecast(port=port, mount="tls_test", transport=tls, %vorbis, s)

i = input.harbor(buffer=2., port=port, transport=ssl, "tls_test")

i = source.on_track(i, fun (_) -> test.pass())

output.dummy(fallible=true, i)
