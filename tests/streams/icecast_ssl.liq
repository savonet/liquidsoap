transport = http.transport.ssl(certificate="./ssl.cert", key="./ssl.key")

port = random.int(min=8000, max=10000)

s = sine()

output.icecast(port=port, mount="ssl_test", transport=transport, %vorbis, s)

i = input.harbor(buffer=2., port=port, transport=transport, "ssl_test")

i = source.on_track(i, fun (_) -> test.pass())

output.dummy(fallible=true, i)
