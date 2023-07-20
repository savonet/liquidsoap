log.level := 4

transport = http.transport.tls(
  certificate="./ssl.cert",
  key="./ssl.key"
)

port = random.int(min=8000, max=10000)

thread.run(delay=1., fun () -> begin
  s = sine()

  output.icecast(
    port=port,
    mount="tls_test",
    transport=transport,
    %vorbis,
    s
  )
end)

i = input.harbor(
  buffer=2.,
  port=port,
  transport=transport,
  "tls_test")

i = source.on_track(i, fun (_) -> test.pass())

output.dummy(fallible=true, i)
