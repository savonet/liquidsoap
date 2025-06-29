radio = single("../media/@wav[stereo].wav")

append = request.queue()

radio = fallback(track_sensitive=true, [append, radio])

thread.run(
  delay=3., {append.push.uri("annotate:foo=bla:../media/@wav[stereo].wav")}
)

radio.on_metadata(
  synchronous=true, fun (m) -> if m["foo"] == "bla" then test.pass() end
)

output.dummy(fallible=true, radio)
