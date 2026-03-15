port = 4629

s = input.harbor(port=port, "bla")
s.on_disconnect(synchronous=true, test.pass)

output.dummy(fallible=true, s)

o = output.icecast(port=port, mount="bla", %mp3, sine())

s.on_connect(synchronous=false, fun (_) -> o.shutdown())
