port = 9301

s = playlist("../media")

output.icecast(fallible=true, port=port, mount="test.flac", %ogg(%flac), s)

s = input.harbor("test.flac", buffer=1., port=port)

o = output.dummy(fallible=true, s)
o.on_start(synchronous=true, test.pass)
