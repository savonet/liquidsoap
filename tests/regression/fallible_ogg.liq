port = 9301

s = playlist("../media")

output.icecast(fallible=true, port=port, mount="test.flac", %ogg(%flac), s)

s = input.harbor("test.flac", buffer=1., port=port)

output.dummy(on_start=test.pass, fallible=true, s)
