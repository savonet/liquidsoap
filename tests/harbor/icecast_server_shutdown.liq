%include "../test.liq"

# Shutting down while a source is connected must not hang (#5417).
def f() =
  server = icecast.server(port=3498)
  server.on_source_connect(fun (_) -> thread.run(delay=1., test.pass))
  output.icecast(%mp3, host="localhost", port=3498, mount="a", sine())
end

test.check(f)
