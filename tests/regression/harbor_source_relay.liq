# A source connecting to harbor is handed the socket, which the handler signals
# the same way it signals an error page. Connecting is not enough to prove that
# worked: the stream has to arrive and decode.
s = input.harbor("test", port=5359, password="hackme")
output.dummy(fallible=true, s)

output.icecast(
  %mp3,
  host="localhost",
  port=5359,
  password="hackme",
  mount="test",
  mksafe(sine())
)

def check() =
  if
    s.buffer_length() > 0.
  then
    test.pass()
  else
    test.fail(
      "no stream reached the mount"
    )
  end
end

thread.run(delay=5., check)
