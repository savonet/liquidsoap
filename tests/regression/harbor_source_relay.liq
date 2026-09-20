%include "../test.liq"

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

# The first icecast connection can race the harbor port opening and wait out
# the 3s reconnect delay, so poll instead of checking at a fixed time.
deadline = time() + 20.

def check() =
  if
    s.buffer_length() > 0.
  then
    test.pass()
    (-1.)
  elsif
    time() > deadline
  then
    test.fail(
      "no stream reached the mount"
    )
    (-1.)
  else
    0.1
  end
end

thread.run.recurrent(delay=1., check)
