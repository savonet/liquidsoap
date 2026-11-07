# disconnect() during an attempt leaves the caller idle, and connect() twice
# gives one peer (spec/caller.md §2, §4).
%include "../test.liq"
%include "helpers.inc"

port = 19528
o =
  output.srt(
    port=port,
    host="127.0.0.1",
    polling_delay=0.1,
    connection_timeout=0.3,
    %wav,
    sine()
  )
created = ref(0)
closed = ref(0)
connected = ref(0)
o.on_socket(
  synchronous=true,
  fun (~mode, _) ->
    if
      mode == "close"
    then
      closed := closed() + 1
    else
      created := created() + 1
    end
)
o.on_connect(synchronous=true, fun () -> connected := connected() + 1)

def run() =
  for _ = 1 to 100 do
    o.connect()
    thread.pause(random.float(min=0., max=0.4))
    o.disconnect()

  end
  wait_until(
    what="every attempt socket closed",
    {created() == closed()}
  )
  if
    o.sockets() != []
  then
    test.fail(
      "a socket is held after disconnect"
    )
  end
  if
    connected() != 0
  then
    test.fail(
      "on_connect fired with no listener"
    )
  end
  i = input.srt(port=port)
  output.dummy(fallible=true, i)
  o.connect()
  o.connect()
  wait_until(
    what="the connection",
    {list.length(i.sockets()) == 1}
  )
  o.connect()
  thread.pause(1.)
  if
    connected() != 1
  then
    test.fail(
      "#{connected()} connections"
    )
  end
  if
    list.length(i.sockets()) != 1
  then
    test.fail(
      "input holds #{list.length(i.sockets())} peers"
    )
  end
  if
    list.length(o.sockets()) != 1
  then
    test.fail(
      "output holds #{list.length(o.sockets())} peers"
    )
  end
  test.pass()
end

thread.run(delay=0.1, run)
