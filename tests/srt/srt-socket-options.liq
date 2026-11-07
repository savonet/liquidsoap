# An option set in on_socket "listen" takes effect: the latency is read back
# on both ends (spec/connection.md §3).
%include "../test.liq"
%include "helpers.inc"

port = 19536
i = input.srt(port=port)
i.on_socket(
  synchronous=true,
  fun (~mode, s) -> if mode == "listen" then s.set_latency(500) end
)
output.dummy(fallible=true, i)
o = output.srt(port=port, host="127.0.0.1", %wav, sine())

def run() =
  wait_until(
    what="both ends",
    {i.sockets() != [] and o.sockets() != []}
  )
  let (_, input_socket) = list.hd(i.sockets())
  let (_, output_socket) = list.hd(o.sockets())
  if
    input_socket.rcvlatency() != 500
  then
    test.fail(
      "input latency #{input_socket.rcvlatency()}"
    )
  end
  if
    output_socket.peerlatency() != 500
  then
    test.fail(
      "output peer latency #{output_socket.peerlatency()}"
    )
  end
  test.pass()
end

thread.run(delay=0.1, run)
