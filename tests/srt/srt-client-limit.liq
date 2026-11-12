# The client limit is exact: with max_clients = 2, three callers give exactly
# two live peers. Admission at handshake is advisory, so a third caller may
# see its handshake complete before it is closed (spec/listener.md §3, §4).
%include "../test.liq"
%include "helpers.inc"

port = 19532
o = output.srt(mode="listener", port=port, max_clients=2, %wav, sine())
codes = ref([])

def run() =
  for _ = 1 to 3 do
    thread.run(
      fun () ->
        begin
          code =
            run_peer(
              "receive --port #{port} --duration 4"
            )
          atomic({codes := code::codes()})
        end
    )

  end
  wait_until(
    what="two peers",
    {list.length(o.sockets()) == 2}
  )
  thread.pause(1.)
  if
    list.length(o.sockets()) != 2
  then
    test.fail(
      "#{list.length(o.sockets())} peers"
    )
  end
  wait_until(
    what="every caller",
    {list.length(codes()) == 3}
  )
  test.pass()
end

thread.run(delay=0.1, run)
