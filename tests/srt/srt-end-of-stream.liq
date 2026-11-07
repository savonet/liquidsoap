# The end of a stream loses no byte: everything sent before the close reaches
# the decoder, as the dump shows (spec/input.md §4).
%include "../test.liq"
%include "helpers.inc"

port = 19520
i = input.srt(port=port, dump="received.ts")
ended = ref(false)
i.on_disconnect(synchronous=true, fun () -> ended := true)
output.dummy(fallible=true, i)

def run() =
  code =
    run_peer(
      "send --port #{port} --file tone.ts --then close"
    )
  if
    code != 0
  then
    test.fail(
      "sender: no connection"
    )
  end
  wait_until(
    what="the end of the stream",
    {ended()}
  )
  sent = file.contents("tone.ts")
  received = file.contents("received.ts")
  if
    received != sent
  then
    test.fail(
      "received #{string.length(encoding="ascii", received)} of #{
        string.length(encoding="ascii", sent)
      } bytes"
    )
  end
  test.pass()
end

thread.run(delay=0.1, run)
