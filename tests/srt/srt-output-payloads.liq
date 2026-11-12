# Payloads are exactly payload_size bytes, and a clean stop delivers the
# encoder's trailer (spec/output.md §5, §6, §10).
%include "../test.liq"
%include "helpers.inc"

port = 19500

# The delay bound is off: a quiet Vorbis stream may fall below one payload
# per bound and release shorter payloads, which srt-low-bitrate checks.
o =
  output.srt(
    mode="listener",
    port=port,
    max_payload_delay=null,
    %ogg(%vorbis),
    sine()
  )

def stop_after_streaming() =
  wait_until(
    what="a connected peer",
    {list.length(o.sockets()) > 0}
  )
  thread.pause(2.)
  o.stop()
end

def check() =
  code =
    run_peer(
      "receive --port #{port} --sizes sizes.txt --bytes stream.ogg"
    )
  if
    code != 0
  then
    test.fail(
      "peer did not connect, status #{code}"
    )
  end
  sizes = payload_sizes("sizes.txt")
  n = list.length(sizes)
  if
    n < 3
  then
    test.fail(
      "only #{n} payloads received"
    )
  end
  wrong = list.filter(fun (s) -> s != 1316, list.prefix(n - 1, sizes))
  if
    wrong != []
  then
    test.fail(
      "payloads of the wrong size: #{wrong}"
    )
  end
  last = list.last(sizes)
  if
    last < 1 or last > 1316
  then
    test.fail(
      "last payload of #{last} bytes"
    )
  end
  if
    not ends_with_ogg_eos(file.contents("stream.ogg"))
  then
    test.fail(
      "the stream does not end with the encoder's trailer"
    )
  end
  test.pass()
end

thread.run(delay=0.1, check)
thread.run(stop_after_streaming)
