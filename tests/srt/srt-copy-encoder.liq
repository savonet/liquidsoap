# The output sends encoded content as it comes: copied packets, muxed again
# (spec/output.md §5).
%include "../test.liq"
%include "helpers.inc"

port = 19596
received = input.srt(port=port)
output.dummy(fallible=true, received)
output.srt(
  fallible=true,
  host="127.0.0.1",
  port=port,
  polling_delay=0.2,
  %ffmpeg(format = "mpegts", %audio.copy),
  single("tone.ts")
)

def run() =
  wait_until(
    what="decoded copied audio",
    {received.is_ready()}
  )
  test.pass()
end

thread.run(delay=0.1, run)
