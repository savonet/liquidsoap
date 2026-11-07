# A content type with decoder options reaches the stream decoder: raw PCM,
# which carries no header to probe (spec/input.md §4).
%include "../test.liq"
%include "helpers.inc"

port = 19576
received =
  input.srt(
    port=port,
    content_type="application/ffmpeg;format=s16le,ch_layout=mono,sample_rate=48000"
  )
output.dummy(fallible=true, received)
output.srt(
  fallible=true,
  host="127.0.0.1",
  port=port,
  polling_delay=0.2,
  %ffmpeg(format = "s16le", %audio(codec = "pcm_s16le", ac = 1, ar = 48000)),
  sine()
)

def run() =
  wait_until(
    what="decoded raw PCM",
    {received.is_ready()}
  )
  test.pass()
end

thread.run(delay=0.1, run)
