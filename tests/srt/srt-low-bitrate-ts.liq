# Payloads the delay bound releases from an MPEG-TS stream are whole TS
# packets, so every payload starts on a sync byte (spec/output.md §10).
%include "../test.liq"
%include "helpers.inc"

port = 19599

# At 32 kbit/s, MP2 in MPEG-TS sits near one payload per bound.
output.srt(
  mode="listener",
  port=port,
  max_payload_delay=0.1,
  %ffmpeg(format = "mpegts", %audio(codec = "mp2", b = "32k")),
  sine()
)

def check() =
  code =
    run_peer(
      "receive --port #{port} --duration 4 --sizes sizes.txt --bytes stream.ts"
    )
  if
    code != 0
  then
    test.fail(
      "peer did not connect, status #{code}"
    )
  end
  sizes = payload_sizes("sizes.txt")
  data = file.contents("stream.ts")
  short = list.filter(fun (s) -> s < 1316, sizes)
  if
    short == []
  then
    test.fail(
      "no payload shorter than payload_size among #{list.length(sizes)}: the \
       bound never released one"
    )
  end
  def rec misaligned(offset, sizes) =
    if
      sizes == []
    then
      []
    else
      size = list.hd(sizes)
      rest = misaligned(offset + size, list.tl(sizes))
      if
        size mod 188 != 0 or string.nth(data, offset) != 0x47
      then
        (offset, size)::rest
      else
        rest
      end
    end
  end
  wrong = misaligned(0, sizes)
  if
    wrong != []
  then
    test.fail(
      "payloads not made of whole TS packets (offset, size): #{wrong}"
    )
  end
  test.pass()
end

thread.run(delay=0.1, check)
