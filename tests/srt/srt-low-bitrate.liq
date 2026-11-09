# A low-bitrate stream is never held back past the delay bound: FLAC
# compresses silence to a few hundred bytes per second, far below one
# payload per bound (spec/output.md §5, KC21).
%include "../test.liq"
%include "helpers.inc"

port = 19598
bound = 0.1
duration = 4.

# Scheduling margin over the bound plus one frame.
margin = 0.4

output.srt(
  mode="listener",
  port=port,
  max_payload_delay=bound,
  %ffmpeg(format = "flac", %audio(codec = "flac")),
  blank()
)

def arrivals(file_name) =
  list.map(
    fun (l) ->
      begin
        let [t, n] =
          string.split(
            separator=" ",
            l
          )
        (float_of_string(t), int_of_string(n))
      end,
    lines(file_name)
  )
end

def largest_gap(times) =
  def rec gap(times, worst) =
    if
      list.length(times) < 2
    then
      worst
    else
      let [a, b] = list.prefix(2, times)
      gap(list.tl(times), max(worst, b - a))
    end
  end
  gap(times, 0.)
end

def check() =
  code =
    run_peer(
      "receive --port #{port} --duration #{duration} --times times.txt"
    )
  if
    code != 0
  then
    test.fail(
      "peer did not connect, status #{code}"
    )
  end
  received = arrivals("times.txt")
  if
    list.length(received) < 10
  then
    test.fail(
      "only #{list.length(received)} payloads in #{duration} seconds"
    )
  end

  # The FLAC header and its padding arrive first; silence follows.
  start = fst(list.hd(received)) + 1.
  silence = list.filter(fun (p) -> fst(p) > start, received)
  silence_bytes = list.fold(fun (total, p) -> total + snd(p), 0, silence)
  if
    silence_bytes > 1316
  then
    test.fail(
      "#{silence_bytes} bytes of silence in 3 seconds: the stream is not low \
       bitrate"
    )
  end
  oversized = list.filter(fun (p) -> snd(p) > 1316, received)
  if
    oversized != []
  then
    test.fail(
      "payloads larger than payload_size: #{oversized}"
    )
  end

  # The end of the peer's reading window counts as an arrival, so that bytes
  # held until the end count too. The window starts as the header arrives.
  window_end = fst(list.hd(received)) + duration
  gap = largest_gap(list.append(list.map(fst, received), [window_end]))
  if
    gap > bound + margin
  then
    test.fail(
      "payloads held back: #{gap}s between two arrivals, bound #{bound}s"
    )
  end
  test.pass()
end

thread.run(delay=0.1, check)
