# See: https://github.com/savonet/liquidsoap/issues/5319
# Segments used to be anchored on the previous split position instead of the
# `segment_duration` grid, so their boundaries drifted backwards. The drift
# rate depends on the encoder's frame size, which made HLS variants using
# different codecs diverge from each other.

tmp_dir = file.temp_dir("tmp")
on_cleanup({file.rmdir(tmp_dir)})

s = sine()
clock.assign_new(sync="none", [s])

segments = 10
segment_duration = 2.
total = ref(0.)
count = ref(0)

def segment_name(m) =
  if
    0 < m.position
  then
    total := total() + m.duration
    ref.incr(count)
    if
      count() == segments
    then
      expected = float(segments) * segment_duration

      # A single segment can be off by one frame, the total cannot drift.
      if
        abs(total() - expected) <= frame.duration()
      then
        test.pass()
      else
        test.fail(
          "#{segments} segments last #{total()}s, expected #{expected}s"
        )
      end
    end
  end
  "#{m.stream_name}_#{m.position}.#{m.extname}"
end

output.file.hls(
  segment_name=segment_name,
  segments=4,
  segment_duration=segment_duration,
  tmp_dir,
  [("stream", %ffmpeg(format = "mpegts", %audio(codec = "aac")))],
  s
)
