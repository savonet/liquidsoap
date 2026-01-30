settings.decoder.decoders := ["ffmpeg"]

s = single("../media/test-subtitle.srt")
s = sequence([s, s, fallback([])])

count = ref(0)

def print_subtitle(
  {
    absolute_start_time,
    absolute_end_time,
    position,
    start_time,
    end_time,
    forced,
    format,
    text
  }
) =
  print(
    "Subtitle #{count()}: #{absolute_start_time}->#{absolute_end_time} (relative \
     to #{position}: #{start_time}->#{end_time}): #{text} (format: #{format}, \
     forced: #{forced})"
  )
  ref.incr(count)

  if
    7 == count()
  then
    if absolute_start_time == 18. then test.pass() else test.fail() end
  end
end

s = on_subtitle(print_subtitle, s)

clock.assign_new(sync='none', [s])
output.dummy(fallible=true, s)
