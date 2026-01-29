settings.decoder.decoders := ["srt"]

s = single("../media/test-subtitle.srt")
s = once(s)

count = ref(0)

def print_subtitle({start_time, end_time, forced, format, text}) =
  print(
    "Subtitle: #{start_time}->#{end_time}: #{text} (format: #{format}, forced: #{
      forced
    })"
  )
  ref.incr(count)

  if 3 < count() then test.pass() end
end

s = on_subtitle(print_subtitle, s)

clock.assign_new(sync='none', [s])
output.dummy(fallible=true, s)
