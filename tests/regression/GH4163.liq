first_blank =
  metadata.map(
    id="first_blank", fun (_) -> [("title", "first_blank")], blank(duration=3.)
  )

after_blank =
  metadata.map(
    id="after_blank", fun (_) -> [("title", "after_blank")], sine(duration=10.)
  )

blank_sequence = sequence(id="blank_sequence", [first_blank, after_blank])

blank_strip =
  blank.strip(id="blank_strip", max_blank=5., start_blank=true, blank_sequence)

fallback_sine =
  metadata.map(id="fallback_sine", fun (_) -> [("title", "fallback")], sine())

s =
  fallback(
    id="main_source", track_sensitive=false, [blank_strip, fallback_sine]
  )

# We want to make sure that blank.strip keeps eating the blank source before switching back:
expected_titles = ["fallback", "after_blank"]
seen_titles = ref([])

def on_metadata(m) =
  seen_titles := [...seen_titles(), m["title"]]
  if
    seen_titles() == expected_titles
  then
    test.pass()
  elsif list.length(seen_titles()) == 2 then test.fail()
  end
end

s = source.on_metadata(id="on_meta", s, on_metadata)

clock.assign_new(sync='none', [s])

output.dummy(s)
