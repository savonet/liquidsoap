%include "../test.liq"

first_blank =
  metadata.map(
    insert_missing=true,
    id="first_blank",
    fun (_) -> [("title", "first_blank")],
    blank(duration=3.)
  )

after_blank =
  metadata.map(
    insert_missing=true,
    id="after_blank",
    fun (_) -> [("title", "after_blank")],
    sine(duration=10.)
  )

blank_sequence = sequence(id="blank_sequence", [first_blank, after_blank])

blank_strip =
  blank.strip(id="blank_strip", max_blank=5., start_blank=true, blank_sequence)

fallback_sine =
  metadata.map(
    insert_missing=true,
    id="fallback_sine",
    fun (_) -> [("title", "fallback")],
    sine()
  )

s =
  fallback(
    id="main_source",
    [
      blank_strip.{track_sensitive = false},
      fallback_sine.{track_sensitive = false}
    ]
  )

# blank.strip keeps streaming without failing. The first track's mark ends the
# starting blank (`track_sensitive`), so the strip resumes on the next frame
# with that track's metadata.
expected_titles = ["fallback", "first_blank", "after_blank"]
seen_titles = ref([])

def on_metadata(m) =
  seen_titles := [...seen_titles(), m["title"]]
  if
    seen_titles() == expected_titles
  then
    test.pass()
  elsif
    list.length(seen_titles()) == 3
  then
    test.fail()
  end
end

s.on_metadata(synchronous=true, on_metadata)

clock.assign_new(sync='none', [s])

output.dummy(s)
