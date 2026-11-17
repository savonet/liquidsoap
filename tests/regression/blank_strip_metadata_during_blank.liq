%include "../test.liq"

# Metadata the source emits while blank.strip strips its blank reaches the
# output with the new track the strip resumes with.

# Sound stops at 1s and resumes at 3s, without track marks.
volume = ref(1.)
live = amplify({volume()}, sine())
thread.run(delay=1., {volume := 0.})
thread.run(delay=2., {live.insert_metadata([("title", "live")])})
thread.run(delay=3., {volume := 1.})

live = blank.strip(max_blank=0.5, live)

music =
  metadata.map(insert_missing=true, fun (_) -> [("title", "music")], sine())

s =
  fallback(
    [
      live.{
        track_sensitive = false,
        on_select = source.composition.legacy_on_select
      },
      music
    ]
  )

s.on_track(
  synchronous=true,
  fun (m) -> if m["title"] == "live" then test.pass() end
)

thread.run(
  delay=6.,
  fun () ->
    test.fail(
      "the metadata sent during the blank never came out"
    )
)

output.dummy(fallible=true, s)
