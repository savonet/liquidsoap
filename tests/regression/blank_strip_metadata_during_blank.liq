%include "../test.liq"

# Metadata the source emits while blank.strip strips its blank reaches the
# output with the new track the strip resumes with.

live = sequence([sine(660., duration=1.), blank(duration=2.), sine(880.)])
thread.run(delay=2., {live.insert_metadata([("title", "live")])})

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
