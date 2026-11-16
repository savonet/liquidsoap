%include "../test.liq"

# When blank.strip's source resumes after a blank with a new track, the track
# mark and metadata it resumes with reach the output, even though the frame
# that carries them is read by the blank check before anything selects it.

live =
  sequence(
    [
      blank(duration=2.),
      metadata.map(
        insert_missing=true,
        fun (_) -> [("title", "live")],
        sine(880.)
      )
    ]
  )

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

tracks = ref([])
s.on_track(synchronous=true, fun (m) -> tracks := [...tracks(), m["title"]])

s.on_position(
  synchronous=true,
  position=1.,
  allow_partial=false,
  fun (_, m) ->
    if
      m["title"] == "live"
    then
      if
        list.last(default="", tracks()) == "live"
      then
        test.pass()
      else
        test.fail(
          "unexpected tracks: #{tracks()}"
        )
      end
    end
)

thread.run(
  delay=5.,
  fun () ->
    test.fail(
      "the live track never came with its metadata: #{tracks()}"
    )
)

clock.assign_new(sync="none", [s])
output.dummy(fallible=true, s)
