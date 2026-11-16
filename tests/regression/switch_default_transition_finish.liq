%include "../test.liq"

# A source preempted less than max_fade before the end of its track finishes it
# under the starting source, which plays right away. Its next track is never
# played.

def tagged(title, s) =
  metadata.map(insert_missing=true, fun (_) -> [("title", title)], s)
end

music =
  sequence(
    [
      tagged("a", sine(duration=2.)),
      tagged("b", sine(duration=2.)),
      tagged("c", sine())
    ]
  )

live_on = ref(false)
music.on_position(synchronous=true, position=1.5, fun (_, _) -> live_on := true)

live = source.available(tagged("live", sine(880.)), live_on)

s = fallback([live.{track_sensitive = false}, music])

titles = ref([])
s.on_metadata(
  synchronous=true,
  fun (m) ->
    if
      list.last(default="", titles()) != m["title"]
    then
      titles := [...titles(), m["title"]]
    end
)

tracks = ref([])
s.on_track(synchronous=true, fun (m) -> tracks := [...tracks(), m["title"]])

# Past the end of "a" and of the second "b" would have started.
s.on_position(
  synchronous=true,
  position=3.,
  fun (_, _) ->
    if
      tracks() == ["a", "live"] and not list.mem("b", titles())
    then
      test.pass()
    else
      test.fail(
        "unexpected tracks: #{tracks()}, metadata: #{titles()}"
      )
    end
)

clock.assign_new(sync="none", [s])
output.dummy(fallible=true, s)
