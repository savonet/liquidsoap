%include "../test.liq"

# A merged rotation plays one track of each child in turn and presents each
# round as a single track: one track reported per round, at its start; the
# children heard in order, each announced as metadata; the same every round.

def tracks(title, duration) =
  metadata.map(
    insert_missing=true,
    fun (_) -> [("title", title)],
    source.dynamic(track_sensitive=true, {sine(duration=duration)})
  )
end

s = rotate.merge([tracks("a", 0.1), tracks("b", 0.153), tracks("c", 0.2)])

rounds = 4
track_titles = ref([])
titles = ref([])

s.on_track(
  synchronous=true,
  fun (m) -> track_titles := [...track_titles(), m["title"]]
)

def on_metadata(m) =
  titles := [...titles(), m["title"]]
  if
    list.length(titles()) == 3 * rounds
  then
    test.equal(
      titles(),
      ["a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c"]
    )
    test.equal(track_titles(), ["a", "a", "a", "a"])
    test.pass()
  end
end

s.on_metadata(synchronous=true, on_metadata)

clock.assign_new(sync="none", [s])
output.dummy(fallible=true, s)
