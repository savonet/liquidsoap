%include "../test.liq"

# merge=true must also merge the boundary into the last source of the sequence.

first =
  once(
    metadata.map(
      insert_missing=true,
      fun (_) -> [("title", "first")],
      sine(duration=1.)
    )
  )

last =
  metadata.map(
    insert_missing=true,
    fun (_) -> [("title", "last")],
    sine(duration=2.)
  )

s = sequence(merge=true, [first, last])

tracks = ref([])
s.on_track(synchronous=true, fun (m) -> tracks := [...tracks(), m["title"]])

s.on_position(
  synchronous=true,
  position=1.5,
  fun (_, _) ->
    if
      tracks() == ["first"]
    then
      test.pass()
    else
      test.fail(
        "unexpected tracks: #{tracks()}"
      )
    end
)

clock.assign_new(sync="none", [s])
output.dummy(fallible=true, s)
