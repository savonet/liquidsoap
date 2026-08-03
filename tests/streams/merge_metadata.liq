log.level.set(4)

def f(m) =
  if
    m ==
      [
        (
          "s",
          "That's me!"
        ),
        (
          "s'",
          "That's me!"
        ),
        ("test", "blo")
      ]
  then
    test.pass()
  else
    test.fail()
  end
end

s = sine()

s' = sine()
s' = insert_metadata(s')

def insert() =
  print(
    "Inserting metadata"
  )
  s.insert_metadata(
    [
      ("test", "bla"),
      (
        "s",
        "That's me!"
      )
    ]
  )
  s'.insert_metadata(
    [
      ("test", "blo"),
      (
        "s'",
        "That's me!"
      )
    ]
  )
end

s =
  source(
    {
      audio = source.tracks(s).audio,
      metadata =
        track.metadata.merge(
          [source.tracks(s).metadata, source.tracks(s').metadata]
        )
    }
  )

s.on_metadata(synchronous=true, f)

# Both insertions have to land in the same frame for the merge to be observable
# as one metadata event. Doing them from a synchronous frame callback keeps the
# streaming loop from producing a frame in between, which a thread cannot.
inserted = ref(false)
s.on_frame(
  synchronous=true,
  fun () ->
    if
      not inserted()
    then
      inserted := true
      insert()
    end
)
output.dummy(s)
