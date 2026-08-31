# A crossfade re-announces the outgoing track and `deduplicate=true` removes
# that repeat. The comparison used to be unbounded, so it also swallowed the
# metadata of a track that repeated the one before it: a single file on a loop
# announced its first track and nothing after, and handlers a script registers
# after `cross` stopped firing.
#
# Check that a repeated track is still announced, and that the crossfade's own
# repeat is still removed.

def titled(t, s) =
  metadata.map(
    insert_missing=true,
    update=false,
    (fun (_) -> [("title", t)]),
    s
  )
end

s =
  sequence(
    [
      titled("a", sine(duration=3., 440.)),
      titled("b", sine(duration=3., 880.)),
      titled("b", sine(duration=3., 660.))
    ]
  )

s = crossfade(duration=1., s)

seen = ref([])

def on_metadata(m) =
  t = m["title"]
  seen := [...seen(), t]
end

s.on_metadata(synchronous=true, on_metadata)
clock.assign_new(sync="none", [s])

o = output.dummy(fallible=true, s)

def on_stop() =
  if
    seen() == ["a", "b", "b"]
  then
    test.pass()
  else
    print(
      "expected 3 announcements a, b, b, got #{seen()}"
    )
    test.fail()
  end
end

o.on_stop(synchronous=true, on_stop)
