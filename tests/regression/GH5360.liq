# Regression test for metadata.deduplicate comparing across track marks.
#
# The operator removes a metadata that repeats the one before it, so that a
# crossfade re-announcing the outgoing track does not show up twice. Comparing
# without bound also swallowed the metadata of every track that repeated the
# previous one, e.g. a single file on a loop, which silenced the handlers a
# script attaches after `cross`.

log.level := 3

s = sine()
d = metadata.deduplicate(s)

announced = ref(0)
d.on_metadata(synchronous=true, fun (_) -> announced := announced() + 1)

output.dummy(fallible=true, d)

m = [("title", "same")]

thread.run(delay=1.0, {s.insert_metadata(new_track=true, m)})

# Repeated within the track: dropped.
thread.run(delay=1.5, {s.insert_metadata(new_track=false, m)})

thread.run(delay=2.0, {s.insert_metadata(new_track=true, m)})

thread.run(
  delay=3.0,
  {
    if
      announced() == 2
    then
      test.pass()
    else
      print(
        "expected 2 announcements, got #{announced()}"
      )
      test.fail()
    end
  }
)
