# Regression test for a synchronous callback reading the source's own frame.
#
# Synchronous callbacks run in the middle of the source's frame generation, so
# `s.time()`, `s.generate_frame()` and the like used to restart that generation
# instead of reporting the frame in progress, generating a source's frame
# several times per clock tick and, with a source that re-announces metadata on
# each track, looping forever.

log.level := 3

s = sine()
last_time = ref(-1.)

s.on_frame(
  before=true,
  synchronous=true,
  fun () ->
    begin
      t = source.time(s)
      if t == last_time() then test.fail() end
      last_time := t
    end
)

s.on_metadata(synchronous=true, fun (_) -> ignore(s.time()))

output.dummy(fallible=true, s)

thread.run(delay=0.5, {s.insert_metadata([("title", "x")])})
thread.run(delay=1.5, test.pass)
