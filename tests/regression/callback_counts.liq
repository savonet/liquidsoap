# Callbacks a script registers are counted on the source itself, so a function
# registering on a source it is handed and never releasing what it registered
# shows up as a count that grows. `cross` runs its transition on every crossing
# and releases what it registered on the sources it owns, so that count stays
# flat however many crossings go by.

def count(s, name) =
  list.assoc(default=0, name, s.callbacks_count())
end

# Releasing takes the registration back off the source.
r = sine(id="released")
for _ = 1 to 20 do r.on_track(synchronous=true, fun (_) -> ()).release()  end
if
  count(r, "on_track") != 0
then
  test.fail(
    "released callbacks still counted: #{count(r, "on_track")}"
  )
end

tracks = chop(every=0.5, sine())

crossings = ref(0)
registered = ref(0)

# Registers on the crossed source, which `cross` owns and collects. Measured
# here rather than downstream: this is the one moment the registration is
# guaranteed to be live, so the count is exactly what the previous crossings
# left behind plus one.
def transition(a, b) =
  tracks.on_metadata(synchronous=true, fun (_) -> ())
  on_metadata = count(tracks, "on_metadata")
  if on_metadata > registered() then registered := on_metadata end
  if
    4 < on_metadata
  then
    test.fail(
      "#{on_metadata} transition callbacks piling up on the crossed source"
    )
  end
  sequence([a.source, b.source])
end

s = cross(duration=0.1, transition, tracks)

clock.assign_new(sync="none", id="test", [s])
output.dummy(fallible=true, s)

def on_track(_) =
  ref.incr(crossings)
  if
    crossings() == 45
  then
    # Seeing none at any point means the transition never ran and the
    # collector is not being exercised.
    if
      registered() < 1
    then
      test.fail(
        "the transition registered nothing on the crossed source"
      )
    else
      test.pass()
    end
  end
end

tracks.on_track(synchronous=true, on_track)
