# Callbacks registered through `on_frame` (and so through `on_track`,
# `on_metadata` and friends) can be deregistered via the `remove` method of
# their return value. Without it, an operator registering on a source it was
# given -- as the fade operators do -- leaks a closure on every call.
s = chop(every=0.5, sine())
s = source.methods(s)

kept = ref(0)
removed = ref(0)

# Registered and immediately removed: must never fire.
h = s.on_track(synchronous=true, fun (_) -> ref.incr(removed))
h.remove()

# Removing twice is a no-op, not an error.
h.remove()

# Left registered: must keep firing.
s.on_track(synchronous=true, fun (_) -> ref.incr(kept))

s.on_track(
  synchronous=true,
  fun (_) ->
    if
      kept() >= 3
    then
      if removed() == 0 then test.pass() else test.fail() end
    end
)

clock.assign_new(sync="none", [s])
output.dummy(s)
