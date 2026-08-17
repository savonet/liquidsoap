log.level.set(4)

# Chained handoffs must not drop an `on_leave`. `a` is preempted by `b`, then
# `b` is dropped while `a`'s transition is still running. `a`'s callback used to
# live in a single slot on the new selection, so the second handoff overwrote it
# and `a` never learned it had been left mid-track.

left = ref([])

def track(name) =
  fun (_) -> left := [...left(), name]
end

# None of these ever end a track on their own, so every handoff is a preemption.
a = (sine(id="a") : source).{track_sensitive = false, on_leave = track("a")}
b =
  (sine(id="b", 550.) : source).{track_sensitive = false, on_leave = track("b")}
c =
  (sine(id="c", 660.) : source).{track_sensitive = false, on_leave = track("c")}

use_b = ref(false)
use_c = ref(false)

s = switch([({use_c()}, c), ({use_b()}, b), ({true}, a)])

thread.run(delay=1., {use_b := true})

# Well inside the default 1s fade of the a -> b handoff: `b` stops being
# eligible and `c` takes over while `a` is still being faded out.
thread.run(
  delay=1.3,
  {
    begin
      use_c := true
      use_b := false
    end
  }
)

thread.run(
  delay=4.,
  {
    begin
      log("left=#{left()}")
      if
        list.mem("a", left()) and list.mem("b", left())
      then
        test.pass()
      else
        test.fail()
      end
    end
  }
)

clock.assign_new(sync="none", [s])
output.dummy(fallible=true, s)
