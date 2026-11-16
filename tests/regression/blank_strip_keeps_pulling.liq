%include "../test.liq"

# blank.strip is active: it keeps pulling its source at every tick, including
# when nothing downstream asks whether it is ready.

pulled = ref(0)
child = sine()
child.on_frame(synchronous=true, fun () -> ref.incr(pulled))

strip = blank.strip(child)

ticks = ref(0)
s = switch([({false}, strip), ({true}, sine(440.))])
s.on_frame(synchronous=true, fun () -> ref.incr(ticks))

s.on_position(
  synchronous=true,
  position=2.,
  allow_partial=false,
  fun (_, _) ->
    if
      pulled() >= ticks() - 1
    then
      test.pass()
    else
      test.fail(
        "source pulled #{pulled()} times in #{ticks()} ticks"
      )
    end
)

clock.assign_new(sync="none", [s])
output.dummy(s)
