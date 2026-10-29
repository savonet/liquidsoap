%include "../test.liq"

# A switch keeping a track-sensitive selection reads no predicate: they are
# evaluated when a selection is made, not on every streaming cycle.

calls = ref(0)

def counted_true() =
  ref.incr(calls)
  true
end

s = switch([(counted_true, sine())])

output.dummy(fallible=true, s)

thread.run(
  delay=2.,
  {
    if
      calls() > 0 and calls() < 5
    then
      test.pass()
    else
      print(
        "predicate evaluated #{calls()} times"
      )
      test.fail()
    end
  }
)
