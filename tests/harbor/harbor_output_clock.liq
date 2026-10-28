%include "../test.liq"

# An output on a clock that is not paced in real time fails to start, with an
# error naming the clock, unless allow_unsynced_clock is set.
errors = ref([])
allowed_started = ref(false)

refused = output.harbor(port=18106, mount="refused", %mp3, sine())
clock.assign_new(
  id="unsynced_refused",
  sync="none",
  on_error=fun (e) ->
    atomic(fun () -> errors := [...errors(), error.methods(e).message]),
  [refused]
)

allowed =
  output.harbor(
    port=18106,
    mount="allowed",
    allow_unsynced_clock=true,
    %mp3,
    sine()
  )
allowed.on_start(synchronous=true, fun () -> allowed_started := true)
clock.assign_new(id="unsynced_allowed", sync="none", [allowed])

def check() =
  test.equal(allowed_started(), true)
  test.equal(refused.is_started(), false)
  test.equal(list.length(errors()) > 0, true)
  test.equal(
    string.contains(
      encoding="ascii",
      substring="unsynced_refused",
      list.hd(errors())
    ),
    true
  )
  test.pass()
end

thread.run(delay=2., check)
