# Regression test for https://github.com/savonet/liquidsoap/issues/5260
# An error raised while shutting down a failed output must not kill the clock
# thread: the clock keeps animating its remaining sources.

failed = ref(false)
frames_after_failure = ref(0)

failing = sine()

def fail_once() =
  if
    not failed()
  then
    failed := true
    error.raise(
      error.failure,
      "streaming failure"
    )
  end
end

failing.on_frame(synchronous=true, fail_once)

o = output.dummy(failing)
o.on_stop(
  synchronous=true,
  fun () ->
    error.raise(
      error.failure,
      "cleanup failure"
    )
)

# Same clock as the failing output: it must keep ticking once the failure has
# been contained.
healthy = sine()

def check() =
  if
    failed()
  then
    frames_after_failure := frames_after_failure() + 1
    if frames_after_failure() >= 20 then test.pass() end
  end
end

healthy.on_frame(synchronous=true, check)
output.dummy(healthy)

clock.assign_new(
  id="failing",
  sync="none",
  on_error=fun (_) -> (),
  [failing, healthy]
)

thread.run(
  delay=20.,
  {
    test.fail(
      "clock stopped ticking after the failed output was shut down"
    )
  }
)
