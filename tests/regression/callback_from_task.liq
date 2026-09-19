# Script code runs on scheduler tasks, and registering a callback there goes
# through an effect whose handler does not cross the thread it is dispatched to.
s = sine()
output.dummy(s)

thread.run(
  delay=0.5,
  fun () ->
    begin
      s.on_metadata(synchronous=false, fun (_) -> ())
      ignore(s.on_track(synchronous=true, fun (_) -> ()))
      test.pass()
    end
)
