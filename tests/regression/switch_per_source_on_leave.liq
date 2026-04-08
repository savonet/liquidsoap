log.level.set(5)

# Test that the per-source on_leave callback is called with the source being
# left when the switch transitions to a different source.
#
# State machine:
#   0 -> see "a" from s_a -> switch to s_b
#        on_leave for s_a fires with s_a as argument (PASS marker set)
#   1 -> see "b" from s_b (first track, transition may still be in progress)
#   2 -> see "b" from s_b (second track) -> check that on_leave fired (PASS) or not (FAIL)

state = ref(0)
on_leave_fired = ref(false)
use_b = ref(false)

s_a =
  (metadata.map(insert_missing=true, fun (_) -> [("title", "a")], sine()) :
    source
  ).{
    track_sensitive = false,
    on_leave =
      fun (x) ->
        begin
          log(
            "on_leave called for #{source.id(x.source)}"
          )
          on_leave_fired := true
        end
  }

s_b =
  metadata.map(
    insert_missing=true,
    fun (_) -> [("title", "b")],
    source.dynamic(infallible=true, track_sensitive=true, {sine(duration=1.)})
  )

s = switch([({use_b()}, s_b), ({true}, s_a)])

s.on_metadata(
  synchronous=true,
  fun (m) ->
    begin
      title = m["title"]
      log(
        "state=#{state()}, title=#{title}, on_leave_fired=#{on_leave_fired()}"
      )
      if
        state() == 0 and title == "a"
      then
        state := 1
        use_b := true
      elsif
        state() == 1 and title == "b"
      then
        state := 2
      elsif
        state() == 2 and title == "b"
      then
        if on_leave_fired() then test.pass() else test.fail() end
      end
    end
)

clock.assign_new(sync="none", [s])
output.dummy(fallible=true, s)
