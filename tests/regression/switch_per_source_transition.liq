log.level.set(5)

# Test that the per-source transition callback is called when switching to a
# source, and that the returned source is used.
#
# The transition for s_b wraps the incoming source in a metadata.map that adds
# a "via_transition" marker. If the transition ran, we see that marker.

state = ref(0)
use_b = ref(false)

s_a =
  metadata.map(
    insert_missing=true,
    fun (_) -> [("title", "a")],
    sine()
  )

s_b_raw =
  metadata.map(
    insert_missing=true,
    fun (_) -> [("title", "b")],
    sine()
  )

s_b =
  (s_b_raw : source).{
    transition = fun (new) ->
      (metadata.map(
         fun (m) -> [("via_transition", "true"), ...m],
         (new : source)
       ) : source)
  }

s =
  switch(
    track_sensitive=false,
    [({use_b()}, s_b), ({true}, s_a)]
  )

s.on_metadata(
  synchronous=true,
  fun (m) ->
    begin
      title = m["title"]
      via = m["via_transition"]
      log("state=#{state()}, title=#{title}, via_transition=#{via}")
      if state() == 0 and title == "a" then
        state := 1
        use_b := true
      elsif state() == 1 and title == "b" then
        if via == "true" then test.pass() else test.fail() end
      end
    end
)

clock.assign_new(sync="none", [s])
output.dummy(fallible=true, s)
