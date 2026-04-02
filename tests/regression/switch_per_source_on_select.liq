log.level.set(5)

# Test that the per-source on_select callback is called when switching to a
# source, and that the returned source is used.
#
# The on_select for s_b wraps the incoming source in a metadata.map that adds
# a "via_on_select" marker. If on_select ran, we see that marker.

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
    on_select = fun (new) ->
      (metadata.map(
         fun (m) -> [("via_on_select", "true"), ...m],
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
      via = m["via_on_select"]
      log("state=#{state()}, title=#{title}, via_on_select=#{via}")
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
