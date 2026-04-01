log.level.set(5)

# Test that per-source replay_metadata=false prevents metadata replay on switch.
#
# State machine:
#   0 -> see "replay" from s_replay (natural) -> switch to s_no_replay
#   1 -> see "no_replay_1" from s_no_replay (natural) -> prepare v2, switch back to s_replay
#   2 -> see "replay" from s_replay (REPLAYED, proves replay_metadata=true works) -> switch to s_no_replay
#   3 -> seeing "no_replay_1" here would mean replay happened (FAIL)
#        seeing "no_replay_2" means new source.dynamic content plays without replay (PASS)

state = ref(0)
no_replay_version = ref(1)
use_no_replay = ref(false)

s_replay =
  metadata.map(
    insert_missing=true,
    fun (_) -> [("title", "replay")],
    sine()
  )

s_no_replay_v1 =
  (metadata.map(
     insert_missing=true,
     fun (_) -> [("title", "no_replay_1")],
     sine()
   ) : source)

s_no_replay_v2 =
  (metadata.map(
     insert_missing=true,
     fun (_) -> [("title", "no_replay_2")],
     sine()
   ) : source)

s_no_replay_dyn =
  source.dynamic(
    track_sensitive=false,
    {if no_replay_version() == 1 then s_no_replay_v1 else s_no_replay_v2 end}
  )

s_no_replay = s_no_replay_dyn.{replay_metadata = false}

s =
  switch(
    track_sensitive=false,
    [({use_no_replay()}, s_no_replay), ({true}, s_replay)]
  )

s.on_metadata(
  synchronous=true,
  fun (m) ->
    begin
      title = m["title"]
      log("state=#{state()}, title=#{title}")
      if state() == 0 and title == "replay" then
        state := 1
        use_no_replay := true
      elsif state() == 1 and title == "no_replay_1" then
        no_replay_version := 2
        state := 2
        use_no_replay := false
      elsif state() == 2 and title == "replay" then
        state := 3
        use_no_replay := true
      elsif state() == 3 then
        if title == "no_replay_1" then test.fail() end
        if title == "no_replay_2" then test.pass() end
      end
    end
)

clock.assign_new(sync="none", [s])
output.dummy(fallible=true, s)
