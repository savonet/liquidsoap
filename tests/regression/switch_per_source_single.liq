log.level.set(5)

# Test that per-source single=true forbids selecting the same source for two
# consecutive tracks.
#
# State machine:
#   0 -> see "a" (s_single selected first) -> skip to force a track boundary
#   1 -> single=true must prevent s_single from being reselected; s_other plays
#        -> see "b" (PASS), seeing "a" again means single was ignored (FAIL)

state = ref(0)

s_single =
  source.dynamic(
    track_sensitive=true,
    {metadata.map(insert_missing=true, fun (_) -> [("title", "a")], sine())}
  ).{single = true}

s_other =
  source.dynamic(
    track_sensitive=true,
    {metadata.map(insert_missing=true, fun (_) -> [("title", "b")], sine())}
  )

s = switch([({true}, s_single), ({true}, s_other)])

o = output.dummy(fallible=true, s)

s.on_metadata(
  synchronous=true,
  fun (m) ->
    begin
      title = m["title"]
      log(
        "state=#{state()}, title=#{title}"
      )
      if
        state() == 0 and title == "a"
      then
        state := 1
        o.skip()
      elsif
        state() == 1
      then
        if title == "a" then test.fail() end
        if title == "b" then test.pass() end
      end
    end
)

clock.assign_new(sync="none", [s])
