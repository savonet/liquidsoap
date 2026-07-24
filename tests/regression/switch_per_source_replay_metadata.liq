log.level.set(5)

# Regression test for stale metadata replay when a file source is re-selected
# after ending at a track boundary.
#
# Bug: on_leave for file sources with track_sensitive=true did not call
# clear_last_metadata(). When the source was re-selected, replay_metadata
# would inject the previous track's metadata, which then overwrote the new
# track's own metadata in the same frame.
#
# State machine:
#   0 -> first frame of s_file (title="a1") -> force skip (track boundary)
#   1 -> s_other plays (title="other") -> switch back to s_file
#   2 -> second track of s_file must have title="a2", not the stale "a1"

state = ref(0)
use_other = ref(false)
s_file_title = ref("a1")

# Infinite sine so the source does not end naturally; we force a track
# boundary via o.skip() to stay in track_sensitive=true territory.
s_file =
  metadata.map(
    insert_missing=true,
    fun (_) -> [("title", s_file_title())],
    sine()
  )

s_other =
  metadata.map(insert_missing=true, fun (_) -> [("title", "other")], sine())

s =
  switch([({use_other()}, s_other.{track_sensitive = false}), ({true}, s_file)])

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
        state() == 0 and title == "a1"
      then
        state := 1
        use_other := true
        o.skip()
      elsif
        state() == 1 and title == "other"
      then
        state := 2
        s_file_title := "a2"
        use_other := false
      elsif
        state() == 2
      then
        if title == "a2" then test.pass() else test.fail() end
      end
    end
)

clock.assign_new(sync="none", [s])
