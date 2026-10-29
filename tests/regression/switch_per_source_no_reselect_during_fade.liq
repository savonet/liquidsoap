%include "../test.liq"

log.level.set(5)

# Test: fade-out transition does not cause premature re-selection.
#
# s1 is initially selected (s1_guard=true). Once s1's first track is out,
# s1_guard is set to false and the switch transitions to s2 via
# default_on_select (which fades out s1).
#
# The custom on_select for s2 re-enables s1_guard on the first frame of the
# ending (s1's proxy), so the guard is restored during the fade. The key
# check: the switch must NOT immediately re-select s1 after the fade. Instead it
# should let the fade complete (pending_on_leave blocks re-selection while
# s1's proxy is still up), then re-select s1 normally.
#
# Expected metadata sequence: s1 → s2 → s1

s1 =
  source.dynamic(
    track_sensitive=true,
    {
      metadata.map(
        insert_missing=true,
        fun (_) -> [("title", "s1")],
        sine(duration=1.)
      )
    }
  )

s2 =
  source.dynamic(
    track_sensitive=true,
    {
      metadata.map(
        insert_missing=true,
        fun (_) -> [("title", "s2")],
        sine(duration=1.)
      )
    }
  )

s1_guard = ref(true)

def s2_on_select({starting, ending, replay_metadata}) =
  s1_guard := true
  source.composition.default_on_select(
    {starting = starting, ending = ending, replay_metadata = replay_metadata}
  )
end

s = switch([(s1_guard, s1), ({true}, s2.{on_select = s2_on_select})])

prev_title = ref("")
titles = ref([])

s.on_metadata(
  synchronous=true,
  fun (m) ->
    begin
      title = m["title"]
      if
        title != "" and title != prev_title()
      then
        prev_title := title
        titles := [...titles(), title]
        if titles() == ["s1"] then s1_guard := false end
        log(
          "titles so far: #{titles()}"
        )
        if
          list.length(titles()) >= 3
        then
          if
            list.prefix(3, titles()) == ["s1", "s2", "s1"]
          then
            test.pass()
          else
            test.fail(
              "unexpected titles: #{titles()}"
            )
          end
        end
      end
    end
)

clock.assign_new(sync="none", [s])
output.dummy(fallible=true, s)
