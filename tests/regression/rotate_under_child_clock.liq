%include "../test.liq"

# A rotation read through a child clock, by an operator that ticks it less
# or more often than its own clock, keeps its weights and plays every track
# in full: what the rotation counts is what its reader took.

def tracks(title, duration) =
  metadata.map(
    insert_missing=true,
    fun (_) -> [("title", title)],
    # A source that may bring its own pace is refused under a child clock.
    source.dynamic(
      self_sync=false,
      track_sensitive=true,
      {sine(duration=duration)}
    )
  )
end

expected =
  ["jingle", "song", "song", "song", "jingle", "song", "song", "song", "jingle"]

durations = [("jingle", 0.5), ("song", 0.31)]
still_playing = ref(2)

def check(ratio) =
  rotation =
    rotate(
      [tracks("jingle", 0.5).{weight = 1}, tracks("song", 0.31).{weight = 3}]
    )

  titles = ref([])
  starts = ref([])

  def on_track(m) =
    titles := [...titles(), m["title"]]
    starts := [...starts(), source.time(rotation)]
    if
      list.length(titles()) == list.length(expected)
    then
      test.equal(titles(), expected)
      list.iteri(
        fun (index, title) ->
          if
            index + 1 < list.length(expected)
          then
            played = list.nth(starts(), index + 1) - list.nth(starts(), index)
            if
              played < list.assoc(title, durations) - 0.05
            then
              test.fail(
                "ratio #{ratio}: #{title} cut short after #{played}s"
              )
            end
          end,
        titles()
      )

      still_playing := still_playing() - 1
      if still_playing() == 0 then test.pass() end
    end
  end

  rotation.on_track(synchronous=true, on_track)
  s = stretch(ratio=ratio, rotation)
  clock.assign_new(sync="none", [s])
  output.dummy(fallible=true, s)
end

# Read on about every other tick, then about twice per tick.
check(2.)
check(0.5)
