%include "../test.liq"

# `switch.when` makes a source due each time a predicate becomes true, until
# that source starts a track. Here the predicate is true for short windows in
# the middle of songs, between two track boundaries: each window gives exactly
# one jingle, at the next boundary, and no song is cut.

def tagged(title, duration) =
  metadata.map(
    insert_missing=true,
    fun (_) -> [("title", title)],
    sine(duration=duration)
  )
end

music = sequence(list.init(30, fun (_) -> tagged("song", 1.)))
jingles = sequence(list.init(30, fun (_) -> tagged("jingle", 0.5)))

active = ref(false)
radio =
  switch(
    [(switch.when(every=0.05, {active()}, jingles), jingles), ({true}, music)]
  )

played = ref([])
radio.on_track(
  synchronous=true,
  fun (m) -> played := [...played(), (m["title"], source.time(radio))]
)

radio.on_frame(
  synchronous=true,
  fun () ->
    begin
      t = source.time(radio)
      active := (2.3 <= t and t < 2.6) or (5.4 <= t and t < 5.7)
      if
        8. <= t
      then
        jingles_played = list.filter(fun (p) -> fst(p) == "jingle", played())

        if
          list.length(jingles_played) != 2
        then
          test.fail(
            "jingles played: #{jingles_played}, expected 2"
          )
        end

        # Each jingle starts after its window, where a song ended.
        let [(_, first), (_, second)] = jingles_played
        if
          first < 2.3 or 4. < first or second < 5.4 or 7. < second
        then
          test.fail(
            "jingles at #{first}s and #{second}s"
          )
        end

        test.pass()
      end
    end
)

output.dummy(fallible=true, radio)
