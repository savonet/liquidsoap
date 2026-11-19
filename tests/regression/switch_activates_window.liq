%include "../test.liq"

# The cookbook's jingle at the start of every hour:
#
#   radio = switch([(predicate.activates({0m-5m}), jingles), ({true}, playlist)])
#
# with the window simulated in stream time. Songs last 3 s, so each window,
# [5, 7) and [20, 22), opens in the middle of a song and contains a song
# boundary. Each window gives exactly one jingle, at that boundary, and no song
# is cut.

def tagged(title, duration) =
  metadata.map(
    insert_missing=true,
    fun (_) -> [("title", title)],
    sine(duration=duration)
  )
end

playlist = sequence(list.init(20, fun (_) -> tagged("song", 3.)))
jingles = sequence(list.init(20, fun (_) -> tagged("jingle", 0.5)))

window = ref(false)

radio = switch([(predicate.activates({window()}), jingles), ({true}, playlist)])

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
      window := (5. <= t and t < 7.) or (20. <= t and t < 22.)
      if
        25. <= t
      then
        jingles = list.filter(fun (p) -> fst(p) == "jingle", played())
        if
          list.length(jingles) != 2
        then
          test.fail(
            "jingles played: #{jingles}, expected one per window"
          )
        end

        let [(_, first), (_, second)] = jingles
        if
          first < 5. or 7. <= first or second < 20. or 22. <= second
        then
          test.fail(
            "jingles at #{first}s and #{second}s, outside their windows"
          )
        end

        test.pass()
      end
    end
)

clock.assign_new(sync="none", [radio])
output.dummy(fallible=true, radio)
