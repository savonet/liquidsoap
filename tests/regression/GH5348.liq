# A switch inside a passive child clock used to re-evaluate its selection on
# every frame: such a clock is ticked by its parent even on cycles where its
# consumer pulls nothing, which the switch mistook for resuming after having
# gone quiet. Track sensitivity was lost, so `rotate` weights were ignored and
# each jingle was cut after a couple of frames.
#
# `stretch(ratio=2.)` gives us such a clock: it slows the stream down, so it
# only pulls its child on every other cycle. Track durations are stretched
# along with it, hence the doubled expectations below.

def tagged(~every, ~title, s) =
  chop(every=every, metadata={[("title", title)]}, s)
end

jingles = tagged(every=1., title="jingle", sine(660.))
music = tagged(every=2., title="music", sine(440.))

radio = rotate(id="rotate", [jingles, music.{weight = 2}])

# Track starts, as (title, time), oldest first.
marks = ref([])

def check(m) =
  marks := [...marks(), (m["title"], source.time(radio))]

  if
    list.length(marks()) == 8
  then
    failed = ref(false)

    def fail(msg) =
      print(msg)
      failed := true
    end

    # The last mark only gives us the end of the track before it, so we check
    # the six tracks that both start and end within what we collected.
    tracks =
      list.map(
        fun (i) ->
          begin
            let (title, start) = list.nth(marks(), i)
            let (_, stop) = list.nth(marks(), i + 1)
            (title, stop - start)
          end,
        [1, 2, 3, 4, 5, 6]
      )

    print(
      "played: #{tracks}"
    )

    # Six consecutive tracks cover two full rotations, whichever one we start
    # on: one jingle for every two music tracks.
    if
      list.length(list.filter(fun ((title, _)) -> title == "jingle", tracks)) !=
        2
    then
      fail(
        "expected 2 jingles out of 6 tracks"
      )
    end

    list.iter(
      fun ((title, duration)) ->
        begin
          expected = title == "jingle" ? 2. : 4.
          if
            abs(duration - expected) > 0.1
          then
            fail(
              "#{title} track lasted #{duration}s, expected #{expected}s"
            )
          end
        end,
      tracks
    )

    if failed() then test.fail() else test.pass() end
  end
end

radio.on_metadata(synchronous=true, check)

radio = stretch(ratio=2., radio)

clock.assign_new(sync="none", [radio])

output.dummy(radio)
