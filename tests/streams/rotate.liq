def mksource(id="", url) =
  metadata.map(
    insert_missing=true,
    id=id,
    fun (_) -> [("source", id)],
    mksafe(playlist(url))
  )
end

jingles = mksource("jingles", "jingles")

music = mksource("music", "playlist")

music2 = mksource("music2", "playlist")

rounds = 10
round_len = 8

played = ref([])
njingles = ref(0)
nmusic = ref(0)
nmusic2 = ref(0)

def ot(m) =
  label = m["source"]

  if
    label != ""
  then
    played := [label, ...played()]

    if label == "music" then ref.incr(nmusic) end
    if label == "music2" then ref.incr(nmusic2) end
    if label == "jingles" then ref.incr(njingles) end

    print(
      "\n**** track: #{label}\n"
    )

    if
      list.length(played()) == round_len * rounds
    then
      print(
        "PLAYED: #{list.rev(played())}\n"
      )

      if
        njingles() == rounds
      and
        nmusic() == 3 * rounds
      and
        nmusic2() == 4 * rounds
      then
        test.pass()
      else
        print(
          "jingles: #{njingles()} / music : #{nmusic()} / music2: #{nmusic2()}"
        )
        test.fail()
      end
    end
  end
end

radio = rotate(id="rotate", weights=[3, 1], [music, jingles])
radio = rotate(id="rotate2", weights=[1, 1], [radio, music2])

radio.on_track(synchronous=true, ot)

clock.assign_new(sync="none", [radio])

output.dummy(fallible=true, radio)
