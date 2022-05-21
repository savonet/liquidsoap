jingles = mksafe(playlist(id="jingles", mode="normal", "jingles"))
music   = mksafe(playlist(id="music", mode="normal", "playlist"))
music2  = mksafe(playlist(id="music2", mode="normal", "playlist"))

rounds = 10
round_len = 8
played = ref([])
njingles = ref(0)
nmusic = ref(0)
nmusic2 = ref(0)

def ot(label)
  played := [label, ...!played]

  if label == "music" then ref.incr(nmusic) end
  if label == "music2" then ref.incr(nmusic2) end
  if label == "jingles" then ref.incr(njingles) end

  print("\n**** track: #{label}\n")

  if list.length(!played) == round_len * rounds then
    print("PLAYED: #{list.rev(!played)}\n")
    if !njingles == rounds and !nmusic == 3 * rounds and !nmusic2 == 4 * rounds then
      test.pass()
    else
      print("jingles: #{!njingles} / music : #{!nmusic} / music2: #{!nmusic2}")
      test.fail()
    end
    shutdown()
  end
end

jingles.on_track(fun (_) -> ot("jingles"))
music.on_track(fun (_) -> ot("music"))
music2.on_track(fun (_) -> ot("music2"))

radio = rotate(id="rotate", weights=[3,1], [music,jingles])
radio = rotate(id="rotate2", weights=[1,1], [radio, music2])
clock.assign_new(sync="none",[radio])
output.dummy(fallible=true, radio)
