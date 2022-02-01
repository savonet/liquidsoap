#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq

# Basic radio test

%include "test.liq"

jingles = mksafe(playlist(mode="normal", "files/jingles"))
music   = mksafe(playlist(mode="normal", "files/playlist"))

rounds = 3
played = ref([])
nmusic = ref(0)
njingles = ref(0)
nmusic = ref(0)

def ot(music, m)
  fname = m["filename"]
  played := fname :: !played
  if music then ref.incr(nmusic) else ref.incr(njingles) end
  print("\n**** track: #{fname}\n")
  if list.length(!played) >= 3 * rounds then
    print("PLAYED: #{list.rev(!played)}\n")
    if !njingles == rounds and !nmusic == 2 * rounds then
      test.pass()
    else
      print("jingles: #{!njingles} / music : #{!nmusic}")
      test.fail()
    end
    shutdown()
  end
end

jingles.on_track(fun(m)->ot(false,m))
music.on_track(fun(m)->ot(true,m))

radio = rotate(weights=[2,1], [music,jingles])
output.dummy(fallible=true, radio)
