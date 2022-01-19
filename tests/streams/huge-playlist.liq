#!../../src/liquidsoap ../../libs/stdlib.liq

# Test huge playlists, see #2162

%include "test.liq"

log.level.set(5)

def f() =
  s = playlist(mode="normal", "files/huge_playlist")
  print("\nWe have #{s.length()} songs!\n")
  output.dummy(mksafe(s))
end

test.check(f)

def on_done () =
  test.pass()
  shutdown()
end

thread.run(delay=5., on_done)
