# Test huge playlists, see #2162

log.level.set(5)

def f() =
  s = playlist(mode="normal", "huge_playlist")
  print("\nWe have #{s.length()} songs!\n")
  output.dummy(mksafe(s))
end

test.check(f)

def on_done () =
  test.pass()
  shutdown()
end

thread.run(delay=5., on_done)
