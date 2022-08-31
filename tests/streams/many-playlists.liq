#!../../liquidsoap ../test.liq

# Test large number of playlists, see #2586

log.level.set(4)

def f() =
  n = 1000
  print(newline=false, "Loading #{n} playlists... ")
  l = list.init(100, fun(i) -> playlist(id="playlist#{i}", mode="normal", "playlist"))
  print("done")
  s = random(l)
  output.dummy(mksafe(s))
end

test.check(f)

def on_done () =
  test.pass()
  shutdown()
end

thread.run(delay=5., on_done)
