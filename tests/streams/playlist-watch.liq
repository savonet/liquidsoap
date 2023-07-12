# Test file watcher logic in playlist.

log.level.set(5)

def f() =
  test.skip()

  reloaded = ref("")

  def on_reload(uri) =
    reloaded := uri
  end

  s = playlist(on_reload=on_reload, reload_mode="watch", "invalid uri")

  playlist = file.temp("foo", "bar")

  on_cleanup({file.remove(playlist)})

  s.reload(uri=playlist)

  if !reloaded != playlist then
    test.fail()
  end

  reloaded := ""

  ignore(process.run("echo bla >> #{playlist} && sleep 0.1"))

  if !reloaded != playlist then
    test.fail()
  end

  reloaded := ""

  s.reload(uri="invalid uri again")

  if !reloaded != "invalid uri again" then
    test.fail()
  end

  test.pass()
end

test.check(f)
