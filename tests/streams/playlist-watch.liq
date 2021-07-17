#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq

# Test file watcher logic in playlist.

%include "test.liq"

log.level.set(5)

def f() =
  reloaded = ref("")

  def on_reload(uri) =
    reloaded := uri
  end

  s = playlist(on_reload=on_reload, reload_mode="watch", "invalid uri")

  s.reload(uri="playlist")

  if !reloaded != "playlist" then
    test.fail()
  end

  reloaded := ""

  ignore(process.run("touch playlist"))

  if !reloaded != "playlist" then
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
