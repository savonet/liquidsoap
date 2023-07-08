settings.log.level.set(4)
s = playlist(reload_mode='watch', "../media/all_media_files")
output.dummy(fallible = true, s)
thread.run(delay=3., fun () -> begin
  print("sending kill -s INT #{process.pid()}")
  process.run("kill -s INT #{process.pid()}")
end)
