settings.log.level.set(6)
s = playlist(reload_mode='watch', "../media/all_media_files")
output.dummy(fallible = true, s)
thread.run(delay=3., fun () -> process.run("kill -s INT #{process.pid()}"))
