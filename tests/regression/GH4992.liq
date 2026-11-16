%include "../test.liq"

# A watched folder of temporary requests: adding a file reloads the playlist,
# which must not destroy the requests already queued. Destroying a temporary
# request deletes its file before it is played, and every deletion triggers
# another reload.

dir = file.temp_dir(cleanup=true, "gh4992")
source_file = "../streams/file1.mp3"

def add(name) =
  file.write(data=file.contents(source_file), "#{dir}/#{name}.mp3")
end

list.iter(add, ["a", "b", "c"])

s = playlist(dir, mode="normal", reload_mode="watch", prefix="tmp:")
output.dummy(fallible=true, s)

thread.run(delay=1., fun () -> add("d"))

# Each file lasts 5s, so none has finished playing yet.
thread.run(
  delay=3.5,
  fun () ->
    begin
      files = list.sort.natural(file.ls(dir))
      if
        files == ["a.mp3", "b.mp3", "c.mp3", "d.mp3"]
      then
        test.pass()
      else
        test.fail(
          "unexpected files: #{files}"
        )
      end
    end
)
