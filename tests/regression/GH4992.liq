%include "../test.liq"

# A watched folder of temporary requests: adding a file reloads the playlist,
# which must not destroy the requests already queued, nor pick them again.
# Destroying a temporary request deletes its file, and every deletion triggers
# another reload.

dir = file.temp_dir(cleanup=true, "gh4992")
source_file = "../streams/file1.mp3"

def add(name) =
  file.write(data=file.contents(source_file), "#{dir}/#{name}.mp3")
end

list.iter(add, ["a", "b", "c"])

reloads = ref(0)
played = ref([])

# Files picked again after a reload, while their request is still queued or
# playing.
picked_again = ref([])
check_reload = ref(fun () -> ())

s =
  playlist(
    dir,
    mode="normal",
    reload_mode="watch",
    on_reload=fun (_) ->
      begin
        check = check_reload()
        check()
      end,
    prefix="annotate:liq_cue_out=1.:tmp:"
  )

check_reload :=
  fun () ->
    begin
      ref.incr(reloads)
      current = s.current()
      kept = list.map(request.uri, s.queue())
      kept =
        if
          null.defined(current)
        then
          [request.uri(null.get(current)), ...kept]
        else
          kept
        end
      again = list.filter(fun (f) -> list.mem(f, kept), s.remaining_files())
      picked_again := [...picked_again(), ...list.map(path.basename, again)]
    end

s.on_track(
  synchronous=true,
  fun (m) -> played := [...played(), path.basename(m["filename"])]
)

output.dummy(fallible=true, s)

# Once the initial load is done, while "a" plays and "b" is queued. Files
# are deleted once their request is done, so the last one played stays.
thread.run(delay=0.3, fun () -> add("d"))

thread.run(
  delay=5.,
  fun () ->
    begin
      left = file.ls(sorted=true, dir)
      if
        reloads() < 2
      then
        test.fail(
          "the watcher never reloaded the playlist"
        )
      elsif
        picked_again() != []
      then
        test.fail(
          "picked again after a reload: #{picked_again()}"
        )
      elsif
        played() != ["a.mp3", "b.mp3", "c.mp3", "d.mp3"]
        or list.exists(
          fun (f) -> list.mem(f, left),
          ["a.mp3", "b.mp3", "c.mp3"]
        )
      then
        test.fail(
          "played: #{played()}, left: #{left}"
        )
      else
        test.pass()
      end
    end
)
