%include "../test.liq"

# files.md §7, known-complexity.md KC11: a final stop without persistence
# leaves no file the output wrote and deletes none of the decoys.
dir = file.temp_dir("hls_cleanup")
on_cleanup({file.rmdir(dir)})
out = "#{dir}/out"
decoys = ["#{out}/a_0.mp3", "#{out}/notes.txt", "#{out}/sub/a_1.mp3"]
file.mkdir(parents=true, "#{out}/sub")
list.iter(fun (f) -> file.write(data="decoy", f), decoys)

s = sine()
clock.assign_new(sync="none", [s])

def fail_on_error(err) =
  test.fail(
    "error while streaming: #{err}"
  )
  null
end

written = ref([])
shutdown_requested = ref(false)

o =
  output.file.hls(
    reopen_on_error=fail_on_error,
    segment_duration=1.,
    segments=3,
    segments_overhead=1,
    out,
    [("a", %mp3), ("b", %shine(bitrate = 64))],
    s
  )

def files_left() =
  list.filter(
    fun (f) -> not file.is_directory(f),
    file.ls(absolute=true, recursive=true, out)
  )
end

def check_cleanup() =
  deadline = time() + 20.

  def rec wait() =
    left = files_left()
    if
      list.sort(fun (x, y) -> x < y ? -1 : (x > y ? 1 : 0), left) ==
        list.sort(fun (x, y) -> x < y ? -1 : (x > y ? 1 : 0), decoys)
    then
      list.iter(
        fun (f) ->
          if
            file.contents(f) != "decoy"
          then
            test.fail(
              "decoy #{f} was overwritten"
            )
          end,
        decoys
      )
      print(
        "#{list.length(written())} files written, none left, decoys kept"
      )
      test.pass()
    elsif
      time() > deadline
    then
      test.fail(
        "after the final stop, files left: #{left}, decoys: #{decoys}"
      )
    else
      thread.pause(0.1)
      wait()
    end
  end

  wait()
end

o.on_file_change(
  synchronous=true,
  fun (event) ->
    begin
      if
        list.mem(event.path, decoys)
      then
        test.fail(
          "the output wrote or deleted the decoy #{event.path}"
        )
      end
      if event.state == "created" then written := event.path::written() end
      if
        not shutdown_requested() and list.length(written()) >= 20
      then
        shutdown_requested := true
        thread.run(
          fun () ->
            begin
              o.shutdown()
              check_cleanup()
            end
        )
      end
    end
)
