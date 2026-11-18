%include "../test.liq"

# A callback that raises is logged and the source goes on: other handlers of
# the same event still run, the stream keeps its tracks, and an output whose
# callback raises keeps writing. A handler that raises on every frame does
# not stop anything either.

s = sequence(list.init(20, fun (_) -> sine(duration=0.5)))

s.on_track(
  synchronous=true,
  fun (_) ->
    error.raise(
      error.failure,
      "on_track failed"
    )
)

tracks = ref(0)
s.on_track(synchronous=true, fun (_) -> ref.incr(tracks))

s.on_frame(
  synchronous=true,
  fun () ->
    error.raise(
      error.failure,
      "on_frame failed"
    )
)

dir = file.temp_dir("hls")
on_cleanup({file.rmdir(dir)})

o =
  output.file.hls(
    fallible=true,
    segment_duration=1.,
    dir,
    [("aac", %ffmpeg(format = "mpegts", %audio(codec = "aac")))],
    s
  )

file_changes = ref(0)
o.on_file_change(
  synchronous=true,
  fun (_) ->
    error.raise(
      error.failure,
      "on_file_change failed"
    )
)
o.on_file_change(synchronous=true, fun (_) -> ref.incr(file_changes))

clock.assign_new(sync="none", [o])
o.on_stop(
  synchronous=true,
  fun () ->
    begin
      if
        tracks() != 20
      then
        test.fail(
          "#{tracks()} tracks seen by the second handler"
        )
      end

      # 10 s of audio in 1 s segments: at least a few segment files written.
      if
        file_changes() < 10
      then
        test.fail(
          "#{file_changes()} file changes seen by the second handler"
        )
      end

      test.pass()
    end
)
