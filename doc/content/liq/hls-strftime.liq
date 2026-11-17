live = input.ffmpeg("srt://0.0.0.0:9000?mode=listener")

# BEGIN
# Wall-clock time the current run started.
run_start = ref(time())

# Media produced since then.
media_time = ref(0.)

def segment_name(s) =
  start = run_start() + media_time()
  media_time := media_time() + s.duration
  "#{time.string(time=start, '%Y/%m/%d/%Hh')}/#{int(start)}.#{s.extname}"
end

o =
  output.file.hls(
    "/out",
    fallible=true,
    segment_duration=30.,
    segment_name=segment_name,
    main_playlist_writer=fun (~extra_tags:_, ~prefix:_, ~version:_, _) -> null,
    [("index", %ffmpeg(format = "mpegts", %audio.copy, %video.copy))],
    live
  )

o.on_start(
  synchronous=true,
  fun () ->
    begin
      run_start := time()
      media_time := 0.
    end
)
# END
