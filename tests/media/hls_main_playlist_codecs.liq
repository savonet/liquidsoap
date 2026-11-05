%include "../test.liq"
%include "hls_lib.liq"

# KC9: the same H.264 and AAC variant in MPEG-TS and in fMP4, beside an Opus
# variant, whose codec string FFmpeg does not know.
fixture = argv(1)
log_path = file.temp("hls-codecs", ".log")
log.file.path := log_path
log.file := true

dir = file.temp_dir(cleanup=true, "hls-codecs")

s = single(fixture)
clock.assign_new(sync="none", [s])

o =
  output.file.hls(
    segment_duration=1.,
    dir,
    [
      (
        "ts",
        %ffmpeg(
          format = "mpegts",
          %audio(codec = "aac", samplerate = 44100, channels = 2),
          %video(codec = "libx264", tune = "zerolatency")
        )
      ),
      (
        "mp4",
        %ffmpeg(
          format = "mp4",
          %audio(codec = "aac", samplerate = 44100, channels = 2),
          %video(codec = "libx264", tune = "zerolatency")
        )
      ),
      (
        "opus",
        %ffmpeg(
          format = "mpegts",
          %audio(codec = "libopus", samplerate = 48000, channels = 2),
          %video.drop
        )
      )
    ],
    s
  )

main = ref("")

def codecs(name) =
  lines = r/\r\n/.split(main())
  let (_, found) =
    list.fold(
      fun (acc, l) ->
        begin
          let (previous, found) = acc
          if l == "#{name}.m3u8" then (l, previous) else (l, found) end
        end,
      ("", ""),
      lines
    )
  m = r/CODECS="([^"]*)"/.exec(found)
  list.assoc(default="", 1, m)
end

def check() =
  ts = codecs("ts")
  mp4 = codecs("mp4")
  hls_test.check(
    r/^(avc1\.[0-9A-Fa-f]{6},mp4a\.40\.2|mp4a\.40\.2,avc1\.[0-9A-Fa-f]{6})$/.test(
      ts
    ),
    "ts: CODECS \"#{ts}\" is not a full H.264 and AAC-LC string: #{main()}"
  )
  hls_test.check(
    ts == mp4,
    "CODECS differ: ts \"#{ts}\", mp4 \"#{mp4}\""
  )
  hls_test.check(
    not r/CODECS/.test(
      list.nth(r/\r\n/.split(main()), list.length(r/\r\n/.split(main())) - 2)
    ),
    "opus: a CODECS attribute is announced: #{main()}"
  )
  warnings =
    hls_test.log_lines(
      log_path,
      "opus: no codec string"
    )
  hls_test.check(
    warnings != [],
    "opus: no warning about its codec string"
  )
  hls_test.done(4)
end

o.on_file_change(
  synchronous=true,
  fun (ev) ->
    if
      main() == "" and path.basename(ev.path) == "stream.m3u8"
    then
      main := file.contents(ev.path)
      thread.run(check)
    end
)
