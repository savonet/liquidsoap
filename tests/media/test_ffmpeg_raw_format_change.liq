%include "../test.liq"

log.level.set(4)
settings.decoder.decoders.set(["ffmpeg"])

# One second in one video format followed by one second in another, with
# continuous timestamps.
fname = argv(1)
s = once(single(fname))

def mkfilter(graph) =
  v = ffmpeg.filter.video.input(graph, source.tracks(s).video)
  v = ffmpeg.filter.hflip(graph, v)
  v = ffmpeg.filter.video.output(graph, v)
  source({video = v})
end

s = ffmpeg.filter.create(mkfilter)

def on_close(fname) =
  let json.parse (parsed : {streams: [{duration: string}]}) =
    test.ffprobe(fname)

  let [stream] = parsed.streams
  duration = float_of_string(stream.duration)

  # Both halves come out of the filter graph and into one encoded stream.
  if 1.5 < duration and duration < 2.5 then test.pass() else test.fail() end
end

output.file(
  fallible=true,
  on_close=on_close,
  %ffmpeg(format = "mp4", %video.raw(codec = "mpeg4")),
  "#{fname}-raw-format-change.mp4",
  s
)
