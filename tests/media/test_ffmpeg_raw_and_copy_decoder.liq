log.level.set(5)
settings.decoder.decoders.set(["ffmpeg"])

fname = argv(default="", 1)

out = {"#{fname}+ffmpeg_raw_and_copy_decoder-#{random.int()}.mp4"}

s = single(fname)
s = once(s)

done = ref(false)

#clock.assign_new(sync='none', [s])
def on_close(encoded_fname) =
  if
    not done()
  then
    done := true

    let json.parse (iparsed :
      {
        streams: [
          {
            index: int,
            channel_layout: string?,
            sample_rate: string?,
            sample_fmt: string?,
            codec_type: string,
            pix_fmt: string?
          }
        ]
      }
    ) = test.ffprobe(fname)

    let json.parse (oparsed :
      {
        streams: [
          {
            index: int,
            channel_layout: string?,
            sample_rate: string?,
            sample_fmt: string?,
            codec_type: string,
            pix_fmt: string?
          }
        ]
      }
    ) = test.ffprobe(encoded_fname)

    filter = fun (type, l) -> list.filter(fun (s) -> s.codec_type == type, l)
    sort =
      fun (l) ->
        list.sort(fun (s1, s2) -> if s1.index < s2.index then -1 else 1 end, l)

    let [{index = _, ...iaudio}] = sort(filter("audio", iparsed.streams))
    let [{index = _, ...ivideo}] = sort(filter("video", iparsed.streams))
    let [{index = _, ...oaudio}] = sort(filter("audio", oparsed.streams))
    let [{index = _, ...ovideo}] = sort(filter("video", oparsed.streams))

    if
      iaudio == oaudio and ivideo == ovideo
    then
      test.pass()
    else
      test.fail()
    end
  end
end

output.file(
  fallible=true,
  on_close=on_close,
  %ffmpeg(format = "mkv", %audio.copy, %video.raw(codec = "libx264")),
  out,
  s
)
