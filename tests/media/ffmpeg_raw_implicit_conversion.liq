log.level.set(5)
settings.decoder.decoders.set(["ffmpeg"])

out = {"ffmpeg_raw_implicit_conversion-#{random.int()}.ts"}

files =
  [
    "@ffmpeg[format='mp4',@audio[codec='aac',samplerate='48k']].mp4",
    "@wav[mono].wav",
    "@wav[stereo].wav"
  ]

s = sequence(list.map(fun (fname) -> once(single(fname)), files))

clock.assign_new(sync='none', [s])
def on_close(filename) =
  ojson =
    process.read(
      "ffprobe -v quiet -print_format json -show_streams #{
        process.quote(filename)
      }"
    )

  let json.parse (parsed :
    {
      streams: [
        {
          channel_layout: string,
          sample_rate: string,
          sample_fmt: string,
          codec_name: string,
          duration: string
        }
      ]
    }
  ) = ojson

  audio_stream = list.hd(parsed.streams)

  if
    audio_stream.channel_layout == "stereo"
  and
    audio_stream.codec_name == "aac"
  and
    audio_stream.sample_fmt == "fltp"
  and
    audio_stream.sample_rate == "44100"
  and
    float_of_string(audio_stream.duration) > 29.
  then
    test.pass()
  else
    test.fail()
  end
end

output.file(
  fallible=true,
  on_close=on_close,
  %ffmpeg(format = "mpegts", %audio.raw(codec = "aac", samplerate = 44100)),
  out,
  s
)
