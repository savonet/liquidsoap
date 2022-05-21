log.level.set(5)
settings.decoder.decoders.set(["FFMPEG"])

fname = argv(default="",1)
out = "#{fname}+test_ffmpeg_copy_decoder.mp4"

if file.exists(out) then
  file.remove(out)
end

s = single(fname)

s = once(s)

clock.assign_new(sync='none',[s])

def on_done () =
  ijson = process.read("ffprobe -v quiet -print_format json -show_streams '#{fname}'")
  ojson = process.read("ffprobe -v quiet -print_format json -show_streams '#{out}'")

  let json.parse ( iparsed : {
    streams: [{
      channel_layout: string?,
      sample_rate: string?,
      sample_fmt: string?,
      codec_name: string?,
      pix_fmt: string?
    }]
  }) = ijson

  let json.parse ( oparsed : {
    streams: [{
      channel_layout: string?,
      sample_rate: string?,
      sample_fmt: string?,
      codec_name: string?,
      pix_fmt: string?
    }]
  }) = ojson

  filter = fun(l) -> list.filter(fun (s) -> null.defined(s.codec_name), l)
  sort = fun (l) -> list.sort(fun (s1, s2) -> if s1.codec_name < s2.codec_name then -1 else 1 end, l)
  let [iaudio, ivideo] = sort(filter(iparsed.streams))
  let [oaudio, ovideo] = sort(filter(oparsed.streams))

  if iaudio == oaudio and ivideo == ovideo then
    test.pass()
  else
    test.fail()
  end
end

output.file(fallible=true, on_stop=on_done, %ffmpeg(format="mkv",%audio.copy,%video.copy), out, s)
