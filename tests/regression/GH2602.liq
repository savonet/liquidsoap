def f() =
  s = input.external.rawaudio(
        channels=1, samplerate=44100, restart=true,
        "ffmpeg -re -f lavfi -i sine=frequency=1000:duration=60 -f s16le -acodec pcm_s16le -ar 44100 -"
      )

  s = ffmpeg.encode.audio(
        %ffmpeg(%audio(
          codec="libmp3lame", channels=1)
        ), s
      )

  s = source.on_track(s, fun (_) -> test.pass())

  output.dummy(fallible=true, s)
end

test.check(f)
