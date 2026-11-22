%include "../test.liq"

# Native %mp3 and %shine files wrote no Info frame, so decoders kept the
# encoder delay: a burst 0.5s into the input decoded about 23ms late, and both
# encoders dropped the end of the input (#2841). silencedetect places the
# onset within a few ms: the delay is 23ms.

settings.frame.audio.samplerate := 48000
pending = ref(2)

def ffmpeg_value(cmd, pattern) =
  out =
    process.read(
      "#{cmd} 2>&1"
    )
  m = regexp(pattern).exec(out)
  float_of_string(default=-1., m[1])
end

def check(name, format) =
  s =
    sequence(
      [
        once(blank(duration=0.5)),
        once(sine(duration=0.05, 1000.)),
        once(blank(duration=1.45))
      ]
    )
  clock.assign_new(sync="none", [s])

  def on_close(f) =
    f = process.quote(f)
    onset =
      ffmpeg_value(
        "ffmpeg -nostats -i #{f} -af silencedetect=n=-30dB:d=0.1 -f null -",
        "silence_end: ([0-9.]+)"
      )

    # Decoded samples, at 48kHz mono 16 bits.
    samples =
      ffmpeg_value(
        "ffmpeg -v error -i #{f} -f s16le -ac 1 -ar 48000 - | wc -c",
        "([0-9]+)"
      ) /
        2.

    print(
      "#{name}: onset #{onset}, samples #{samples}"
    )

    if
      abs(onset - 0.5) < 0.01 and samples >= 96000. and samples < 96000. + 1152.
    then
      pending := pending() - 1
      if pending() == 0 then test.pass() end
    else
      test.fail()
    end
  end

  output.file(
    fallible=true,
    on_close=on_close,
    format,
    file.temp(cleanup=true, "mp3_gapless_#{name}", ".mp3"),
    s
  )
end

check("mp3", %mp3(samplerate = 48000, bitrate = 320))
check("shine", %shine(samplerate = 48000, bitrate = 320))
