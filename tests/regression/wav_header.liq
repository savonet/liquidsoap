%include "../test.liq"

# A %wav stream's sizes are the values readers take as "to the end of the
# stream", and more than 16 bits or 2 channels use WAVE_FORMAT_EXTENSIBLE,
# the only tag defined for them.

pending = ref(2)

def bytes(path, start, length) =
  string.sub(encoding="ascii", file.contents(path), start=start, length=length)
end

def check(name, format, ~tag, ~codec) =
  s = once(sine(duration=0.5))
  clock.assign_new(sync="none", [s])

  def on_close(path) =
    probed =
      string.trim(
        process.read(
          "ffprobe -v error -show_entries stream=codec_name -of csv=p=0 #{
            process.quote(path)
          }"
        )
      )

    riff = bytes(path, 4, 4)
    format_tag = bytes(path, 20, 2)
    print(
      "#{name}: #{probed} riff=#{string.quote(riff)} tag=#{
        string.quote(format_tag)
      }"
    )
    if
      probed == codec and riff == "\xff\xff\xff\xff" and format_tag == tag
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
    file.temp(cleanup=true, "wav_header_#{name}", ".wav"),
    s
  )
end

check("s16", %wav, tag="\x01\x00", codec="pcm_s16le")
check("s24", %wav(samplesize = 24), tag="\xfe\xff", codec="pcm_s24le")
