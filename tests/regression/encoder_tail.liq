%include "../test.liq"

# Encoders keep the end of their input. %mp3 left its last buffer unencoded,
# %shine its last partial pass and the samples still in its delay line, and
# %fdkaac never wrote its delay out: its flush returned nothing. A 10ms burst
# at the very end of the input must be in the decoded file.

pending = ref(0)

def check(name, format, ext) =
  pending := pending() + 1
  s =
    sequence(
      [once(blank(duration=1.99)), once(sine(amplitude=0.8, duration=0.01))]
    )

  clock.assign_new(sync="none", [s])

  def on_close(path) =
    out =
      process.read(
        "ffmpeg -nostats -i #{process.quote(path)} -af volumedetect -f null - \
         2>&1"
      )

    m = r/max_volume: (-?[0-9.]+) dB/.exec(out)
    level = float_of_string(default=-200., m[1])
    print(
      "#{name}: max_volume #{level} dB"
    )
    if
      level > -20.
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
    file.temp(cleanup=true, "encoder_tail_#{name}", ext),
    s
  )
end

%ifencoder %mp3
check("mp3", %mp3, ".mp3")
%endif
%ifencoder %shine
check("shine", %shine, ".mp3")
%endif
%ifencoder %fdkaac
check("fdkaac", %fdkaac, ".aac")
%endif
if pending() == 0 then test.skip() end
