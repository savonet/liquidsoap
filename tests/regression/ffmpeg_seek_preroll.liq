# Seeking used to leave the ffmpeg decoder without the packets preceding the
# seek point, and mp3 stays silent until its bit reservoir has caught up: the
# first ~150ms after a seek were blank, which is what made a CUE playlist click
# between tracks (#3971). The lower the bitrate, the longer the blank, hence
# the 32k encode below.

tmp = file.temp("liq-seek-preroll", ".mp3")

encoded = ref(false)

def on_close(_) =
  encoded := true
end

tone = once(sine(duration=4.))
clock.assign_new(sync="none", [tone])

output.file(
  fallible=true,
  on_close=on_close,
  %mp3(bitrate = 32, samplerate = 44100, mono),
  tmp,
  tone
)

def next() =
  if encoded() then request.create(tmp) else null end
end

src = request.dynamic(next)

# Shorter than the blank we are looking for, so it is either all silence or all
# tone.
window = 0.1
s = rms(duration=window, src)
clock.assign_new(sync="none", [s])

seeked = ref(false)
elapsed = ref(0.)

def check() =
  if
    not seeked()
  then
    if
      src.is_ready()
    then
      ignore(src.seek(2.))
      seeked := true
    end
  elsif
    elapsed() < window
  then
    elapsed := elapsed() + settings.frame.duration()
  else
    v = s.rms()
    if
      0.1 < v
    then
      test.pass()
    else
      print(
        "rms over the #{window}s following the seek: #{v}"
      )
      test.fail()
    end
  end
end

s.on_frame(synchronous=true, check)

output.dummy(fallible=true, s)

thread.run(delay=30., test.fail)
