%include "../test.liq"

# Raw and copied audio against a main clock at 1/44100 (#5720): 48 kHz frames
# are 940.8 ticks long, so their positions carry rounding that the encoder
# must not turn into dropped samples or packets.
settings.decoder.decoders := ["ffmpeg"]

dir = file.temp_dir(cleanup=true, "raw-audio-timing")

def path(name) =
  "#{dir}/#{name}"
end

def ffmpeg(args) =
  r =
    process.run(
      timeout=60.,
      "ffmpeg -loglevel error -y #{args} 2>&1"
    )
  if
    r.status != "exit" or r.status.code != 0
  then
    test.fail(
      "ffmpeg #{args}: #{r.stdout}"
    )
  end
end

ffmpeg(
  "-f lavfi -i sine=sample_rate=48000 -t 20 -c:a aac -ac 2 #{path("audio.ts")}"
)
ffmpeg(
  "-f lavfi -i testsrc2=size=160x90:rate=25 -f lavfi -i sine=sample_rate=48000 \
   -t 20 -c:v libx264 -c:a aac -ac 2 #{path("audio_video.ts")}"
)
ffmpeg(
  "-i #{path("audio.ts")} -c copy -bsf:a \"noise=drop='eq(mod(n\\,100)\\,50)'\" #{
    path("missing.ts")
  }"
)
ffmpeg(
  "-i #{path("audio.ts")} -c copy -bsf:a setts=ts=TS*99/100 #{path("ahead.ts")}"
)
ffmpeg(
  "-i #{path("audio.ts")} -map 0:a -c copy -bsf:a \
   setts=dts=DTS/1920+126000:pts=PTS/1920+126000 -f nut #{path("close.nut")}"
)

def probe(stream, entries, filename) =
  lines =
    string.split(
      separator="\n",
      process.read(
        "ffprobe -v error -select_streams #{stream} -show_entries #{entries} -of \
         csv=p=0 #{process.quote(filename)}"
      )
    )
  lines = list.map(fun (line) -> r/,+$/.replace(fun (_) -> "", line), lines)
  list.filter(fun (line) -> line != "", lines)
end

def samples(filename) =
  list.fold(
    fun (total, line) -> total + int_of_string(line),
    0,
    probe("a", "frame=nb_samples", filename)
  )
end

# First pts and end (last pts + its duration) of a stream, in seconds.
def span(stream, filename) =
  times =
    list.map(
      fun (line) ->
        list.map(
          fun (v) -> float_of_string(default=0., v),
          string.split(separator=",", line)
        ),
      probe(stream, "packet=pts_time,duration_time", filename)
    )
  if
    times == []
  then
    (0., -1.)
  else
    first = list.hd(list.hd(times))
    last =
      list.fold(
        fun (last, t) -> list.nth(t, 0) > list.nth(last, 0) ? t : last,
        list.hd(times),
        times
      )
    (first, list.nth(last, 0) + list.nth(default=0., last, 1))
  end
end

def dts_steps(filename) =
  dts = list.map(int_of_string, probe("a", "packet=dts", filename))
  if
    dts == []
  then
    []
  else
    list.mapi(fun (i, d) -> d - list.nth(dts, i), list.tl(dts))
  end
end

checks = ref([])
pending = ref(0)

def check(name, ok, details) =
  print(
    "#{name}: #{ok ? 'ok' : 'FAILED'} (#{details})"
  )
  checks := [...checks(), ok]
end

def done() =
  pending := pending() - 1
  if
    pending() == 0
  then
    if
      list.length(checks()) != 7
    then
      test.fail(
        "ran #{list.length(checks())} checks, expected 7"
      )
    elsif
      list.for_all(fun (ok) -> ok, checks())
    then
      test.pass()
    else
      test.fail()
    end
  end
end

def encode(input, ~name, format, on_close) =
  pending := pending() + 1
  s = once(single(path(input)))
  clock.assign_new(sync="none", [s])
  closed = ref(false)
  output.file(
    fallible=true,
    on_close=fun (filename) ->
      if
        not closed()
      then
        closed := true
        on_close(filename)
        done()
      end,
    format,
    path(name),
    s
  )
end

raw = %ffmpeg(format = "matroska", %audio.raw(codec = "pcm_s16le"))

encode(
  "audio.ts",
  name="raw_rate.mkv",
  raw,
  fun (out) ->
    begin
      let expected = samples(path("audio.ts"))
      let got = samples(out)
      check(
        "rate mismatch keeps every sample",
        expected > 0 and got == expected,
        "#{got} of #{expected}"
      )
    end
)

encode(
  "missing.ts",
  name="raw_missing.mkv",
  raw,
  fun (out) ->
    begin
      let expected = samples(path("missing.ts"))
      let got = samples(out)
      check(
        "missing audio keeps every sample",
        expected > 0 and got == expected,
        "#{got} of #{expected}"
      )
      let (start, stop) = span("a", path("missing.ts"))
      let (start', stop') = span("a", out)
      let duration = stop - start
      let duration' = stop' - start'
      check(
        "missing audio keeps the input's span",
        duration > 19. and abs(duration' - duration) <= 0.002,
        "#{duration'} for #{duration}"
      )
    end
)

encode(
  "ahead.ts",
  name="raw_ahead.mkv",
  raw,
  fun (out) ->
    begin
      let (start, stop) = span("a", path("ahead.ts"))
      let (start', stop') = span("a", out)
      let duration = stop - start
      let duration' = stop' - start'
      check(
        "samples ahead follow the timestamps",
        duration > 19. and abs(duration' - duration) <= 0.002,
        "#{duration'} for #{duration}"
      )
    end
)

encode(
  "audio_video.ts",
  name="raw_cut.mkv",
  %ffmpeg(
    format = "matroska",
    %audio.raw(codec = "pcm_s16le"),
    %video.raw(codec = "libx264")
  ),
  fun (out) ->
    begin
      let (_, audio_end) = span("a", out)
      let (_, video_end) = span("v", out)
      check(
        "a cut at the track end trims the audio",
        video_end > 19. and abs(audio_end - video_end) <= 0.001,
        "audio ends at #{audio_end}, video at #{video_end}"
      )
    end
)

encode(
  "close.nut",
  name="copy_close.nut",
  %ffmpeg(format = "nut", %audio.copy),
  fun (out) ->
    begin
      let expected = list.length(probe("a", "packet=dts", path("close.nut")))
      let got = list.length(probe("a", "packet=dts", out))
      check(
        "copied DTS closer than a tick are kept",
        expected > 0 and got == expected,
        "#{got} of #{expected} packets"
      )
    end
)

encode(
  "audio.ts",
  name="copy_steps.ts",
  %ffmpeg(format = "mpegts", %audio.copy),
  fun (out) ->
    begin
      let expected = dts_steps(path("audio.ts"))
      let got = dts_steps(out)
      check(
        "copied DTS keep their steps",
        list.length(expected) > 0 and got == expected,
        "#{list.length(got)} steps, #{
          list.length(list.filter(fun (s) -> s != 1920, got))
        } not 1920"
      )
    end
)
