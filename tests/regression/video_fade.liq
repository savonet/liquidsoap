%include "../test.liq"

# video.fade.in and video.fade.out used to discard the image their transition
# computed, so no transition was ever visible (#3180). Transitions fade to and
# from black on their own, with nothing underneath. A source of unknown length
# must not be faded out.

log.level := 4
settings.frame.video.width := 160
settings.frame.video.height := 120
settings.frame.video.framerate := 10

def brightness(filename) =
  lines =
    process.read.lines(
      "ffprobe -v error -f lavfi #{
        process.quote('movie=#{filename},signalstats')
      } -show_entries frame_tags=lavfi.signalstats.YAVG -of csv=p=0"
    )

  # ffprobe ends the first value with a stray comma.
  lines =
    list.map(fun (l) -> string.replace(pattern=",", fun (_) -> "", l), lines)
  list.map(float_of_string, list.filter(fun (l) -> l != "", lines))
end

pending = ref(4)

def check(transition) =
  s = once(video.fill(color=0xffffff, blank(duration=3.)))
  s = video.fade.in(duration=1., transition=transition, s)
  s = video.fade.out(duration=1., transition=transition, s)
  s = source.mux.audio(audio=sine(), s)
  clock.assign_new(sync="none", [s])

  def on_close(filename) =
    y = brightness(filename)
    print(
      "#{transition}: #{y}"
    )
    n = list.length(y)
    if
      n < 30
    then
      test.fail()
    else
      first = list.nth(y, 0)
      ramping = list.nth(y, 5)
      middle = list.nth(y, 15)
      last = list.nth(y, 29)
      if
        first < 50.
        and ramping > 80.
        and ramping < 180.
        and middle > 230.
        and last < 50.
      then
        pending := pending() - 1
        if pending() == 0 then test.pass() end
      else
        test.fail()
      end
    end
  end

  output.file(
    fallible=true,
    on_close=on_close,
    %ffmpeg(
      format = "mkv",
      %audio(codec = "aac"),
      %video(codec = "libx264", preset = "ultrafast", crf = 0)
    ),
    file.temp(cleanup=true, "video_fade_#{transition}", ".mkv"),
    s
  )
end

check("fade")
check("disc")
check("slide_left")

# A source with no known end must not be faded out.
def check_endless() =
  s = video.fill(color=0xffffff, blank())
  s = video.fade.out(duration=1., transition="fade", s)
  s = max_duration(2., add([video.fill(color=0x000000, blank()), s]))
  s = source.mux.audio(audio=sine(), s)
  clock.assign_new(sync="none", [s])

  def on_close(filename) =
    y = brightness(filename)
    print(
      "endless: #{y}"
    )
    if
      list.length(y) >= 10 and list.for_all(fun (y) -> y > 230., y)
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
    %ffmpeg(
      format = "mkv",
      %audio(codec = "aac"),
      %video(codec = "libx264", preset = "ultrafast", crf = 0)
    ),
    file.temp(cleanup=true, "video_fade_endless", ".mkv"),
    s
  )
end

check_endless()
