# VU meter: display the audio volume (RMS in dB) on the standard output.
# @category Source / Visualization
# @flag extra
# @param ~rms_min Minimal volume (dB).
# @param ~rms_max Maximal volume (dB).
# @param ~scroll Update the display in the same line.
# @param ~window Duration in seconds of volume computation.
def vumeter(~rms_min=-25., ~rms_max=-5., ~window=0.5, ~scroll=false, s)
  screen_width = 80
  bar_width = screen_width
  let s = rms(duration=window, s)
  def display()
    v = dB_of_lin(s.rms())
    x = (v - rms_min) / (rms_max - rms_min)
    x = if x < 0. then 0. else x end
    x = if x > 1. then 1. else x end
    n = int_of_float(x * float_of_int(bar_width))
    bar = ref("")
    if not scroll then bar := "\r" end
    for _ = 0 to n-1 do bar := bar() ^ "=" end
    for _ = 0 to bar_width-n-1 do bar := bar() ^ "." end
    bar := bar() ^ " " ^ string(v)
    if scroll then bar := bar() ^ "\n" end
    print(newline=false, bar())
  end
  thread.run(fast=true, every=window, display)
  s
end

# VU meter: display the audio volume (RMS in dB). This adds a video track to the
# source.
# @category Source / Visualization
# @flag extra
# @param ~rms_min Minimal volume (dB).
# @param ~rms_max Maximal volume (dB).
# @param ~window Duration in seconds of volume computation.
# @param ~color Color of the display (0xRRGGBB).
# @param ~persistence Persistence of the display (s).
def video.vumeter(~rms_min=-35., ~rms_max=0., ~window=0.1, ~color=0xff0000, ~persistence=0., s)
  s = source(s.{video = source.tracks(blank()).video})
  s = rms(duration=window, s)
  height = video.frame.height()
  width = ref(0)
  def update()
    v = dB_of_lin(s.rms())
    x = (v - rms_min) / (rms_max - rms_min)
    x = if x < 0. then 0. else x end
    x = if x > 1. then 1. else x end
    width := int_of_float(x * float_of_int(video.frame.width()))
  end
  thread.run(fast=true, every=window, update)
  s = video.add_rectangle(width=width, height=height, color=color, s)
  video.persistence(duration=persistence, s)
end
