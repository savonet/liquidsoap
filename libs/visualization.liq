# VU meter: display the audio volume (RMS in dB).
# @category Visualization
# @flag extra
# @param ~rms_min Minimal volume.
# @param ~rms_max Maximal volume.
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
    for i = 0 to n-1 do bar := !bar ^ "=" end
    for i = 0 to bar_width-n-1 do bar := !bar ^ "." end
    bar := !bar ^ " " ^ string_of(v)
    if scroll then bar := !bar ^ "\n" end
    print(newline=false, !bar)
  end
  thread.run(fast=true, every=window, display)
  s
end
