%ifdef input.ffmpeg
# Stream from a video4linux2 input device, such as a webcam.
# @category Source / Input
# @flag extra
# @param ~id Force the value of the source ID.
# @param ~device V4L2 device to use.
def input.v4l2(~id="",~device="/dev/video0")
  (input.ffmpeg(format="v4l2", device):source(audio=none))
end

# Test video source
# @category Source / Video Processing
# @param ~kind Kind of the source: `"testsrc"`, `"smptebars"` or `"rgbtestsrc"`.
# @param ~buffer How much to buffer in advance (in seconds).
# @param ~duration Duration of the source.
def video.testsrc.ffmpeg(~id="", ~kind="testsrc", ~buffer=0.5, ~duration=null())
  if not list.mem(kind, ["testsrc", "smptebars", "rgbtestsrc"]) then failwith("invalid kind for video.testsrc.ffmpeg") end
  size = "size=#{video.frame.width()}x#{video.frame.height()}"
  rate = "rate=#{video.frame.rate()}"
  duration = if null.defined(duration) then ":duration=#{duration}" else "" end
  src = "#{kind}=#{size}:#{rate}#{duration}"
  (input.ffmpeg(buffer=buffer, format="lavfi", src):source(audio=none))
end
%endif
