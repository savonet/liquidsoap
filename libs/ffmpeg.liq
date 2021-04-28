%ifdef input.ffmpeg
# Stream from a video4linux2 input device, such as a webcam.
# @category Source / Input
# @flag extra
# @param ~id Force the value of the source ID.
# @param ~device V4L2 device to use.
def input.v4l2(~id="",~device="/dev/video0")
  (input.ffmpeg(format="v4l2", device):source(audio=none))
end
%endif