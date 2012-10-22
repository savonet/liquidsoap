%ifdef input.gstreamer.video
# Stream from a video4linux 2 input device, such as a webcam.
# @category Source / Input
# @param ~id Force the value of the source ID.
# @param ~clock_safe Force the use of the dedicated v4l clock.
# @param ~device V4L2 device to use.
def input.v4l2(~id="",~clock_safe=true,~device="/dev/video0")
  pipeline = "v4l2src device=#{device}"
  input.gstreamer.video(id=id, clock_safe=clock_safe, pipeline=pipeline)
end

# Stream from a video4linux 2 input device, such as a webcam.
# @category Source / Input
# @param ~id Force the value of the source ID.
# @param ~clock_safe Force the use of the dedicated v4l clock.
# @param ~device V4L2 device to use.
def input.v4l2_with_audio(~id="",~clock_safe=true,~device="/dev/video0")
  audio_pipeline = "autoaudiosrc"
  video_pipeline = "v4l2src device=#{device}"
  input.gstreamer.audio_video(id=id, clock_safe=clock_safe, audio_pipeline=audio_pipeline, video_pipeline=video_pipeline)
end

def gstreamer.encode_x264_avi(fname, source)
  output.gstreamer.video(pipeline="videoconvert ! x264enc ! avimux ! filesink location=\"#{fname}\"", source)
end

def gstreamer.encode_jpeg_avi(fname, source)
  output.gstreamer.video(pipeline="videoconvert ! jpegenc ! avimux ! filesink location=\"#{fname}\"", source)
end

def gstreamer.encode_mp3(fname, source)
  output.gstreamer.audio(pipeline="audioconvert ! lamemp3enc ! filesink location=\"#{fname}\"", source)
end
%endif