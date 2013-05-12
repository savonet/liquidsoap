%ifdef input.gstreamer.video

# Stream from a video4linux 2 input device, such as a webcam.
# @category Source / Input
# @param ~id Force the value of the source ID.
# @param ~device V4L2 device to use.
def input.v4l2(~id="",~device="/dev/video0")
  pipeline = "v4l2src device=#{device}"
  input.gstreamer.video(id=id, pipeline=pipeline)
end

# Stream from a video4linux 2 input device, such as a webcam.
# @category Source / Input
# @param ~id Force the value of the source ID.
# @param ~device V4L2 device to use.
def input.v4l2_with_audio(~id="",~device="/dev/video0")
  audio_pipeline = "autoaudiosrc"
  video_pipeline = "v4l2src device=#{device}"
  input.gstreamer.audio_video(id=id, audio_pipeline=audio_pipeline, video_pipeline=video_pipeline)
end

# Play an http live stream.
# @category Source / Input
# @param ~id Force the value of the source ID.
# @param uri URI of the HLS stream index.
def gstreamer.hls(~id="",uri) =
  pipeline = "souphttpsrc location=#{uri} ! tee name=t"
  audio_pipeline = "t. ! queue"
  video_pipeline = "t. ! queue"
  input.gstreamer.audio_video(id=id, pipeline=pipeline, audio_pipeline=audio_pipeline, video_pipeline=video_pipeline)
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

# Broadcast a video in RTP. In order to play it, save the following in xxx.sdp
# and use vlc xxx.sdp:
# v=0
# m=video 5000 RTP/AVP 96
# c=IN IP4 127.0.0.1
# a=rtpmap:96 MP4V-ES/90000
def gstreamer.rtp.mpeg4(~host="127.0.0.1",~port=5000,source)
  output.gstreamer.video(pipeline="videoconvert ! avenc_mpeg4 ! rtpmp4vpay config-interval=2 ! udpsink host=#{host} port=#{port}", source)
end

%endif
