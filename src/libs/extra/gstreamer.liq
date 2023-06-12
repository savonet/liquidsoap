%ifdef input.gstreamer.video

# Stream from a video4linux2 input device, such as a webcam.
# @category Source / Input
# @flag extra
# @param ~id Force the value of the source ID.
# @param ~device V4L2 device to use.
# @param ~on_error Callback executed when an error happens.
def input.v4l2(~id=null(),~device="/dev/video0",~on_error=fun(_)->3.)
  pipeline = "v4l2src device=#{device}"
  input.gstreamer.video(id=id, pipeline=pipeline, on_error=on_error)
end

# Stream from a video4linux2 input device, such as a webcam.
# @category Source / Input
# @flag extra
# @param ~id Force the value of the source ID.
# @param ~device V4L2 device to use.
# @param ~on_error Callback executed when an error happens.
def input.v4l2_with_audio(~id=null(),~device="/dev/video0",~on_error=fun(_)->3.)
  audio_pipeline = "autoaudiosrc"
  video_pipeline = "v4l2src device=#{device}"
  input.gstreamer.audio_video(id=id, audio_pipeline=audio_pipeline, video_pipeline=video_pipeline, on_error=on_error)
end

let gstreamer = ()
let gstreamer.single = ()

# Play a single audio file using GStreamer.
# @category Source / Input
# @flag extra
# @param ~id Force the value of the source ID.
# @param ~on_error Callback executed when an error happens.
# @param uri URI of the file to be played.
def gstreamer.single.audio(~id=null(),~on_error=fun(_)->3.,uri) =
  uri = getter.function(uri)
  input.gstreamer.audio(id=id, on_error=on_error, pipeline={"filesrc location=\"#{uri()}\""})
end


# Play an http live stream.
# @category Source / Input
# @flag extra
# @param ~id Force the value of the source ID.
# @param ~on_error Callback executed when an error happens.
# @param uri URI of the HLS stream index.
def output.file.hls.gstreamer(~id=null(),~on_error=fun(_)->3.,uri) =
  uri = getter.function(uri)
  pipeline = {"souphttpsrc location=\"#{uri()}\" ! tee name=t"}
  audio_pipeline = "t. ! queue"
  video_pipeline = "t. ! queue"
  input.gstreamer.audio_video(id=id, pipeline=pipeline, audio_pipeline=audio_pipeline, video_pipeline=video_pipeline, on_error=on_error)
end

# Play an audio-only http live stream.
# @category Source / Input
# @flag extra
# @param ~id Force the value of the source ID.
# @param ~on_error Callback executed when an error happens.
# @param uri URI of the HLS stream index.
def output.file.hls.gstreamer.audio(~id=null(),~on_error=fun(_)->3.,uri) =
  uri = getter.function(uri)
  pipeline = {"souphttpsrc location=\"#{uri()}\""}
  input.gstreamer.audio(id=id, pipeline=pipeline, on_error=on_error)
end

# Encode an x264 video file using gstreamer
# @category Source / Output
# @flag extra
# @param fname Encoded file name
# @param source Source
def gstreamer.encode_x264_avi(fname, source)
  output.gstreamer.video(pipeline="videoconvert ! x264enc ! avimux ! filesink location=\"#{fname}\"", source)
end

# Encode jpeg video file using gstreamer
# @category Source / Output
# @flag extra
# @param fname Encoded file name
# @param source Source
def gstreamer.encode_jpeg_avi(fname, source)
  output.gstreamer.video(pipeline="videoconvert ! jpegenc ! avimux ! filesink location=\"#{fname}\"", source)
end

# Encode a mp3 file using gstreamer
# @category Source / Output
# @flag extra
# @param fname Encoded file name
# @param source Source
def gstreamer.encode_mp3(fname, source)
  output.gstreamer.audio(pipeline="audioconvert ! lamemp3enc ! filesink location=\"#{fname}\"", source)
end

let gstreamer.rtp = ()

# Broadcast a video in RTP. In order to play it, save the following in `xxx.sdp`
# and use `vlc xxx.sdp`:
# ```
# v=0
# m=video 5000 RTP/AVP 96
# c=IN IP4 127.0.0.1
# a=rtpmap:96 MP4V-ES/90000
# ```
# @category Source / Output
# @flag extra
def gstreamer.rtp.mpeg4(~host="127.0.0.1",~port=5000,source)
  output.gstreamer.video(pipeline="videoconvert ! avenc_mpeg4 ! rtpmp4vpay config-interval=2 ! udpsink host=#{host} port=#{port}", source)
end

# Stream live on youtube. You need the following Gstreamer plugins: flvmux, rtmpsink, x264enc and a suitable AAC encoder (see `audio_encoder` params).
# @param ~id Source ID
# @param ~video_bitrate Video bitrate
# @param ~audio_encoder Audio encoder. Can be one of: "fdkaacenc", "voaacenc"
# @param ~audio_bitrate Audio bitrate
# @param ~url Server URL
# @param ~key Secret key
# @param source Source to stream
# @category Source / Output
# @flag extra
def output.youtube.live.gstreamer(~id=null(),
  ~video_bitrate=2000,
  ~audio_encoder="fdkaacenc",
  ~audio_bitrate=128000,
  ~url="rtmp://a.rtmp.youtube.com/live2",
  ~key,
  source) =
  video_pipeline = "videoconvert ! \
      x264enc bitrate=#{video_bitrate} byte-stream=false key-int-max=60 bframes=0 aud=true tune=zerolatency ! \
      video/x-h264,profile=main ! queue ! mux."
  audio_pipeline = "audioconvert ! #{audio_encoder} bitrate=#{audio_bitrate} ! queue ! mux."
  key = (key : string)
  pipeline = "flvmux streamable=true name=mux ! rtmpsink location=\"#{url}/#{key} app=live2\""

  output.gstreamer.audio_video(id=id,video_pipeline=video_pipeline,
    audio_pipeline=audio_pipeline,pipeline=pipeline,source)
end

# Test audio-video source using GStreamer.
# @category Source / Input
# @flag extra
def video.testsrc.gstreamer(~id=null())
  video_pipeline="videotestsrc"
  audio_pipeline="audiotestsrc"
  input.gstreamer.audio_video(id=id,video_pipeline=video_pipeline, audio_pipeline=audio_pipeline)
end
%endif
