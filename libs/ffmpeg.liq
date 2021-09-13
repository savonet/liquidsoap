%ifdef input.ffmpeg
# Stream from a video4linux2 input device, such as a webcam.
# @category Source / Input
# @param ~id Force the value of the source ID.
# @param ~max_buffer Maximum data buffer in seconds
# @param ~device V4L2 device to use.
def input.v4l2(~id=null(), ~max_buffer=0.5, ~device="/dev/video0")
  (input.ffmpeg(id=id, format="v4l2", max_buffer=max_buffer, device):source(audio=none))
end

# A test video source, which generates various patterns.
# @category Source / Video Processing
# @param ~pattern Pattern drawn in the video: `"testsrc"`, `"testsrc2"`, `"smptebars"` or `"rgbtestsrc"`.
# @param ~max_buffer Maximum data buffer in seconds
# @param ~duration Duration of the source.
def video.testsrc(~id=null(), ~pattern="testsrc", ~max_buffer=0.5, ~duration=null())
  if not list.mem(pattern, ["testsrc", "testsrc2", "smptebars", "rgbtestsrc"]) then failwith("invalid pattern for video.testsrc.ffmpeg") end
  size = "size=#{video.frame.width()}x#{video.frame.height()}"
  rate = "rate=#{video.frame.rate()}"
  duration = if null.defined(duration) then ":duration=#{duration}" else "" end
  src = "#{pattern}=#{size}:#{rate}#{duration}"
  (input.ffmpeg(id=id, max_buffer=max_buffer, format="lavfi", src):source(audio=none))
end

# Read an RTMP stream.
# @category Source / Input
# @param ~max_buffer Maximum data buffer in seconds
# @param ~timeout Timeout for source connection
# @param url URL to read RTMP from, in the form `rtmp://IP:PORT/ENDPOINT`
def input.rtmp(~max_buffer=5., ~timeout=10., url)
  input.ffmpeg(max_buffer=max_buffer, log_overfull=false, format="live_flv",
               int_args=[("listen", 1), ("rw_timeout", int_of_float(timeout * 1000000.))],
                url)
end
%endif

%ifdef ffmpeg.filter.drawtext
# Display a text.
# @category Source / Video Processing
# @param ~color Color of the text.
# @param ~font Font to use.
# @param ~size Size of the text.
# @param ~x x offset.
# @param ~y y offset.
# @param text Text to display.
# @param s Source on which the text should be added.
def video.add_text.ffmpeg(~color=0xffffff, ~font="", ~size=18, ~x=10, ~y=10, text, s)
  color = "0x" ^ string.hex_of_int(pad=6, color)
  fontfile =
    if font == "" then
      null()
    else
      font
    end
  def mkfilter(graph)
    s = ffmpeg.filter.video.input(graph, s)
    s = ffmpeg.filter.drawtext(fontfile=fontfile, fontsize="#{size}", x="#{x}", y="#{y}", fontcolor=color, text=text, graph, s)
    ffmpeg.filter.video.output(graph, s)
  end
  mux_audio(audio=drop_video(s), ffmpeg.filter.create(mkfilter))
end
%endif

%ifdef ffmpeg.filter.video.output
let ffmpeg.filter.audio_video = ()

# Return a source with audio and video from a filter's output.
# @category Source / Output
# @param id Force the value of the source ID.
# @param buffer Duration of the pre-buffered data.
# @param fps Output frame per seconds. Defaults to global value.
def ffmpeg.filter.audio_video.output(~id=null(), ~buffer=0.1, ~fps=null(), graph, audio, video)
  a = ffmpeg.filter.audio.output(id=id, buffer=buffer, graph, audio)
  v = ffmpeg.filter.video.output(id=id, buffer=buffer, fps=fps, graph, video)
  mux_audio(audio=a, v)
end
%endif

%ifencoder %ffmpeg
# Output an MPEG-DASH playlist.
# @category Output
# @flag extra
# @param ~id Force the value of the source ID.
# @param ~codec Codec to use for audio (following FFmpeg's conventions).
# @param ~fallible Allow the child source to fail, in which case the output will be (temporarily) stopped.
# @param ~on_start Callback executed when outputting starts.
# @param ~on_stop Callback executed when outputting stops.
# @param ~start Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
# @param ~playlist Playlist name
# @param ~directory Directory to write to
def output.file.dash(~id=null(), ~fallible=false,
                     ~on_start={()}, ~on_stop={()}, ~codec="libmp3lame", ~bitrate=128,
                     ~start=true, ~playlist="stream.mpd", ~directory, s)
  enc = %ffmpeg(format="dash", %audio(codec=codec, b="#{bitrate}k"))
  output.file(id=id, fallible=fallible, on_start=on_start, on_stop=on_stop, start=start, enc, "#{directory}/#{playlist}", s)
end
%endif
