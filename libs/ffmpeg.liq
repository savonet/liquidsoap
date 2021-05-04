%ifdef input.ffmpeg
# Stream from a video4linux2 input device, such as a webcam.
# @category Source / Input
# @flag extra
# @param ~id Force the value of the source ID.
# @param ~buffer How much to buffer in advance (in seconds).
# @param ~device V4L2 device to use.
def input.v4l2(~id="", ~buffer=0.5, ~device="/dev/video0")
  (input.ffmpeg(format="v4l2", buffer=buffer, device):source(audio=none))
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
      null(font)
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
def ffmpeg.filter.audio_video.output(~id="", ~buffer=0.1, ~fps=null(), graph, audio, video)
  a = ffmpeg.filter.audio.output(id=id, buffer=buffer, graph, audio)
  v = ffmpeg.filter.video.output(id=id, buffer=buffer, fps=fps, graph, video)
  mux_audio(audio=a, v)
end
%endif
