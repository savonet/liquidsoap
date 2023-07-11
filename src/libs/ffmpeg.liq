%ifdef track.ffmpeg.raw.encode.audio
let ffmpeg.encode = ()
let ffmpeg.raw.encode = ()

# Encode a source's audio content
# @category Source / Conversion
def ffmpeg.encode.audio(~id=null("ffmpeg.encode.audio"), f, s) =
  audio = track.ffmpeg.encode.audio(f, source.tracks(s).audio)
  source(id=id, {
    track_marks = track.track_marks(audio),
    metadata    = track.metadata(audio),
    audio       = audio
  })
end

# Encode a source's audio content
# @category Source / Conversion
def ffmpeg.raw.encode.audio(~id=null("ffmpeg.raw.encode.audio"), f, s) =
  audio = track.ffmpeg.raw.encode.audio(f, source.tracks(s).audio)
  source(id=id, {
    track_marks = track.track_marks(audio),
    metadata    = track.metadata(audio),
    audio       = audio
  })
end

# Encode a source's video content
# @category Source / Conversion
def ffmpeg.encode.video(~id=null("ffmpeg.encode.video"), f, s) =
  video = track.ffmpeg.encode.video(f, source.tracks(s).video)
  source(id=id, {
    track_marks = track.track_marks(video),
    metadata    = track.metadata(video),
    video       = video
  })
end

# Encode a source's video content
# @category Source / Conversion
def ffmpeg.raw.encode.video(~id=null("ffmpeg.raw.encode.video"), f, s) =
  video = track.ffmpeg.raw.encode.video(f, source.tracks(s).video)
  source(id=id, {
    track_marks = track.track_marks(video),
    metadata    = track.metadata(video),
    video       = video
  })
end

# Encode a source's audio and video content
# @category Source / Conversion
def ffmpeg.encode.audio_video(~id=null("ffmpeg.encode.audio_video"), f, s) =
  let {audio, video} = source.tracks(s)
  audio = track.ffmpeg.encode.audio(f, audio)
  video = track.ffmpeg.encode.video(f, video)
  source(id=id, {
    track_marks = track.track_marks(audio),
    metadata    = track.metadata(audio),
    audio       = audio,
    video       = video
  })
end

# Encode a source's audio and video content
# @category Source / Conversion
def ffmpeg.raw.encode.audio_video(~id=null("ffmpeg.raw.encode.audio_video"), f, s) =
  let {audio, video} = source.tracks(s)
  audio = track.ffmpeg.raw.encode.audio(f, audio)
  video = track.ffmpeg.raw.encode.video(f, video)
  source(id=id, {
    track_marks = track.track_marks(audio),
    metadata    = track.metadata(audio),
    audio       = audio,
    video       = video
  })
end
%endif

%ifdef track.ffmpeg.decode.audio
let ffmpeg.decode = ()
let ffmpeg.raw.decode = ()

# Decode a source's audio content
# @category Source / Conversion
def ffmpeg.decode.audio(~id=null("ffmpeg.decode.audio"), s) =
  audio = track.ffmpeg.decode.audio(source.tracks(s).audio)
  source(id=id, {
    track_marks = track.track_marks(audio),
    metadata    = track.metadata(audio),
    audio       = audio
  })
end

# Decode a source's audio content
# @category Source / Conversion
def ffmpeg.raw.decode.audio(~id=null("ffmpeg.raw.decode.audio"), s) =
  audio = track.ffmpeg.raw.decode.audio(source.tracks(s).audio)
  source(id=id, {
    track_marks = track.track_marks(audio),
    metadata    = track.metadata(audio),
    audio       = audio
  })
end

# Decode a source's video content
# @category Source / Conversion
def ffmpeg.decode.video(~id=null("ffmpeg.decode.video"), s) =
  video = track.ffmpeg.decode.video(source.tracks(s).video)
  source(id=id, {
    track_marks = track.track_marks(video),
    metadata    = track.metadata(video),
    video       = video
  })
end

# Decode a source's video content
# @category Source / Conversion
def ffmpeg.raw.decode.video(~id=null("ffmpeg.raw.decode.video"), s) =
  video = track.ffmpeg.raw.decode.video(source.tracks(s).video)
  source(id=id, {
    track_marks = track.track_marks(video),
    metadata    = track.metadata(video),
    video       = video
  })
end

# Decode a source's audio and video content
# @category Source / Conversion
def ffmpeg.decode.audio_video(~id=null("ffmpeg.decode.audio_video"), s) =
  let {audio, video} = source.tracks(s)
  audio = track.ffmpeg.decode.audio(audio)
  video = track.ffmpeg.decode.video(video)
  source(id=id, {
    track_marks = track.track_marks(audio),
    metadata    = track.metadata(audio),
    audio       = audio,
    video       = video
  })
end

# Decode a source's audio and video content
# @category Source / Conversion
def ffmpeg.raw.decode.audio_video(~id=null("ffmpeg.raw.decode.audio_video"), s) =
  let {audio, video} = source.tracks(s)
  audio = track.ffmpeg.raw.decode.audio(audio)
  video = track.ffmpeg.raw.decode.video(video)
  source(id=id, {
    track_marks = track.track_marks(audio),
    metadata    = track.metadata(audio),
    audio       = audio,
    video       = video
  })
end
%endif

%ifdef input.ffmpeg
# Stream from a video4linux2 input device, such as a webcam.
# @category Source / Input
# @param ~id Force the value of the source ID.
# @param ~max_buffer Maximum data buffer in seconds
# @param ~device V4L2 device to use.
def input.v4l2(~id=null(), ~max_buffer=0.5, ~device="/dev/video0")
  (input.ffmpeg(id=id, format="v4l2", max_buffer=max_buffer, device):source(video=canvas))
end

# A test video source, which generates various patterns.
# @category Source / Video processing
# @param ~pattern Pattern drawn in the video: `"testsrc"`, `"testsrc2"`, `"smptebars"`, `"pal75bars"`, `"pal100bars"`, `"smptehdbars"`, `"yuvtestsrc"` or `"rgbtestsrc"` and more. Support any of the patterns supported by `ffmpeg`.
# @param ~max_buffer Maximum data buffer in seconds
# @param ~duration Duration of the source.
def video.testsrc.ffmpeg(~id=null(), ~pattern="testsrc", ~max_buffer=0.5, ~duration=null())
  size = "size=#{settings.frame.video.width()}x#{settings.frame.video.height()}"
  rate = "rate=#{settings.frame.video.framerate()}"
  duration = if null.defined(duration) then ":duration=#{duration}" else "" end
  src = "#{pattern}=#{size}:#{rate}#{duration}"
  (input.ffmpeg(id=id, max_buffer=max_buffer, format="lavfi", src):source(video=canvas))
end

# Read an RTMP stream.
# @category Source / Input
# @param ~max_buffer Maximum data buffer in seconds
# @param ~listen Act as a RTMP server and wait for incoming connection
# @param url URL to read RTMP from, in the form `rtmp://IP:PORT/ENDPOINT`
def input.rtmp(~id=null(), ~max_buffer=5., ~listen=true, url)
  input.ffmpeg(id=id, max_buffer=max_buffer, format="live_flv", self_sync=true,
               int_args=[("listen", listen ? 1 : 0)], url)
end
%endif

%ifdef ffmpeg.filter.drawtext
let video.add_text.ffmpeg = ()
let video.add_text.ffmpeg.raw = ()

# Display a text. Use this operator inside ffmpeg filters with a ffmpeg video input
# Returns a ffmpeg video output with `on_change` and `on_metadata` methods to be used
# to update the output text.
# @category Source / Video processing
# @param ~color Text color (in 0xRRGGBB format).
# @param ~cycle Cycle text when it reaches left boundary.
# @param ~font Path to ttf font file.
# @param ~metadata Change text on a particular metadata (empty string means disabled).
# @param ~size Font size.
# @param ~speed Horizontal speed in pixels per second (0 means no scrolling and update \
#               according to x and y in case they are variable).
# @param ~graph a ffmpeg filter graph to attach this filter to.
# @param ~x x offset.
# @param ~y y offset.
# @params d Text to display.
# @method on_change Method to call when parameters have changed to update the filter's rendered out, including when text changes.
# @method on_metadata Method to call on new metadata.
def video.add_text.ffmpeg.raw.filter(
                              ~color=0xffffff,~cycle=true,~font=null(),
                              ~metadata=null(),~size=18,~speed=70,~x=getter(10),~y=getter(10),
                              ~graph, d=getter(""), s) =
  color = "0x" ^ string.hex_of_int(pad=6, color)

  x =
    if speed != 0 then
      last_time = ref(time())
      changed = getter.changes(x)
      effective_x = ref(getter.get(x))
      getter({
        begin
          cur_time = time()
          traveled_to = int(float(speed) * (cur_time - last_time()))
          last_time := cur_time
          if changed() then
            effective_x := getter.get(x)
          else
            effective_x := effective_x() - traveled_to
          end
          if effective_x() < 0 then
            effective_x := settings.frame.video.width() - effective_x()
          end
          effective_x()
        end})
    else
      x
    end

  filter = ffmpeg.filter.drawtext.create(
                                  fontfile=font, fontsize="#{size}", x="#{getter.get(x)}", y="#{getter.get(y)}",
                                  fontcolor=color, text=getter.get(d), graph)

  def escape =
    def special_char(~encoding=_, s) =
      string.contains(substring=s, "(',%,\\,:,{,})")
    end
    def escape_char(~encoding=_, s) =
      "\\#{s}"
    end
    fun (s) -> string.escape(special_char=special_char,
                             escape_char=escape_char, s)
  end

  def escaped_text() =
    escape(getter.get(d))
  end

  filters = [{
    args = getter({"x=#{getter.get(x)}:y=#{getter.get(y)}:text=#{escaped_text()}"}),
    filter = filter
  }]

  filters =
    if cycle then
      x = getter({"min(#{getter.get(x)}-w,#{getter.get(x)}-text_w)"})
      [...filters,{
        args   = getter({"x=#{getter.get(x)}:y=#{getter.get(y)}:text=#{escaped_text()}"}),
        filter = ffmpeg.filter.drawtext.create(
                                      fontfile=font, fontsize="#{size}", x="#{getter.get(x)}", y="#{getter.get(y)}",
                                      fontcolor=color, text=getter.get(d), graph)}]
    else filters end

  changed = getter.changes(getter({(getter.get(x),getter.get(y),getter.get(d))}))
  def on_change()
    ignore(getter.get(x))
    ignore(getter.get(y))
    ignore(getter.get(d))
    if changed() then
      list.iter((fun (el) ->
        ignore(el.filter.process_command("reinit", getter.get(el.args)))), filters)
    end
  end

  def on_metadata(m) =
    if null.defined(metadata) then
      meta = (null.get(metadata):string)
      d = escape(m[meta])
      if d != "" then
        log(level=3,  label="ffmpeg.filter.drawtext", "Setting new text #{d} from metadata #{meta}")
        list.iter((fun (el) ->
           ignore(el.filter.process_command("reinit", "text=#{d}"))), filters)
      end
    end
  end

  v = list.fold((fun (cur, el) ->
    begin
      el.filter.set_input(cur)
      el.filter.output
    end), s, filters)

   v.{on_change = on_change, on_metadata = on_metadata}
end

# Display a text. Use this operator inside ffmpeg filters with a input source
# @category Source / Video processing
# @param ~color Text color (in 0xRRGGBB format).
# @param ~cycle Cycle text when it reaches left boundary.
# @param ~font Path to ttf font file.
# @param ~metadata Change text on a particular metadata (empty string means disabled).
# @param ~size Font size.
# @param ~speed Horizontal speed in pixels per second (0 means no scrolling and update \
#               according to x and y in case they are variable).
# @param ~graph a ffmpeg filter graph to attach this filter to.
# @param ~x x offset.
# @param ~y y offset.
# @params d Text to display.
def replaces video.add_text.ffmpeg.raw(
                              ~color=0xffffff,~cycle=true,~font=null(),
                              ~metadata=null(),~size=18,~speed=70,~x=getter(10),~y=getter(10),
                              ~graph, d=getter(""), s) =
  on_frame = ref (fun () -> ())
  on_metadata = ref (fun (_) -> ())
  s = source.on_metadata(s, fun (m) -> begin
    fn = on_metadata()
    fn(m)
  end)
  s = source.on_frame(s, fun () -> begin
    fn = on_frame()
    fn()
  end)

  s = ffmpeg.filter.video.input(graph, source.tracks(s).video)
  v = video.add_text.ffmpeg.raw.filter(color=color, cycle=cycle,font=font,
                                       metadata=metadata,size=size,speed=speed,
                                       x=x,y=y,graph=graph,d,s)

  on_frame := v.on_change
  on_metadata := v.on_metadata

  ffmpeg.filter.null(graph, v)
end

# Display a text.
# @category Source / Video processing
# @param ~id Force the value of the source ID.
# @param ~color Text color (in 0xRRGGBB format).
# @param ~cycle Cycle text when it reaches left boundary.
# @param ~font Path to ttf font file.
# @param ~metadata Change text on a particular metadata (empty string means disabled).
# @param ~size Font size.
# @param ~speed Horizontal speed in pixels per second (0 means no scrolling and update \
#               according to x and y in case they are variable).
# @param ~x x offset.
# @param ~y y offset.
# @params d Text to display.
def video.add_text.ffmpeg.internal(~id=null(),~color=0xffffff,~cycle=true,~font=null(),
                          ~duration=null(), ~metadata=null(),~size=18,~speed=70,
                          ~x=getter(10),~y=getter(10),
                          d, s) =
  id = string.id.default(default="video.add_text.ffmpeg", id)
  s = ffmpeg.raw.encode.audio_video(%ffmpeg(%audio.raw, %video.raw), s)
  v = source({video=source.tracks(s).video})
  def mkfilter(graph)
    v = video.add_text.ffmpeg.raw(color=color,cycle=cycle,font=font,metadata=metadata,
                                  size=size,speed=speed,x=x,y=y,graph=graph,d,v)
    ffmpeg.filter.video.output(graph, v)
  end
  video = ffmpeg.filter.create(mkfilter)
  tracks = source.tracks(s)
  s = source(id=id,tracks.{video=video})
  s = ffmpeg.raw.decode.audio_video(s)
  null.defined(duration) ? max_duration(null.get(duration), s) : s
end

let replaces video.add_text.ffmpeg = video.add_text.ffmpeg.internal
# video.add_text.available := [("ffmpeg", video.add_text.ffmpeg.internal), ...video.add_text.available()]

# if settings.video.add_text() != "sdl" then
#   settings.video.add_text.set("ffmpeg")
# end
%endif

%ifdef ffmpeg.filter.video.output
let ffmpeg.filter.audio_video = ()

# Return a source with audio and video from a filter's output.
# @category Source / Output
# @param id Force the value of the source ID.
def ffmpeg.filter.audio_video.output(~id=null(), graph, audio, video)
  audio = ffmpeg.filter.audio.output(graph, audio.tracks().audio)
  video = ffmpeg.filter.video.output(graph, video.tracks().video)
  source(id=id, {audio=audio, video=video})
end
%endif
