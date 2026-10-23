%ifdef input.ffmpeg
# Read a video4linux2 capture device with FFmpeg. A capture device paces its
# clock by default: a clock holds one sync source at a time, so a camera in the
# same clock as another sync source (a sound card, a second camera, a live
# network input with `self_sync`) raises the clock's sync error. Set
# `self_sync=false` on the camera, or wrap one side in `buffer()` to put it in
# its own clock.
# @category Source / Input / Active
# @param ~id Force the value of the source ID.
# @param ~max_buffer Maximum data buffered, in seconds.
# @param ~self_sync Pace the clock with the device.
# @param ~device V4L2 device to use.
def input.v4l2(
  ~id=null,
  ~max_buffer=0.5,
  ~self_sync=true,
  ~device="/dev/video0"
) =
  (
    input.ffmpeg(
      id=id,
      format="v4l2",
      max_buffer=max_buffer,
      self_sync=self_sync,
      device
    )
  :
    source(video=yuv420p)
  )
end

# Read an RTMP stream with FFmpeg, by default as a server waiting for a
# publisher.
# @category Source / Input / Active
# @param ~id Force the value of the source ID.
# @param ~max_buffer Maximum data buffered, in seconds.
# @param ~listen Wait for a publisher instead of connecting to a server.
# @param url RTMP URL, e.g. `rtmp://0.0.0.0:1935/live/stream`.
def input.rtmp(~id=null, ~max_buffer=5., ~listen=true, url) =
  input.ffmpeg(
    id=id,
    max_buffer=max_buffer,
    format="live_flv",
    self_sync=true,
    int_args=[("listen", listen ? 1 : 0)],
    url
  )
end
%endif

%ifdef track.ffmpeg.raw.encode.audio
# Encode the audio of a source with `track.ffmpeg.encode.*`. The input is read in a child clock: any other output reading the same source runs at the pace this source is read. With an infallible input, a persistent data fault is a hard error: use `mksafe` or `fallback` to keep streaming through it.
# @category Source / Conversion
# @param ~id Force the value of the source ID.
# @param f Encoding format.
# @param s Input source.
def ffmpeg.encode.audio(~id=null("ffmpeg.encode.audio"), f, s) =
  tracks = source.tracks(s)
  audio = track.ffmpeg.encode.audio(f, tracks.audio)
  source(
    id=id,
    {
      track_marks = track.track_marks(audio),
      metadata = track.metadata(audio),
      audio = audio
    }
  )
end

# Encode the video of a source with `track.ffmpeg.encode.*`. The input is read in a child clock: any other output reading the same source runs at the pace this source is read. With an infallible input, a persistent data fault is a hard error: use `mksafe` or `fallback` to keep streaming through it.
# @category Source / Conversion
# @param ~id Force the value of the source ID.
# @param f Encoding format.
# @param s Input source.
def ffmpeg.encode.video(~id=null("ffmpeg.encode.video"), f, s) =
  tracks = source.tracks(s)
  video = track.ffmpeg.encode.video(f, tracks.video)
  source(
    id=id,
    {
      track_marks = track.track_marks(video),
      metadata = track.metadata(video),
      video = video
    }
  )
end

# Encode the audio and video of a source with `track.ffmpeg.encode.*`. The input is read in a child clock: any other output reading the same source runs at the pace this source is read. With an infallible input, a persistent data fault is a hard error: use `mksafe` or `fallback` to keep streaming through it.
# @category Source / Conversion
# @param ~id Force the value of the source ID.
# @param f Encoding format.
# @param s Input source.
def ffmpeg.encode.audio_video(~id=null("ffmpeg.encode.audio_video"), f, s) =
  tracks = source.tracks(s)
  audio = track.ffmpeg.encode.audio(f, tracks.audio)
  video = track.ffmpeg.encode.video(f, tracks.video)
  source(
    id=id,
    {
      track_marks = track.track_marks(audio),
      metadata = track.metadata(audio),
      audio = audio,
      video = video
    }
  )
end

# Encode the audio of a source with `track.ffmpeg.raw.encode.*`. The input is read in a child clock: any other output reading the same source runs at the pace this source is read. With an infallible input, a persistent data fault is a hard error: use `mksafe` or `fallback` to keep streaming through it.
# @category Source / Conversion
# @param ~id Force the value of the source ID.
# @param f Encoding format.
# @param s Input source.
def ffmpeg.raw.encode.audio(~id=null("ffmpeg.raw.encode.audio"), f, s) =
  tracks = source.tracks(s)
  audio = track.ffmpeg.raw.encode.audio(f, tracks.audio)
  source(
    id=id,
    {
      track_marks = track.track_marks(audio),
      metadata = track.metadata(audio),
      audio = audio
    }
  )
end

# Encode the video of a source with `track.ffmpeg.raw.encode.*`. The input is read in a child clock: any other output reading the same source runs at the pace this source is read. With an infallible input, a persistent data fault is a hard error: use `mksafe` or `fallback` to keep streaming through it.
# @category Source / Conversion
# @param ~id Force the value of the source ID.
# @param f Encoding format.
# @param s Input source.
def ffmpeg.raw.encode.video(~id=null("ffmpeg.raw.encode.video"), f, s) =
  tracks = source.tracks(s)
  video = track.ffmpeg.raw.encode.video(f, tracks.video)
  source(
    id=id,
    {
      track_marks = track.track_marks(video),
      metadata = track.metadata(video),
      video = video
    }
  )
end

# Encode the audio and video of a source with `track.ffmpeg.raw.encode.*`. The input is read in a child clock: any other output reading the same source runs at the pace this source is read. With an infallible input, a persistent data fault is a hard error: use `mksafe` or `fallback` to keep streaming through it.
# @category Source / Conversion
# @param ~id Force the value of the source ID.
# @param f Encoding format.
# @param s Input source.
def ffmpeg.raw.encode.audio_video(
  ~id=null("ffmpeg.raw.encode.audio_video"),
  f,
  s
) =
  tracks = source.tracks(s)
  audio = track.ffmpeg.raw.encode.audio(f, tracks.audio)
  video = track.ffmpeg.raw.encode.video(f, tracks.video)
  source(
    id=id,
    {
      track_marks = track.track_marks(audio),
      metadata = track.metadata(audio),
      audio = audio,
      video = video
    }
  )
end
%endif

%ifdef track.ffmpeg.decode.audio
# Decode the audio of a source with `track.ffmpeg.decode.*`. The input is read in a child clock: any other output reading the same source runs at the pace this source is read. With an infallible input, a persistent data fault is a hard error: use `mksafe` or `fallback` to keep streaming through it.
# @category Source / Conversion
# @param ~id Force the value of the source ID.
# @param s Input source.
def ffmpeg.decode.audio(~id=null("ffmpeg.decode.audio"), s) =
  tracks = source.tracks(s)
  audio = track.ffmpeg.decode.audio(tracks.audio)
  source(
    id=id,
    {
      track_marks = track.track_marks(audio),
      metadata = track.metadata(audio),
      audio = audio
    }
  )
end

# Decode the video of a source with `track.ffmpeg.decode.*`. The input is read in a child clock: any other output reading the same source runs at the pace this source is read. With an infallible input, a persistent data fault is a hard error: use `mksafe` or `fallback` to keep streaming through it.
# @category Source / Conversion
# @param ~id Force the value of the source ID.
# @param s Input source.
def ffmpeg.decode.video(~id=null("ffmpeg.decode.video"), s) =
  tracks = source.tracks(s)
  video = track.ffmpeg.decode.video(tracks.video)
  source(
    id=id,
    {
      track_marks = track.track_marks(video),
      metadata = track.metadata(video),
      video = video
    }
  )
end

# Decode the audio and video of a source with `track.ffmpeg.decode.*`. The input is read in a child clock: any other output reading the same source runs at the pace this source is read. With an infallible input, a persistent data fault is a hard error: use `mksafe` or `fallback` to keep streaming through it.
# @category Source / Conversion
# @param ~id Force the value of the source ID.
# @param s Input source.
def ffmpeg.decode.audio_video(~id=null("ffmpeg.decode.audio_video"), s) =
  tracks = source.tracks(s)
  audio = track.ffmpeg.decode.audio(tracks.audio)
  video = track.ffmpeg.decode.video(tracks.video)
  source(
    id=id,
    {
      track_marks = track.track_marks(audio),
      metadata = track.metadata(audio),
      audio = audio,
      video = video
    }
  )
end

# Decode the audio of a source with `track.ffmpeg.raw.decode.*`. The input is read in a child clock: any other output reading the same source runs at the pace this source is read. With an infallible input, a persistent data fault is a hard error: use `mksafe` or `fallback` to keep streaming through it.
# @category Source / Conversion
# @param ~id Force the value of the source ID.
# @param s Input source.
def ffmpeg.raw.decode.audio(~id=null("ffmpeg.raw.decode.audio"), s) =
  tracks = source.tracks(s)
  audio = track.ffmpeg.raw.decode.audio(tracks.audio)
  source(
    id=id,
    {
      track_marks = track.track_marks(audio),
      metadata = track.metadata(audio),
      audio = audio
    }
  )
end

# Decode the video of a source with `track.ffmpeg.raw.decode.*`. The input is read in a child clock: any other output reading the same source runs at the pace this source is read. With an infallible input, a persistent data fault is a hard error: use `mksafe` or `fallback` to keep streaming through it.
# @category Source / Conversion
# @param ~id Force the value of the source ID.
# @param s Input source.
def ffmpeg.raw.decode.video(~id=null("ffmpeg.raw.decode.video"), s) =
  tracks = source.tracks(s)
  video = track.ffmpeg.raw.decode.video(tracks.video)
  source(
    id=id,
    {
      track_marks = track.track_marks(video),
      metadata = track.metadata(video),
      video = video
    }
  )
end

# Decode the audio and video of a source with `track.ffmpeg.raw.decode.*`. The input is read in a child clock: any other output reading the same source runs at the pace this source is read. With an infallible input, a persistent data fault is a hard error: use `mksafe` or `fallback` to keep streaming through it.
# @category Source / Conversion
# @param ~id Force the value of the source ID.
# @param s Input source.
def ffmpeg.raw.decode.audio_video(
  ~id=null("ffmpeg.raw.decode.audio_video"),
  s
) =
  tracks = source.tracks(s)
  audio = track.ffmpeg.raw.decode.audio(tracks.audio)
  video = track.ffmpeg.raw.decode.video(tracks.video)
  source(
    id=id,
    {
      track_marks = track.track_marks(audio),
      metadata = track.metadata(audio),
      audio = audio,
      video = video
    }
  )
end
%endif

%ifdef ffmpeg.filter.video.output
let ffmpeg.filter.audio_video = ()

# Return a source carrying an audio and a video output of a filter graph.
# @category Source / FFmpeg filter
# @param ~id Force the value of the returned source's ID.
# @param graph Filter graph.
# @param audio Value whose tracks hold the audio pad, under `audio`.
# @param video Value whose tracks hold the video pad, under `video`.
def ffmpeg.filter.audio_video.output(~id=null, graph, audio, video) =
  a = ffmpeg.filter.audio.output(graph, audio.tracks().audio)
  v = ffmpeg.filter.video.output(graph, video.tracks().video)
  source(id=id, {audio = a, video = v})
end
%endif

%ifdef ffmpeg.filter.drawtext
let video.add_text.ffmpeg = ()
let video.add_text.ffmpeg.raw = ()

# Escape a text for a drawtext command.
# @flag hidden
def ffmpeg.filter.drawtext.escape(text) =
  string.replace(pattern="(['%\\\\:{}])", fun (c) -> "\\#{c}", text)
end

# Draw a text on a video pad of a filter graph with `drawtext`, updated by
# commands. The text and its position are read on each call of the returned
# value's `on_change`; `on_metadata` sets the text from a metadata.
# @category Source / FFmpeg filter
# @flag extra
# @param ~color Text color (in 0xRRGGBB format).
# @param ~cycle Draw a second copy of the text so that scrolling wraps around.
# @param ~font Path to the font file.
# @param ~metadata Metadata to read the text from.
# @param ~size Font size.
# @param ~speed Horizontal speed, in pixels per second (`0`: no scrolling).
# @param ~x x offset.
# @param ~y y offset.
# @param ~graph Filter graph.
# @param d Text to display.
# @param v Video pad.
def video.add_text.ffmpeg.raw.filter(
  ~color=0xffffff,
  ~cycle=true,
  ~font=null,
  ~metadata=null,
  ~size=18,
  ~speed=70,
  ~x=getter(10),
  ~y=getter(10),
  ~graph,
  d=getter(""),
  v
) =
  color = "0x#{string.hex_of_int(pad=6, color)}"
  escape = ffmpeg.filter.drawtext.escape
  base_x = x
  x =
    if
      speed == 0
    then
      x
    else
      last_x = ref(getter.get(base_x))
      current = ref(getter.get(base_x))
      last_time = ref(time())

      def x() =
        start = getter.get(base_x)
        if
          start != last_x()
        then
          last_x := start
          current := start
        end

        now = time()
        current := current() - int_of_float(float(speed) * (now - last_time()))
        last_time := now
        if
          current() < 0
        then
          current := settings.frame.video.width() - start
        end

        current()
      end

      x
    end

  def make(x_expr) =
    ffmpeg.filter.drawtext.create(
      fontfile=font,
      fontsize="#{size}",
      x=x_expr("#{getter.get(x)}"),
      y="#{getter.get(y)}",
      fontcolor=color,
      text=escape(getter.get(d)),
      graph
    )
  end

  def first_x(x) =
    x
  end

  def second_x(x) =
    "min(#{x}-w,#{x}-text_w)"
  end

  first = make(first_x)
  first.set_input(v)
  filters = ref([(first, first_x)])
  output = ref(first.output)
  if
    cycle
  then
    second = make(second_x)
    second.set_input(first.output)
    filters := [(first, first_x), (second, second_x)]
    output := second.output
  end

  def send(command) =
    list.iter(
      fun (f) ->
        begin
          let (f, x_expr) = f
          ignore(f.process_command("reinit", command(x_expr)))
        end,
      filters()
    )
  end

  last = ref(null)

  def on_change() =
    state = (getter.get(x), getter.get(y), getter.get(d))
    if
      last() != state
    then
      last := state
      let (x, y, text) = state
      send(fun (x_expr) -> "x=#{x_expr("#{x}")}:y=#{y}:text=#{escape(text)}")
    end
  end

  def on_metadata(m) =
    if
      null.defined(metadata)
    then
      key = (null.get(metadata) : string)
      text = escape(m[key])
      if
        text != ""
      then
        log.important(
          label="ffmpeg.filter.drawtext",
          "Setting new text #{text} from metadata #{key}"
        )
        send(fun (_) -> "text=#{text}")
      end
    end
  end

  output().{on_change = on_change, on_metadata = on_metadata}
end

# Draw a text on the video of a source carrying raw FFmpeg video, in a filter
# graph. The text and its position are updated on each frame of the source.
# @category Source / FFmpeg filter
# @flag extra
# @param ~color Text color (in 0xRRGGBB format).
# @param ~cycle Draw a second copy of the text so that scrolling wraps around.
# @param ~font Path to the font file.
# @param ~metadata Metadata to read the text from.
# @param ~size Font size.
# @param ~speed Horizontal speed, in pixels per second (`0`: no scrolling).
# @param ~x x offset.
# @param ~y y offset.
# @param ~graph Filter graph.
# @param d Text to display.
# @param s Source whose video track carries the text.
def replaces video.add_text.ffmpeg.raw(
  ~color=0xffffff,
  ~cycle=true,
  ~font=null,
  ~metadata=null,
  ~size=18,
  ~speed=70,
  ~x=getter(10),
  ~y=getter(10),
  ~graph,
  d,
  s
) =
  on_change = ref(fun () -> ())
  on_metadata = ref(fun (_) -> ())
  def frame_change() =
    f = on_change()
    f()
  end

  def metadata_change(m) =
    f = on_metadata()
    f(m)
  end

  s.on_frame(synchronous=true, frame_change)
  s.on_metadata(synchronous=true, metadata_change)
  let {video = video_track} = source.tracks(s)
  v = ffmpeg.filter.video.input(graph, video_track)
  v =
    video.add_text.ffmpeg.raw.filter(
      color=color,
      cycle=cycle,
      font=font,
      metadata=metadata,
      size=size,
      speed=speed,
      x=x,
      y=y,
      graph=graph,
      d,
      v
    )

  on_change := v.on_change
  on_metadata := v.on_metadata
  ffmpeg.filter.null(graph, v)
end

# Display a text over the video of a source, with FFmpeg's `drawtext` filter.
# The source is converted to raw FFmpeg content for the filter graph and back.
# @category Source / Video processing
# @param ~id Force the value of the source ID.
# @param ~color Text color (in 0xRRGGBB format).
# @param ~cycle Draw a second copy of the text so that scrolling wraps around.
# @param ~font Path to the font file.
# @param ~metadata Metadata to read the text from.
# @param ~size Font size.
# @param ~speed Horizontal speed, in pixels per second (`0`: no scrolling).
# @param ~x x offset.
# @param ~y y offset.
# @param ~duration Duration in seconds (`null` means infinite).
# @param d Text to display.
# @param s Source to display the text on.
def replaces video.add_text.ffmpeg(
  ~id=null("video.add_text.ffmpeg"),
  ~color=0xffffff,
  ~cycle=true,
  ~font=null,
  ~metadata=null,
  ~size=18,
  ~speed=70,
  ~x=getter(10),
  ~y=getter(10),
  ~duration=null,
  d,
  s
) =
  s = ffmpeg.raw.encode.audio_video(%ffmpeg(%audio.raw, %video.raw), s)

  def mkfilter(graph) =
    v =
      video.add_text.ffmpeg.raw(
        color=color,
        cycle=cycle,
        font=font,
        metadata=metadata,
        size=size,
        speed=speed,
        x=x,
        y=y,
        graph=graph,
        d,
        s
      )

    let {audio} = source.tracks(s)
    a = ffmpeg.filter.audio.input(graph, audio)
    a = ffmpeg.filter.acopy(graph, a)
    a = ffmpeg.filter.audio.output(graph, a)
    v = ffmpeg.filter.video.output(graph, v)
    let {metadata = m, track_marks = t} = source.tracks(source({audio = a}))
    source(id=id, {audio = a, video = v, metadata = m, track_marks = t})
  end

  s = ffmpeg.filter.create(mkfilter)
  s = ffmpeg.raw.decode.audio_video(s)
  null.defined(duration) ? max_duration(null.get(duration), s) : s
end
%endif
