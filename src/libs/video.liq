# Width for all video frames.
# @category Source / Video processing
def video.frame.width =
  settings.frame.video.width
end

# Height for all video frames.
# @category Source / Video processing
def video.frame.height =
  settings.frame.video.height
end

# Framerate for all video frames.
# @category Source / Video processing
def video.frame.rate =
  settings.frame.video.framerate
end

# Generate a source from an image request.
# @category Source / Video processing
# @param ~id Force the value of the source ID.
# @param ~fallible Whether we are allowed to fail (in case the file is non-existent or invalid).
# @param ~width Scale to width
# @param ~height Scale to height
# @param ~x x position.
# @param ~y y position.
# @param req Image request
def request.image(
  ~id=null,
  ~fallible=false,
  ~width=null,
  ~height=null,
  ~x=getter(0),
  ~y=getter(0),
  req
) =
  last_req = ref(null)

  def next() =
    req = (getter.get(req) : request)

    if
      req != last_req()
    then
      last_req := req
      image = request.single(id=id, fallible=fallible, req)
      image = video.crop(image)
      video.resize(id=id, x=x, y=y, width=width, height=height, image)
    else
      null
    end
  end

  source.dynamic(id=id, track_sensitive=false, next)
end

# Generate a source from an image file.
# @category Source / Video processing
# @param ~id Force the value of the source ID.
# @param ~fallible Whether we are allowed to fail (in case the file is non-existent or invalid).
# @param ~width Scale to width
# @param ~height Scale to height
# @param ~x x position.
# @param ~y y position.
# @param file Path to the image.
# @method set Change the request.
def image(%argsof(request.image), file) =
  r = getter.map.memoize(fun (file) -> request.create(file), file)

  request.image(%argsof(request.image), r)
end

# @flag hidden
let orig_request = request

# Add a static request on the given video track.
# @category Track / Video processing
# @param ~id Force the value of the source ID.
# @param ~fallible Whether we are allowed to fail (in case the file is non-existent or invalid).
# @param ~width Scale to width
# @param ~height Scale to height
# @param ~x x position.
# @param ~y y position.
# @param ~request Request to add to the video track
def track.video.add_request(
  ~id=null("track.video.add_request"),
  ~fallible=false,
  ~width=null,
  ~height=null,
  ~x=getter(0),
  ~y=getter(0),
  ~request,
  v
) =
  image =
    orig_request.image(
      id=id,
      fallible=fallible,
      x=x,
      y=y,
      width=width,
      height=height,
      request
    )

  let {video = image} = source.tracks(image)
  track.video.add([v, image])
end

# Add a static image on the given video track.
# @category Track / Video processing
# @param ~id Force the value of the source ID.
# @param ~fallible Whether we are allowed to fail (in case the file is non-existent or invalid).
# @param ~width Scale to width
# @param ~height Scale to height
# @param ~x x position.
# @param ~y y position.
# @param ~file Path to the image file.
def track.video.add_image(
  ~id=null("track.video.add_image"),
  ~fallible=false,
  ~width=null,
  ~height=null,
  ~x=getter(0),
  ~y=getter(0),
  ~file,
  v
) =
  image =
    image(id=id, fallible=fallible, x=x, y=y, width=width, height=height, file)

  let {video = image} = source.tracks(image)
  track.video.add([v, image])
end

# Add a static request on the source video channel.
# @category Source / Video processing
# @param ~id Force the value of the source ID.
# @param ~fallible Whether we are allowed to fail (in case the file is non-existent or invalid).
# @param ~width Scale to width
# @param ~height Scale to height
# @param ~x x position.
# @param ~y y position.
# @param ~request Request to add to the video channel
def video.add_request(
  ~id=null("video.add_request"),
  ~fallible=false,
  ~width=null,
  ~height=null,
  ~x=getter(0),
  ~y=getter(0),
  ~request,
  (s:source)
) =
  let {video, ...tracks} = source.tracks(s)
  video =
    track.video.add_request(
      fallible=fallible,
      width=width,
      height=height,
      x=x,
      y=y,
      request=request,
      video
    )

  source(id=id, tracks.{video=video})
end

# Add a static image on the source video channel.
# @category Source / Video processing
# @param ~id Force the value of the source ID.
# @param ~fallible Whether we are allowed to fail (in case the file is non-existent or invalid).
# @param ~width Scale to width
# @param ~height Scale to height
# @param ~x x position.
# @param ~y y position.
# @param ~file Path to the image file.
def video.add_image(
  ~id=null("video.add_image"),
  ~fallible=false,
  ~width=null,
  ~height=null,
  ~x=getter(0),
  ~y=getter(0),
  ~file,
  (s:source)
) =
  let {video, ...tracks} = source.tracks(s)
  video =
    track.video.add_image(
      fallible=fallible,
      width=width,
      height=height,
      x=x,
      y=y,
      file=file,
      video
    )

  source(id=id, tracks.{video=video})
end

# Generate a video source containing cover-art for current track of input audio
# source.
# @category Source / Video processing
# @param s Audio source whose metadata contain cover-art.
def video.cover(s) =
  last_filename = ref(null)
  last_metadata = source.methods(s).last_metadata
  b = (blank() : source)

  def next() =
    m = last_metadata() ?? []

    filename = m["filename"]

    if
      filename != last_filename()
    then
      last_filename := filename

      cover =
        if file.exists(filename) then file.cover(filename) else "".{mime=""} end

      if
        null.defined(cover) and null.get(cover) != ""
      then
        cover = null.get(cover)
        extname =
          (
            null.defined(cover.mime)
            ? file.mime.extension(null.get(cover.mime)) : null
          )
            ??
              ".osb"
        f = file.temp("cover", extname)
        log.debug(
          "Found cover for #{filename}."
        )
        file.write(data=cover, f)
        request.once(request.create(temporary=false, f))
      else
        log.debug(
          "No cover for #{filename}."
        )
        b
      end
    else
      null
    end
  end

  source.dynamic(track_sensitive=false, next)
end

let output.youtube = ()
let output.youtube.live = ()

# Stream to youtube using RTMP.
# @category Source / Output
# @param ~id Force the value of the source ID.
# @param ~fallible Allow the child source to fail, in which case the output will be (temporarily) stopped.
# @param ~start Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
# @param ~url RTMP URL to stream to
# @param ~encoder Encoder to use (most likely a `%ffmpeg` encoder)
# @param ~key Your secret youtube key
def output.youtube.live.rtmp(
  ~id=null,
  ~fallible=false,
  ~start=true,
  ~url="rtmp://a.rtmp.youtube.com/live2",
  ~(key:string),
  ~encoder,
  s
) =
  output.url(
    id=id,
    fallible=fallible,
    start=start,
    url="#{url}/#{key}",
    encoder,
    s
  )
end

# Stream to youtube using HLS.
# @category Source / Output
# @param ~id Force the value of the source ID.
# @param ~fallible Allow the child source to fail, in which case the output will be (temporarily) stopped.
# @param ~start Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
# @param ~segment_duration Segment duration (in seconds).
# @param ~segments Number of segments per playlist.
# @param ~segments_overhead Number of segments to keep after they have been featured in the live playlist.
# @param ~url HLS URL to stream to
# @param ~encoder Encoder to use (most likely a `%ffmpeg` encoder)
# @param ~key Your secret youtube key
def output.youtube.live.hls(
  ~id=null,
  ~fallible=false,
  ~segment_duration=2.0,
  ~segments=4,
  ~segments_overhead=4,
  ~start=true,
  ~url="https://a.upload.youtube.com/http_upload_hls",
  ~(key:string),
  ~encoder,
  s
) =
  id = string.id.default(default="output.youtube.live.hls", id)

  def file_url(fname) =
    "#{url}?cid=#{key}&copy=0&file=#{fname}"
  end

  def on_file_change({state, path = fname}) =
    if
      (state == "created" or state == "updated")
    and
      path.basename(fname) != "main.m3u8"
    then
      try
        ignore(http.post(data=file.read(fname), file_url(path.basename(fname))))
      catch err do
        log(
          label=id,
          level=3,
          "Error while uploading: #{err}"
        )
      end
    end
  end

  tmpdir = file.temp_dir("hls", "")
  on_shutdown({file.rmdir(tmpdir)})
  o =
    output.file.hls(
      id=id,
      start=start,
      fallible=fallible,
      playlist="main.m3u8",
      segment_duration=segment_duration,
      segments=segments,
      segments_overhead=segments_overhead,
      tmpdir,
      [("live", encoder)],
      s
    )
  o.on_file_change(synchronous=false, on_file_change)
  o
end

# @flag hidden
def add_text_builder(f) =
  def at(
    ~id=null,
    ~duration=null,
    ~color=getter(0xffffff),
    ~cycle=true,
    ~font=null,
    ~metadata=null,
    ~size=getter(18),
    ~speed=0,
    ~x=getter(10),
    ~y=getter(10),
    ~on_cycle={()},
    text,
    s
  ) =
    available = s.is_ready

    # Handle modifying the text with metadata.
    tref = ref(getter.get(text))
    text = null.defined(metadata) ? tref : text

    def on_metadata(m) =
      if
        null.defined(metadata)
      then
        m = m[null.get(metadata)]
        if m != "" then tref := m end
      end
    end

    if
      null.defined(metadata)
    then
      s.on_metadata(synchronous=true, on_metadata)
    end

    # Our text source.
    t = f(id=id, duration=duration, color=color, font=font, size=size, text)
    t = video.info(video.crop(t))

    # Handle scrolling if necessary.
    x =
      if
        speed == 0
      then
        x
      else
        fps = video.frame.rate()
        x = ref(getter.get(x))

        def x() =
          if
            cycle and x() < 0 - t.width()
          then
            on_cycle()
            x := video.frame.width()
          end

          x := x() - getter.get(speed) / fps
          x()
        end

        x
      end

    t = video.translate(x=x, y=y, t)

    # Ensure that we fail when s fails.
    t = source.available(t, available)

    # Add the text to the original source.
    let {video = v, ...tracks} = source.tracks(s)
    let {video = t} = source.tracks(t)
    let v = track.video.add([v, t])
    source(tracks.{video=v})
  end

  at
end

let video.add_text = ()
let video.text.available = ref([])

# Add a text to a stream (native implementation).
# @category Source / Video processing
# @param ~id Force the value of the source ID.
# @param ~color Text color (in 0xRRGGBB format).
# @param ~cycle Cycle text when it reaches left boundary.
# @param ~font Path to ttf font file.
# @param ~metadata Change text on a particular metadata (empty string means disabled).
# @param ~size Font size.
# @param ~speed Horizontal speed in pixels per second (`0` means no scrolling and update according to `x` and `y` in case they are variable).
# @param ~x x offset.
# @param ~y y offset.
# @params d Text to display.
def video.add_text.native =
  add_text_builder(video.text.native)
end

video.text.available :=
  [("native", video.text.native), ...video.text.available()]

%ifdef video.text.gd
video.text.available := [("gd", video.text.gd), ...video.text.available()]

# Add a text to a stream (GD implementation).
# @category Source / Video processing
# @param ~id Force the value of the source ID.
# @param ~color Text color (in 0xRRGGBB format).
# @param ~cycle Cycle text when it reaches left boundary.
# @param ~font Path to ttf font file.
# @param ~metadata Change text on a particular metadata (empty string means disabled).
# @param ~size Font size.
# @param ~speed Horizontal speed in pixels per second (`0` means no scrolling and update according to `x` and `y` in case they are variable).
# @param ~x x offset.
# @param ~y y offset.
# @params d Text to display.
def video.add_text.gd =
  add_text_builder(video.text.gd)
end
%endif

%ifdef video.text.sdl
video.text.available := [("sdl", video.text.sdl), ...video.text.available()]

# Add a text to a stream (SDL implementation).
# @category Source / Video processing
# @param ~id Force the value of the source ID.
# @param ~color Text color (in 0xRRGGBB format).
# @param ~cycle Cycle text when it reaches left boundary.
# @param ~font Path to ttf font file.
# @param ~metadata Change text on a particular metadata (empty string means disabled).
# @param ~size Font size.
# @param ~speed Horizontal speed in pixels per second (`0` means no scrolling and update according to `x` and `y` in case they are variable).
# @param ~x x offset.
# @param ~y y offset.
# @params d Text to display.
def video.add_text.sdl =
  add_text_builder(video.text.sdl)
end
%endif

%ifdef video.text.camlimages
video.text.available :=
  [("camlimages", video.text.camlimages), ...video.text.available()]

# Add a text to a stream (camlimages implementation).
# @category Source / Video processing
# @param ~id Force the value of the source ID.
# @param ~color Text color (in 0xRRGGBB format).
# @param ~cycle Cycle text when it reaches left boundary.
# @param ~font Path to ttf font file.
# @param ~metadata Change text on a particular metadata (empty string means disabled).
# @param ~size Font size.
# @param ~speed Horizontal speed in pixels per second (`0` means no scrolling and update according to `x` and `y` in case they are variable).
# @param ~x x offset.
# @param ~y y offset.
# @params d Text to display.
def video.add_text.camlimages =
  add_text_builder(video.text.camlimages)
end
%endif

let settings.video.text =
  settings.make(
    description="`video.text` implementation.",
    fst(list.hd(video.text.available()))
  )

thread.run(
  (
    fun () ->
      begin
        text = settings.video.text()
        if
          list.assoc.mem(text, video.text.available())
        then
          log.important(
            label="video.text",
            "Using #{text} implementation"
          )
        else
          log.severe(
            label="video.text",
            "Cannot find #{text} implementation for `video.text`, using default #{
              fst(list.hd(video.text.available()))
            }"
          )
        end
      end
  )
)

# Display a text using the first available operator in: camlimages, SDL, FFmpeg, gd or native.
# @category Source / Video processing
# @param ~id Force the value of the source ID.
# @param ~color Text color (in 0xRRGGBB format).
# @param ~duration Duration in seconds (`null` means infinite).
# @param ~font Path to ttf font file.
# @param ~size Font size.
# @param text Text to display.
def replaces video.text(
  ~id=null,
  ~color=getter(0xffffff),
  ~duration=null,
  ~font=null,
  ~size=getter(18),
  text
) =
  f =
    list.assoc(
      default=snd(list.hd(video.text.available())),
      settings.video.text(),
      video.text.available()
    )

  f(id=id, color=color, duration=duration, font=font, size=size, text)
end

# Add a text to a stream. Uses the first available operator in: camlimages, SDL,
# FFmpeg, gd or native.
# @category Source / Video processing
# @param ~id Force the value of the source ID.
# @param ~color Text color (in 0xRRGGBB format).
# @param ~cycle Cycle text when it reaches left boundary.
# @param ~font Path to ttf font file.
# @param ~metadata Change text on a particular metadata (empty string means disabled).
# @param ~size Font size.
# @param ~speed Horizontal speed in pixels per second (`0` means no scrolling and update according to `x` and `y` in case they are variable).
# @param ~x x offset.
# @param ~y y offset.
# @param ~on_cycle Function called when text is cycling.
# @params d Text to display.
def replaces video.add_text(
  ~id=null,
  ~duration=null,
  ~color=0xffffff,
  ~cycle=true,
  ~font=null,
  ~metadata=null,
  ~size=18,
  ~speed=0,
  ~x=getter(10),
  ~y=getter(10),
  ~on_cycle={()},
  d,
  s
) =
  add_text = add_text_builder(video.text)
  add_text(
    id=id,
    duration=duration,
    cycle=cycle,
    font=font,
    metadata=metadata,
    size=size,
    color=color,
    speed=speed,
    x=x,
    y=y,
    on_cycle=on_cycle,
    d,
    s
  )
end

# Add subtitle from metadata.
# @category Source / Video processing
# @param ~override Metadata where subtitle to display are located.
# @param ~offset Offset in pixels.
# @param s Source.
def video.add_subtitle(
  ~override="subtitle",
  ~size=18,
  ~color=0xffffff,
  ~offset=20,
  s
) =
  subtitle = ref("")
  t = video.text(size=size, color=color, subtitle)
  t = video.bounding_box(t)
  x = {(video.frame.width() - t.width()) / 2}
  y = {video.frame.height() - t.height() - offset}
  t = video.translate(x=x, y=y, t)

  def meta(m) =
    if list.assoc.mem(override, m) then subtitle := list.assoc(override, m) end
  end

  s.on_metadata(synchronous=true, meta)
  let {video = v, ...tracks} = source.tracks(s)
  let {video = t} = source.tracks(t)
  let v = track.video.add([v, t])
  source(tracks.{video=v})
end

# Display a slideshow (typically of pictures).
# @category Source / Video processing
# @param ~cyclic Go to the first picture after the last.
# @param ~advance Skip to the next file after this amount of time in seconds (negative means never).
# @param l List of files to display.
# @method append Append a list of files to the slideshow.
# @method clear Clear the list of files in the slideshow.
# @method next Go to next file.
# @method prev Go to previous file.
# @method current Currently displayed file.
def video.slideshow(
  ~id=null,
  ~cyclic=getter(true),
  ~advance=getter(-1.),
  l=[]
) =
  id = string.id.default(default="video.slideshow", id)
  l = ref(l)
  n = ref(-1)

  next_source = ref(null)

  def next() =
    s = next_source()
    next_source := null
    s
  end

  s = source.dynamic(next)

  def current() =
    list.nth(l(), n())
  end

  # Set current file to the nth.
  def set(n') =
    if
      0 <= n' and n' < list.length(l()) and n' != n()
    then
      n := n'
      new_source = request.once(request.create(current()))
      new_source = s.prepare(new_source)
      next_source := new_source
    end
  end

  def next() =
    log.debug(
      label=id,
      "Going to next file"
    )
    n' = n() + 1
    n' =
      if
        n' >= list.length(l())
      then
        if getter.get(cyclic) then 0 else list.length(l()) - 1 end
      else
        n'
      end

    set(n')
  end

  def prev() =
    log.debug(
      label=id,
      "Going to previous file"
    )
    n' = n() - 1
    n' =
      if
        n' < 0
      then
        if getter.get(cyclic) then list.length(l()) - 1 else 0 end
      else
        n'
      end

    set(n')
  end

  def clear() =
    l := []
    n := 0
  end

  def append(l') =
    l := list.append(l(), l')
  end

  set(0)
  if
    getter.get(advance) >= 0.
  then
    thread.run(delay=getter.get(advance), every=advance, next)
  end

  s.{append=append, clear=clear, next=next, prev=prev, current=current}
end

# Generate a video filled with given color.
# @category Source / Video processing
# @param color Color (in 0xRRGGBB format).
def video.color(color) =
  video.fill(color=color, blank())
end

# Tile sources
# @category Source / Video processing
# @argsof track.video.tile
# @argsof track.audio.add[!id]
def video.tile(
  ~id=null("video.tile"),
  %argsof(track.audio.add[!id]),
  %argsof(track.video.tile[!id]),
  ~weights=[],
  sources
) =
  tracks = list.map(fun (s) -> source.tracks(s), sources)
  video_tracks = list.map(fun (t) -> t.video, tracks)
  new_tracks =
    {video=track.video.tile(%argsof(track.video.tile[!id]), video_tracks)}

  new_tracks =
    if
      list.length(tracks) != 0 and null.defined(list.hd(tracks)?.audio)
    then
      def mk_audio_track(pos, track) =
        weight =
          try
            list.nth(weights, pos)
          catch _ do
            getter(1.)
          end

        null.get(track?.audio).{weight=weight}
      end

      audio_tracks = list.mapi(mk_audio_track, tracks)
      new_tracks.{
        audio=track.audio.add(%argsof(track.audio.add[!id]), audio_tracks)
      }
    else
      new_tracks
    end

  source(id=id, new_tracks)
end

# Plot a floating point value.
# @category Source / Video processing
# @param ~color Color of the drawn point (in 0xRRGGBB format).
# @param ~lines Draw lines connecting plotted points.
# @param ~min Minimal value of the parameter.
# @param ~max Maximal value of the parameter.
# @param ~speed Speed in pixels per second.
# @param y Value to plot.
def video.plot(~lines=true, ~min=0., ~max=1., ~speed=100., ~color=0xffffff, y) =
  width = video.frame.width()
  height = video.frame.height()
  s = video.board(width=width * 3, height=height)
  height = float(s.height())
  tx = ref(width)
  ty = ref(0)
  s' = video.translate(x=tx, y=ty, s)

  # offset of the currently drawn point.
  x = ref(0)
  dx = int_of_float(speed * frame.duration())

  def update() =
    tx := tx() - dx
    y = int_of_float(((max - y()) / (max - min)) * height)
    if
      lines
    then
      s.line_to(color=color, x(), y)
    else
      s.pixel(x(), y) := color
    end

    x := x() + dx
    if
      x() > width * 2
    then
      s.clear_and_copy(x=0 - width)
      tx := tx() + width
      x := x() - width
    end
  end

  s'.on_frame(synchronous=true, update)
  s'
end

let video.canvas = ()

# Create a virtual canvas that can be used to return video position and sizes
# that are independent of the frame's dimensions.
# @category Source / Video processing
# @param ~virtual_width Virtual height, in pixels, of the canvas
# @param ~actual_size Actual size, in pixels, of the canvas
# @param ~font_size Font size, in virtual pixels.
# @method ~px Map a virtual size in pixel to the actual size.
# @method ~rem Map a fraction of the virtual font size into an actual font size
# @method ~vh Return a position in percent (as a value between `0.` and `1.`) \
#             of the canvas height
# @method ~vw Return a position in percent (as a value between `0.` and `1.`) \
#             of the canvas width
def video.canvas.make(~virtual_width, ~actual_size, ~font_size) =
  virtual_width = float(virtual_width)
  actual_height = float(actual_size.height)
  actual_width = float(actual_size.width)
  ratio = actual_width / virtual_width
  font_ratio = float(font_size) * ratio

  def px((v:int)) =
    int_of_float(float(v) * ratio)
  end

  def vh(v) =
    int_of_float(v * actual_height)
  end

  def vw(v) =
    int_of_float(v * (float(actual_width)))
  end

  def rem(v) =
    int_of_float(v * font_ratio)
  end

  {px=px, rem=rem, vw=vw, vh=vh, ...actual_size}
end

# Standard video canvas based off a `10k` virtual canvas.
# @category Source / Video processing
def video.canvas.virtual_10k =
  def make(width, height) =
    video.canvas.make(
      virtual_width=10000,
      actual_size={width=width, height=height},
      font_size=160
    )
  end

  {
    actual_360p=make(640, 360),
    actual_480p=make(640, 480),
    actual_720p=make(1280, 720),
    actual_1080p=make(1920, 1080),
    actual_1440p=make(2560, 1440),
    actual_4k=make(3840, 2160),
    actual_8k=make(7680, 4320)
  }
end
