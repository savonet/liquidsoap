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

# Generate a source from a static image.
# @category Source / Video processing
# @param ~id Force the value of the source ID.
# @param ~fallible Whether we are allowed to fail (in case the file is non-existent or invalid).
# @param file Path to the image.
# @method set Change the image.
def image(~id=null(), ~fallible=false, file="")
  s = source.dynamic()
  def set(file)
    s.set(single(id=id, fallible=fallible, file))
  end
  set(file)
  s.{set=set}
end

# Add a static image on the first video channel.
# @category Source / Video processing
# @param ~id Force the value of the source ID.
# @param ~width Scale to width
# @param ~height Scale to height
# @param ~x x position.
# @param ~y y position.
# @param ~file Path to the image file.
def video.add_image(~id=null(), ~width=null(), ~height=null(), ~x=getter(0), ~y=getter(0), ~file, s)
  width = width ?? video.frame.width()
  height = height ?? video.frame.height()
  image = single(id=id, file)
  image = video.resize(id=id, x=x, y=y, width=width, height=height, image)
  add([(s:source),image])
end

# Generate a video source containing cover-art for current track of input audio
# source.
# @category Source / Video processing
# @param s Audio source whose metadata contain cover-art.
def video.cover(s)
  video = source.dynamic()
  def read_cover(m)
    filename = m["filename"]
    cover = if file.exists(filename) then file.cover(filename) else "".{mime=""} end
    if null.defined(cover) then
      cover = null.get(cover)
      ext =
        if cover.mime == "image/png" then
          ".png"
        else
          ".jpg"
        end
      f = file.temp("cover", ext)
      log.debug("Found cover for #{filename}.")
      file.write(data=cover, f)
      video.set(request.once(request.create(temporary=true, f)))
    else
      log.debug("No cover for #{filename}.")
      video.set(source.fail())
    end
  end
  s.on_track(read_cover)
  (video:source(video=canvas))
end

# Input from ffmpeg.
# @category Source / Video processing
# @flag extra
# @param ~restart restart on exit.
# @param ~restart_on_error restart on exit with error.
# @param ~buffer Duration of the pre-buffered data.
# @param ~max Maximum duration of the buffered data.
# @param inputopts ffmpeg options specifying the input
def input.external.ffmpeg(~id="input.external.ffmpeg", ~show_command=false,
                          ~restart=true,~restart_on_error=false,
                          ~buffer=0.2,~max=10.,inputopts)
  inputopts = (inputopts : string)
  ffmpeg = null.get(settings.decoder.external.ffmpeg.path())
  cmd = "#{ffmpeg} #{inputopts} -f avi \
         -vf \"scale=#{video.frame.width()}:#{video.frame.height()}:\
               force_original_aspect_ratio=decrease,pad=#{video.frame.width()}:\
               #{video.frame.height()}:(ow-iw)/2:(oh-ih)/2\",\
               format=yuv420p,fps=fps=#{video.frame.rate()} \
         -c:v rawvideo -r #{video.frame.rate()} -c:a pcm_s16le -ac 2 \
         -ar #{audio.samplerate()} pipe:1"

  if show_command then log.important(label=id, "command: #{cmd}") end

  (input.external.avi(id=id,restart=restart,
                      restart_on_error=restart_on_error,
                      buffer=buffer,max=max,cmd):source(audio=pcm(stereo),video=canvas))
end

# ffmpeg's test source video (useful for testing and debugging).
# @param ~restart restart on exit.
# @param ~restart_on_error restart on exit with error.
# @param ~buffer Duration of the pre-buffered data.
# @param ~max Maximum duration of the buffered data.
# @category Source / Video processing
# @flag extra
def video.external.testsrc(~id="video.external.testsrc",~restart=true,~restart_on_error=false,~buffer=0.2,~max=10.,~framerate=0)
  framerate = if framerate <= 0 then video.frame.rate() else framerate end
  cmd = "-f lavfi -i testsrc=size=#{video.frame.width()}x#{video.frame.height()}:rate=#{framerate} \
         -f lavfi -i sine=frequency=440:beep_factor=2:sample_rate=#{audio.samplerate()}"

  input.external.ffmpeg(id=id,restart=restart,
                        restart_on_error=restart_on_error,
                        buffer=buffer,max=max,show_command=true,cmd)
end

# Output to ffmpeg.
# @category Source / Output
# @flag extra
# @param ~id Force the value of the source ID.
# @param ~fallible Allow the child source to fail, in which case the output will be (temporarily) stopped.
# @param ~flush Perform a flush after each write.
# @param ~on_start Callback executed when outputting starts.
# @param ~on_stop Callback executed when outputting stops.
# @param ~reopen_delay Prevent re-opening within that delay, in seconds.
# @param ~reopen_on_metadata Re-open on every new metadata information.
# @param ~reopen_when When should the output be re-opened.
# @param ~start Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
def output.external.ffmpeg(~id=null(), ~show_command=false, ~flush=false, ~fallible=false,
                           ~on_start={()}, ~on_stop={()}, ~reopen_delay=120.,
                           ~reopen_on_metadata=false, ~reopen_when={false},
                           ~start=true, outputopts, s)
  outputopts = (outputopts : string)
  cmd = "ffmpeg -f avi -vcodec rawvideo -r #{video.frame.rate()} -acodec pcm_s16le -i pipe:0 #{outputopts}"
  if show_command then log.important(label="output.external.ffmpeg", "command: #{cmd}") end
  output.external(id=id, flush=flush, fallible=fallible, on_start=on_start, on_stop=on_stop, reopen_delay=reopen_delay,
                  reopen_on_metadata=reopen_on_metadata, reopen_when=reopen_when, start=start, %avi, cmd, s)
end

# Output a HLS playlist using ffmpeg
# @category Source / Output
# @flag extra
# @param ~id Force the value of the source ID.
# @param ~fallible Allow the child source to fail, in which case the output will be (temporarily) stopped.
# @param ~flush Perform a flush after each write.
# @param ~on_start Callback executed when outputting starts.
# @param ~on_stop Callback executed when outputting stops.
# @param ~reopen_delay Prevent re-opening within that delay, in seconds.
# @param ~reopen_on_metadata Re-open on every new metadata information.
# @param ~reopen_when When should the output be re-opened.
# @param ~start Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
# @param ~playlist Playlist name
# @param ~directory Directory to write to
def output.file.hls.ffmpeg(~id=null(), ~flush=false, ~fallible=false,
                           ~on_start={()}, ~on_stop={()}, ~reopen_delay=120.,
                           ~reopen_on_metadata=false, ~reopen_when={false},
                           ~start=true, ~playlist="stream.m3u8", ~directory, s)
  width = video.frame.width()
  height = video.frame.height()
  directory = (directory : string)
  cmd = "-profile:v baseline -pix_fmt yuv420p -level 3.0 -s #{width}x#{height} -start_number 0 -hls_time 10 -hls_list_size 0 -f hls #{directory}/#{playlist}"
  output.external.ffmpeg(id=id, flush=flush, fallible=fallible, on_start=on_start, on_stop=on_stop, reopen_delay=reopen_delay,
                         reopen_on_metadata=reopen_on_metadata, reopen_when=reopen_when, start=start, cmd, s)
end

let output.file.dash = ()

# Output an MPEG-DASH playlist using ffmpeg
# @category Source / Output
# @param ~id Force the value of the source ID.
# @param ~fallible Allow the child source to fail, in which case the output will be (temporarily) stopped.
# @param ~flush Perform a flush after each write.
# @param ~on_start Callback executed when outputting starts.
# @param ~on_stop Callback executed when outputting stops.
# @param ~reopen_delay Prevent re-opening within that delay, in seconds.
# @param ~reopen_on_metadata Re-open on every new metadata information.
# @param ~reopen_when When should the output be re-opened.
# @param ~start Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
# @param ~playlist Playlist name
# @param ~directory Directory to write to
def output.file.dash.ffmpeg(~id=null(), ~flush=false, ~fallible=false,
                           ~on_start={()}, ~on_stop={()}, ~reopen_delay=120.,
                           ~reopen_on_metadata=false, ~reopen_when={false},
                           ~start=true, ~playlist="stream.mpd", ~directory, s)
  width = video.frame.width()
  height = video.frame.height()
  samplerate = audio.samplerate()
  cmd = "-map 0 -map 0 -c:a libfdk_aac -c:v libx264 -b:v:0 800k -b:v:1 300k -s:v:1 #{width}x#{height} -profile:v:1 baseline -profile:v:0 main -bf 1 -keyint_min 120 -g 120 -sc_threshold 0 -b_strategy 0 -ar:a:1 #{samplerate} -use_timeline 1 -use_template 1 -window_size 5 -adaptation_sets \"id=0,streams=v id=1,streams=a\" -f dash #{(directory:string)}/#{playlist}"
  output.external.ffmpeg(id=id, flush=flush, fallible=fallible, on_start=on_start, on_stop=on_stop, reopen_delay=reopen_delay, reopen_on_metadata=reopen_on_metadata, reopen_when=reopen_when, start=start, show_command=true, cmd, s)
end

let output.youtube = ()
let output.youtube.live = ()

# Stream to youtube using RTMP.
# @category Source / Output
# @param ~id Force the value of the source ID.
# @param ~fallible Allow the child source to fail, in which case the output will be (temporarily) stopped.
# @param ~on_start Callback executed when outputting starts.
# @param ~on_stop Callback executed when outputting stops.
# @param ~start Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
# @param ~url RTMP URL to stream to
# @param ~encoder Encoder to use (most likely a `%ffmpeg` encoder)
# @param ~key Your secret youtube key
def output.youtube.live.rtmp(~id=null(), ~fallible=false, ~on_start={()}, ~on_stop={()},
                             ~start=true, ~url="rtmp://a.rtmp.youtube.com/live2",
                             ~(key:string),~encoder,s)
  output.url(id=id, fallible=fallible, on_start=on_start, on_stop=on_stop, start=start, url="#{url}/#{key}", encoder, s)
end

# Stream to youtube using HLS.
# @category Source / Output
# @param ~id Force the value of the source ID.
# @param ~fallible Allow the child source to fail, in which case the output will be (temporarily) stopped.
# @param ~on_start Callback executed when outputting starts.
# @param ~on_stop Callback executed when outputting stops.
# @param ~start Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
# @param ~segment_duration Segment duration (in seconds).
# @param ~segments Number of segments per playlist.
# @param ~segments_overhead Number of segments to keep after they have been featured in the live playlist.
# @param ~url HLS URL to stream to
# @param ~encoder Encoder to use (most likely a `%ffmpeg` encoder)
# @param ~key Your secret youtube key
def output.youtube.live.hls(~id=null(), ~fallible=false, ~on_start={()}, ~on_stop={()},
                            ~segment_duration=2.0,~segments=4,~segments_overhead=4,
                            ~start=true, ~url="https://a.upload.youtube.com/http_upload_hls",
                            ~(key:string), ~encoder, s)
  id = string.id.default(default="output.youtube.live.hls", id)

  def file_url(fname) =
    "#{url}?cid=#{key}&copy=0&file=#{fname}"
  end

  def file_upload(fname) = fun () ->
    try
      ignore(http.post(data=file.read(fname),file_url(path.basename(fname))))
    catch err do
      log(label=id, level=3, "Error while uploading: #{err}")
    end
  end

  def on_file_change(~state, fname) =
    if state == "closed" and path.basename(fname) != "main.m3u8" then
      thread.run(file_upload(fname))
    end
  end

  tmpdir = file.temp_dir("hls","")

  on_shutdown({file.rmdir(tmpdir)})

  output.file.hls(
    id=id,
    start=start,
    fallible=fallible,
    on_stop=on_stop,
    on_start=on_start,
    playlist="main.m3u8",
    segment_duration=segment_duration,
    segments=segments,
    segments_overhead=segments_overhead,
    on_file_change=on_file_change,
    tmpdir,
    [("live", encoder)],
    s
  )
end

# @flag hidden
def add_text_builder(f) =
  def at(~id=null(),~duration=null(),~color=getter(0xffffff),~cycle=true,~font=null(),~metadata=null(),~size=getter(18),~speed=0,~x=getter(10),~y=getter(10),~on_cycle={()},text,s)
    available = s.is_ready

    # Handle modifying the text with metadata.
    tref = ref(getter.get(text))
    text = null.defined(metadata) ? tref : text
    def on_metadata(m)
      if null.defined(metadata) then
        m = m[null.get(metadata)]
        if m != "" then tref := m end
      end
    end
    s = null.defined(metadata) ? source.on_metadata(s, on_metadata) : s

    # Our text source.
    t = f(id=id, duration=duration, color=color, font=font, size=size, text)
    t = video.info(video.crop(t))

    # Handle scrolling if necessary.
    x = if speed == 0 then x else
      fps = video.frame.rate()
      x = ref(getter.get(x))
      def x()
        if cycle and x() < 0 - t.width() then on_cycle(); x := video.frame.width() end
        x := x() - getter.get(speed) / fps
        x()
      end
      x
    end
    t = video.translate(x=x, y=y, t)

    # Ensure that we fail when s fails.
    t = source.available(t, available)

    # Add the text to the original source.
    add(normalize=false, [(s:source),t])
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
def video.add_text.native = add_text_builder(video.text.native) end

video.text.available := [("native", video.text.native), ...video.text.available()]

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
def video.text.gd = add_text_builder(video.text.gd) end
%endif

%ifdef video.text.gstreamer
video.text.available := [("gstreamer", video.text.gstreamer), ...video.text.available()]

# Add a text to a stream (GStreamer implementation).
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
def video.add_text.gstreamer = add_text_builder(video.text.gstreamer) end
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
def video.add_text.sdl = add_text_builder(video.text.sdl) end
%endif

%ifdef video.text.camlimages
video.text.available := [("camlimages", video.text.camlimages), ...video.text.available()]

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
def video.add_text.camlimages = add_text_builder(video.text.camlimages) end
%endif

let settings.video.text = settings.make(
  description="`video.text` implementation.",
  fst(list.hd(video.text.available()))
)

thread.run.recurrent((fun () ->
  begin
    text = settings.video.text()
    if list.assoc.mem(text, video.text.available()) then
      log.important(label="video.text", "Using #{text} implementation")
    else
      log.severe(label="video.text", "Cannot find #{text} implementation for `video.text`, using default #{fst(list.hd(video.text.available()))}")
    end
    (-1.)
  end))

# Display a text.
# @category Source / Video processing
# @param ~id Force the value of the source ID.
# @param ~color Text color (in 0xRRGGBB format).
# @param ~duration Duration in seconds (`null` means infinite).
# @param ~font Path to ttf font file.
# @param ~size Font size.
# @param text Text to display.
def replaces video.text(~id=null(), ~color=getter(0xffffff), ~duration=null(), ~font=null(), ~size=getter(18), text)
  f = list.assoc(default=snd(list.hd(video.text.available())), settings.video.text(), video.text.available())
  f(id=id, color=color, duration=duration, font=font, size=size, text)
end

# Add a text to a stream. Uses the first available operator in: camlimages, SDL,
# FFmpeg, GStreamer, gd or native.
# Display a text using the first available operator in: SDL, GStreamer, gd or native.
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
def replaces video.add_text(~id=null(),~duration=null(),~color=0xffffff,~cycle=true,~font=null(),~metadata=null(),~size=18,~speed=0,~x=getter(10),~y=getter(10),~on_cycle={()},d,s) =
  add_text = add_text_builder(video.text)
  add_text(id=id,duration=duration,cycle=cycle,font=font,metadata=metadata,size=size,color=color,speed=speed,x=x,y=y,on_cycle=on_cycle,d,s)
end

# Add subtitle from metadata.
# @category Source / Video processing
# @param ~override Metadata where subtitle to display are located.
# @param ~offset Offset in pixels.
# @param s Source.
def video.add_subtitle(~override="subtitle", ~size=18, ~color=0xffffff, ~offset=20, s)
  subtitle = ref("")
  t = video.text(size=size, color=color, subtitle)
  t = video.bounding_box(t)
  x = {(video.frame.width() - t.width())/2}
  y = {video.frame.height() - t.height() - offset}
  t = video.translate(x=x, y=y, t)
  def meta(m)
    if list.assoc.mem(override, m) then
      subtitle := list.assoc(override, m)
    end
  end
  s = source.on_metadata(s, meta)
  add([s,t])
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
def video.slideshow(~id=null(), ~cyclic=getter(true), ~advance=getter(-1.), l=[])
  id = string.id.default(default="video.slideshow", id)
  l = ref(l)
  n = ref(-1)
  s = source.dynamic()
  def current()
    list.nth(l(), n())
  end
  # Set current file to the nth.
  def set(n')
    if 0 <= n' and n' < list.length(l()) and n' != n() then
      n := n'
      s.set(request.once(request.create(current())))
    end
  end
  def next()
    log.debug(label=id, "Going to next file")
    n' = n() + 1
    n' = if n' >= list.length(l()) then if getter.get(cyclic) then 0 else list.length(l()) - 1 end else n' end
    set(n')
  end
  def prev()
    log.debug(label=id, "Going to previous file")
    n' = n() - 1
    n' = if n' < 0 then if getter.get(cyclic) then list.length(l()) - 1 else 0 end else n' end
    set(n')
  end
  def clear()
    l := []
    n := 0
  end
  def append(l')
    l := list.append(l(), l')
  end
  set(0)
  if getter.get(advance) >= 0. then thread.run(delay=getter.get(advance), every=advance, next) end
  s.{append=append, clear=clear, next=next, prev=prev, current=current}
end

# Generate a video filled with given color.
# @category Source / Video processing
# @param color Color (in 0xRRGGBB format).
def video.color(color)
  video.fill(color=color, blank())
end

# Tile sources
# @category Source / Video processing
# @argsof track.video.tile
# @argsof track.audio.add[!id]
def video.tile(~id=null("video.tile"), %argsof(track.audio.add[!id]), %argsof(track.video.tile[!id]), ~weights=[], sources) =
  tracks = list.map(fun (s) -> source.tracks(s), sources)
  video_tracks = list.map(fun (t) -> t.video, tracks)
  new_tracks = {
    video = track.video.tile(%argsof(track.video.tile[!id]), video_tracks)
  }

  new_tracks =
    if list.length(tracks) != 0 and null.defined(list.hd(tracks)?.audio) then
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
      new_tracks.{audio = track.audio.add(%argsof(track.audio.add[!id]), audio_tracks)}
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
def video.plot(~lines=true, ~min=0., ~max=1., ~speed=100., ~color=0xffffff, y)
  width = video.frame.width()
  height = video.frame.height()
  s = video.board(width=width*3, height=height)
  height = float(s.height())
  tx = ref(width)
  ty = ref(0)
  s' = video.translate(x=tx, y=ty, s)
  # offset of the currently drawn point.
  x = ref(0)
  dx = int_of_float(speed * frame.duration())
  def update()
    tx := tx() - dx
    y = int_of_float(((max - y()) / (max - min)) * height)
    if lines then s.line_to(color=color, x(), y) else s.pixel(x(), y) := color end
    x := x() + dx
    if x() > width*2 then
      s.clear_and_copy(x=0-width)
      tx := tx() + width
      x := x() - width
    end
  end
  source.on_frame(s', update)
end
