# Width for all video frames.
# @category Source / Video Processing
def video.frame.width =
  settings.frame.video.width
end

# Height for all video frames.
# @category Source / Video Processing
def video.frame.height =
  settings.frame.video.height
end

# Framerate for all video frames.
# @category Source / Video Processing
def video.frame.rate =
  settings.frame.video.framerate
end

# Generate a source from a static image.
# @category Source / Video Processing
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
  s = mux_audio(id=id, audio=blank(), s)
  s.{set=set}
end

# Add a static image on the first video channel.
# @category Source / Video Processing
# @param ~id Force the value of the source ID.
# @param ~width Scale to width
# @param ~height Scale to height
# @param ~x x position.
# @param ~y y position.
# @param ~file Path to the image file.
def video.add_image(~id=null(), ~width=getter(null()), ~height=getter(null()), ~x=getter(0), ~y=getter(0), ~file, s)
  image =
    if getter.is_constant(x) and getter.is_constant(y) and getter.is_constant(width) and getter.is_constant(height) then
      x      = getter.get(x)
      y      = getter.get(y)
      width  = getter.get(width)
      height = getter.get(height)
      # This version is more efficient and does not mess up the dimensions (with
      # the one below, images get decoded with transparent border which messes
      # up their dimensions).
      width = if null.defined(width) then "width=#{null.get(width)}" else "" end
      height = if null.defined(height) then "height=#{null.get(height)}" else "" end
      x = if x == 0 then "" else "x=#{x}" end
      y = if y == 0 then "" else "y=#{y}" end
      single(id=id,"annotate:#{width},#{height},#{x},#{y}:#{file}")
    else
      width = getter.map(fun (v) -> v ?? video.frame.width(), width)
      height = getter.map(fun (v) -> v ?? video.frame.height(), height)
      image = single(id=id, file)
      video.resize(id=id, x=x, y=y, width=width, height=height, image)
    end
  image = mux_audio(audio=blank(), image)
  add([(s:source),image])
end

# Generate a video source containing cover-art for current track of input audio
# source.
# @category Source / Video Processing
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
  (video:source(audio=none))
end

# Input from ffmpeg.
# @category Source / Video Processing
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
                      buffer=buffer,max=max,cmd):source(audio=pcm(stereo),video=yuva420p,midi=none))
end

# ffmpeg's test source video (useful for testing and debugging).
# @param ~restart restart on exit.
# @param ~restart_on_error restart on exit with error.
# @param ~buffer Duration of the pre-buffered data.
# @param ~max Maximum duration of the buffered data.
# @category Source / Video Processing
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
# @category Output
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
# @category Output
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
# @category Output
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
def output.file.dash.ffmpeg(~id=null(), ~flush=false, ~fallible=false,
                           ~on_start={()}, ~on_stop={()}, ~reopen_delay=120.,
                           ~reopen_on_metadata=false, ~reopen_when={false},
                           ~start=true, ~playlist="stream.mpd", ~directory, s)
  width = video.frame.width()
  height = video.frame.height()
  samplerate = audio.samplerate()
  cmd = "-map 0 -map 0 -c:a libfdk_aac -c:v libx264 -b:v:0 800k -b:v:1 300k -s:v:1 #{width}x#{height} -profile:v:1 baseline -profile:v:0 main -bf 1 -keyint_min 120 -g 120 -sc_threshold 0 -b_strategy 0 -ar:a:1 #{samplerate} -use_timeline 1 -use_template 1 -window_size 5 -adaptation_sets \"id=0,streams=v id=1,streams=a\" -f dash #{directory}/#{playlist}"
  output.external.ffmpeg(id=id, flush=flush, fallible=fallible, on_start=on_start, on_stop=on_stop, reopen_delay=reopen_delay, reopen_on_metadata=reopen_on_metadata, reopen_when=reopen_when, start=start, show_command=true, cmd, s)
end

let output.youtube = ()
let output.youtube.live = ()

# Stream to youtube using the `ffmpeg` CLI. `output.youtube.live.rtmp` should be preffered.
# @category Output
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
# @param ~url URL to stream to
# @param ~bitrate Bitrate of the video (in kbps)
# @param ~quality Quality of the video (low / medium / high)
# @param ~key Your secret youtube key
def output.youtube.live.ffmpeg(~id=null(),~flush=false, ~fallible=false,
                           ~on_start={()}, ~on_stop={()}, ~reopen_delay=120.,
                           ~reopen_on_metadata=false, ~reopen_when={false},
                           ~start=true, ~url="rtmp://a.rtmp.youtube.com/live2",
                           ~bitrate=2500,~quality="medium",~key,s)
  # Output parameters
  fps = video.frame.rate()
  samplerate = audio.samplerate()
  key = (key : string)
  cmd = "-deinterlace -vcodec libx264 -pix_fmt yuv420p -preset #{quality} -r #{fps} -g $((#{fps} * 2)) \
          -b:v #{bitrate}k -acodec libmp3lame -ar #{samplerate} -threads 6 -qscale:v 3 -b:a 128k \
          -bufsize 512k -f flv \"#{url}/#{key}\""
  output.external.ffmpeg(id=id, flush=flush, fallible=fallible, on_start=on_start, on_stop=on_stop, reopen_delay=reopen_delay,
                         reopen_on_metadata=reopen_on_metadata, reopen_when=reopen_when, start=start, cmd, s)
end

# Stream to youtube using RTMP.
# @category Output
# @flag extra
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

let video.add_text.available = ref([("native", video.add_text.native)])

%ifdef video.add_text.gstreamer
video.add_text.available := [("gstreamer", video.add_text.gstreamer), ...!video.add_text.available]
%endif

%ifdef video.add_text.gd
video.add_text.available := [("gd", video.add_text.gd), ...!video.add_text.available]
%endif

# TODO: %ifdef cannot account for values defined in the scripting library.
# This is done in ffmpeg.liq for now.
#%ifdef video.add_text.ffmpeg
#video.add_text.available := [("FFmpeg", video.add_text.ffmpeg), ...!video.add_text.available]
#%endif

%ifdef video.add_text.sdl
video.add_text.available := [("sdl", video.add_text.sdl), ...!video.add_text.available]
%endif

let settings.video.add_text = settings.make(
  description="`video.add_text` implementation.",
  fst(list.hd(!video.add_text.available))
)

thread.run.recurrent((fun () ->
  begin
    add_text = settings.video.add_text()
    if list.assoc.mem(add_text, !video.add_text.available) then
      log.important(label="video.add_text", "Using #{add_text} implementation")
    else
      log.severe(label="video.add_text", "Cannot find #{add_text} implementation for `video.add_text`, using default #{fst(list.hd(!video.add_text.available))}")
    end
    (-1.)
  end))

# Display a text using the first available operator in: SDL, GStreamer, gd or native.
# @category Source / Video Processing
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
def replaces video.add_text(~id=null(),~color=0xffffff,~cycle=true,~font="",
                   ~metadata="",~size=18,~speed=70,~x=getter(10),~y=getter(10),
                   d,s) =
  add_text = list.assoc(default=snd(list.hd(!video.add_text.available)), settings.video.add_text(), !video.add_text.available)
  add_text(id=id,cycle=cycle,font=font,
     metadata=metadata,size=size,color=color,
     speed=speed,x=x,y=y,d,s)
end
