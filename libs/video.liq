# Width for all video frames.
# @category Source / Video Processing
def video.frame.width() =
  get(default=-1,"frame.video.width")
end

# Height for all video frames.
# @category Source / Video Processing
def video.frame.height() =
  get(default=-1,"frame.video.height")
end

# Rate for all video frames.
# @category Source / Video Processing
def video.frame.rate() =
  get(default=-1,"frame.video.samplerate")
end

# Add a static image on the first video channel.
# @category Source / Video Processing
# @param ~id Force the value of the source ID.
# @param ~width Scale to width (zero means frame width, negative means original width).
# @param ~height Scale to height (zero means frame height, negative means original height).
# @param ~x x position.
# @param ~y y position.
# @param ~file Path to the image file.
def video.add_image(~id="",~width=0,~height=0,~x=max_int(),~y=max_int(),~file,s)
  width = if width == 0 then "" else "width=#{width}" end
  height = if height == 0 then "" else "height=#{height}" end
  x = if x == max_int() then "" else "x=#{x}" end
  y = if y == max_int() then "" else "y=#{y}" end
  image = single(id=id,"annotate:#{width},#{height},#{x},#{y}:#{file}")
  image = mux_audio(audio=blank(), image)
  add([s,image])
end

# Input from ffmpeg.
# @category Source / Video Processing
# @param input ffmpeg options specifying the input
def input.external.ffmpeg(~id="", ~show_command=false, input)
  input = (input : string)
  cmd = "ffmpeg #{input} -f avi -vf format=rgb24 -vf \"scale=#{video.frame.width()}:#{video.frame.height()}:force_original_aspect_ratio=decrease,pad=#{video.frame.width()}:#{video.frame.height()}:(ow-iw)/2:(oh-ih)/2\" -c:v rawvideo -r #{video.frame.rate()} -c:a pcm_s16le -ac 2 -ar #{audio.samplerate()} pipe:1"
  if show_command then log.important("command: #{cmd}") end
  (input.external.avi(cmd):source(audio=2,video=1,midi=0))
end

# ffmpeg's test source video (useful for testing and debugging).
# @category Source / Video Processing
def video.external.testsrc(~id="")
  input.external.ffmpeg(id=id, "-f lavfi -i testsrc=size=#{video.frame.width()}x#{video.frame.height()}:rate=#{video.frame.rate()} -f lavfi -i sine=frequency=1000:sample_rate=#{audio.samplerate()}")
end

# Output to ffmpeg.
# @category Source / Video Processing
# @param input ffmpeg options specifying the output
def output.external.ffmpeg(~id="", ~show_command=false, output, s)
  output = (output : string)
  cmd = "ffmpeg -f avi -vcodec rawvideo -r #{video.frame.rate()} -acodec pcm_s16le -i pipe:0 #{output}"
  if show_command then log.important(label="output.external.ffmpeg", "command: #{cmd}") end
  output.external(id=id, %avi(), cmd, s)
end

def output.file.hls.ffmpeg(~id="", ~playlist="stream.m3u8", dir, s)
  width = video.frame.width()
  height = video.frame.height()
  cmd = "-profile:v baseline -level 3.0 -s #{width}x#{height} -start_number 0 -hls_time 10 -hls_list_size 0 -f hls #{dir}/#{playlist}"
  output.external.ffmpeg(id=id, cmd, s)
end

# Stream to youtube.
# @category Source / Video Processing
# @param ~id Force the value of the source ID.
# @param ~url URL to stream to
# @param ~bitrate Bitrate of the video (in kbps)
# @param ~quality Quality of the video (low / medium / high)
# @param ~key Your secret youtube key
def output.youtube.live.ffmpeg(~id="",~url="rtmp://a.rtmp.youtube.com/live2",~bitrate="2500",~quality="medium",~key,s)
  # Output parameters
  fps = video.frame.rate()
  samplerate = audio.samplerate()
  output.external.ffmpeg(id=id, "-deinterlace -vcodec libx264 -pix_fmt yuv420p -preset #{quality} -r #{fps} -g $((#{fps} * 2)) -b:v #{bitrate}k -acodec libmp3lame -ar #{samplerate} -threads 6 -qscale:v 3 -b:a 712000 -bufsize 512k -f flv \"#{url}/#{key}\"", s)
end
