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

# ffmpeg's test source video (useful for testing and debugging).
# @category Source / Video Processing
def video.external.testsrc()
  s = input.external.avi("ffmpeg -f lavfi -i testsrc -f avi -vf format=rgb24 -vcodec rawvideo -r #{video.frame.rate()} -s #{video.frame.width()}x#{video.frame.height()} pipe:1")
end
