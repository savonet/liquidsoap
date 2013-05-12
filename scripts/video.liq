%ifdef video.add_text.gd
# Add a scrolling line of text on video frames.
# @category Source / Video Processing
# @param ~id       Force the value of the source ID.
# @param ~color    Text color (in 0xRRGGBB format).
# @param ~cycle    Cycle text.
# @param ~font     Path to ttf font file.
# @param ~metadata Change text on a particular metadata \
#                  (empty string means disabled).
# @param ~size     Font size.
# @param ~speed    Speed in pixels per second (0 means no scrolling).
# @param ~x        x offset (negative means from right).
# @param ~y        y offset (negative means from bottom).
# @param text      Text to display.
def video.add_text(~id="",~color=16777215,~cycle=true,
                   ~font=configure.default_font,
                   ~metadata="",~size=18,~speed=70,~x=-1,~y=-5,
                   text,source)
  video.add_text.gd(id=id,color=color,cycle=cycle,font=font,metadata=metadata,
                    size=size,speed=speed,x=x,y=y,text,source)
end
%endif

%ifdef video.add_text.sdl
# Add a scrolling line of text on video frames.
# @category Source / Video Processing
# @param ~id       Force the value of the source ID.
# @param ~color    Text color (in 0xRRGGBB format).
# @param ~cycle    Cycle text.
# @param ~font     Path to ttf font file.
# @param ~metadata Change text on a particular metadata \
#                  (empty string means disabled).
# @param ~size     Font size.
# @param ~speed    Speed in pixels per second (0 means no scrolling).
# @param ~x        x offset (negative means from right).
# @param ~y        y offset (negative means from bottom).
# @param text      Text to display.
def video.add_text(~id="",~color=16777215,~cycle=true,
                   ~font=configure.default_font,
                   ~metadata="",~size=18,~speed=70,~x=-1,~y=-5,
                   text,source)
  video.add_text.sdl(id=id,color=color,cycle=cycle,font=font,metadata=metadata,
                     size=size,speed=speed,x=x,y=y,text,source)
end
%endif

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
  add([s,single(id=id,"annotate:#{width},#{height},#{x},#{y}:#{file}")])
end