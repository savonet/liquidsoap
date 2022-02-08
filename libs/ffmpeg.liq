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
def video.testsrc.ffmpeg(~id=null(), ~pattern="testsrc", ~max_buffer=0.5, ~duration=null())
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
# @param ~listen Act as a RTMP server and wait for incoming connection
# @param url URL to read RTMP from, in the form `rtmp://IP:PORT/ENDPOINT`
def input.rtmp(~max_buffer=5., ~listen=true, url)
  input.ffmpeg(max_buffer=max_buffer, log_overfull=false, format="live_flv",
               int_args=[("listen", listen ? 1 : 0)], url)
end
%endif

# %ifdef ffmpeg.filter.drawtext
# let video.add_text.ffmpeg = ()

# # Display a text. Use this operator inside ffmpeg filters.
# # @category Source / Video Processing
# # @param ~color Text color (in 0xRRGGBB format).
# # @param ~cycle Cycle text when it reaches left boundary.
# # @param ~font Path to ttf font file.
# # @param ~metadata Change text on a particular metadata (empty string means disabled).
# # @param ~size Font size.
# # @param ~speed Horizontal speed in pixels per second (0 means no scrolling and update \
# #               according to x and y in case they are variable).
# # @param ~graph a ffmpeg filter graph to attach this filter to.
# # @param ~x x offset.
# # @param ~y y offset.
# # @params d Text to display.
# def video.add_text.ffmpeg.raw(~color=0xffffff,~cycle=true,~font="",
                              # ~metadata=null(),~size=18,~speed=70,~x=getter(10),~y=getter(10),
                              # ~graph, d, s) =
  # color = "0x" ^ string.hex_of_int(pad=6, color)

  # fontfile = 
    # if font == "" then
      # null()
    # else
      # font
    # end

  # x =
    # if speed != 0 then
      # last_time = ref(time())
      # changed = getter.changes(x)
      # effective_x = ref(getter.get(x))
      # getter({
        # begin
          # cur_time = time()
          # traveled_to = int(float(speed) * (cur_time - !last_time))
          # last_time := cur_time
          # if changed() then
            # effective_x := getter.get(x)
          # else
            # effective_x := !effective_x - traveled_to
          # end
          # if !effective_x < 0 then
            # effective_x := video.frame.width() - !effective_x
          # end
          # !effective_x
        # end})
    # else
      # x
    # end

  # filter = ffmpeg.filter.drawtext.create(
                                  # fontfile=fontfile, fontsize="#{size}", x="#{getter.get(x)}", y="#{getter.get(y)}",
                                  # fontcolor=color, text=getter.get(d), graph)

  # filters = [{
    # args = getter({"x=#{getter.get(x)}:y=#{getter.get(y)}:text=#{getter.get(d)}"}),
    # filter = filter
  # }]

  # filters =
    # if cycle then
      # x = getter({"min(#{getter.get(x)}-w,#{getter.get(x)}-text_w)"})
      # [...filters,{
        # args   = getter({"x=#{getter.get(x)}:y=#{getter.get(y)}:text=#{getter.get(d)}"}),
        # filter = ffmpeg.filter.drawtext.create(
                                      # fontfile=fontfile, fontsize="#{size}", x="#{getter.get(x)}", y="#{getter.get(y)}",
                                      # fontcolor=color, text=getter.get(d), graph)}]
    # else filters end

  # changed = getter.changes(getter({(getter.get(x),getter.get(y),getter.get(d))}))
  # def on_change()
    # ignore(getter.get(x))
    # ignore(getter.get(y))
    # ignore(getter.get(d))
    # if changed() then
      # list.iter((fun (el) ->
        # ignore(el.filter.process_command("reinit", getter.get(el.args)))), filters)
    # end
  # end

  # def on_metadata(m) =
    # if null.defined(metadata) then
      # meta = null.get(metadata)
      # d = string.quote(m[meta])
      # if d != "" then
        # log(level=3,  label="ffmpeg.filter.drawtext", "Setting new text #{d} from metadata #{meta}") 
        # list.iter((fun (el) ->
           # ignore(el.filter.process_command("reinit", "text=#{d}"))), filters)
      # end
    # end
  # end

  # s.on_metadata(on_metadata)
  # s = source.on_frame(s, on_change)
  # s = ffmpeg.filter.video.input(graph, s)

  # list.fold((fun (cur, el) ->
    # begin
      # el.filter.set_input(cur)
      # el.filter.output
    # end), s, filters)
# end

# # Display a text.
# # @category Source / Video Processing
# # @param ~id Force the value of the source ID.
# # @param ~color Text color (in 0xRRGGBB format).
# # @param ~cycle Cycle text when it reaches left boundary.
# # @param ~font Path to ttf font file.
# # @param ~metadata Change text on a particular metadata (empty string means disabled).
# # @param ~size Font size.
# # @param ~speed Horizontal speed in pixels per second (0 means no scrolling and update \
# #               according to x and y in case they are variable).
# # @param ~x x offset.
# # @param ~y y offset.
# # @params d Text to display.
# def replaces video.add_text.ffmpeg(~id=null(),~color=0xffffff,~cycle=true,~font="",
                          # ~metadata=null(),~size=18,~speed=70,~x=getter(10),~y=getter(10),
                          # d, s) =
  # id =  string.id.default(default="video.add_text.ffmpeg", id)
  # s = ffmpeg.raw.encode.audio_video(%ffmpeg(%audio.raw, %video.raw), s)
  # v = drop_audio(s)
  # def mkfilter(graph)
    # v = video.add_text.ffmpeg.raw(color=color,cycle=cycle,font=font,metadata=metadata,
                                  # size=size,speed=speed,x=x,y=y,graph=graph,d,v)
    # ffmpeg.filter.video.output(graph, v)
  # end
  # v = ffmpeg.filter.create(mkfilter)
  # s = mux_audio(id=id,audio=drop_video(s),v)
  # ffmpeg.raw.decode.audio_video(s)
# end
# %endif

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
