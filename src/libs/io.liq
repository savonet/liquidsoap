let replaces output=output.dummy
%ifdef output.ao
  let replaces output=output.ao
%endif
%ifdef output.alsa
  let replaces output=output.alsa
%endif
%ifdef output.oss
  let replaces output=output.oss
%endif
%ifdef output.portaudio
  let replaces output=output.portaudio
%endif
%ifdef output.pulseaudio
  let replaces output=output.pulseaudio
%endif

# Output a stream using the default operator. The input source does not need to
# be infallible, blank will just be played during failures.
# @category Source / Output
# @param ~id Force the value of the source ID.
# @param ~fallible Allow the child source to fail, in which case the output will be (temporarily) stopped.
# @param ~on_start Callback executed when outputting starts.
# @param ~on_stop Callback executed when outputting stops.
# @param ~start Automatically start outputting whenever possible. If `true`, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
# @param s Source to play.
def replaces output(~id=null(),~fallible=true,
           ~on_start={()},~on_stop={()},~start=true,s)
  output(id=id,fallible=fallible,
         start=start,on_start=on_start,on_stop=on_stop,
         s)
end

let output.video = output.dummy
%ifdef output.sdl
  def output.video(%argsof(output.sdl), s)
    if output.sdl.has_video() then
      (output.sdl(%argsof(output.sdl), s):unit)
    else
      # Avoid using SDL when there is no video output.
      (output.dummy(fallible=fallible, s):unit)
    end
  end
%endif
%ifdef output.graphics
  let output.video = output.graphics
%endif

# Output a video stream using the default operator. The input source does not
# need to be infallible, blank will just be played during failures.
# @category Source / Output
# @param ~id Force the value of the source ID.
# @param ~fallible Allow the child source to fail, in which case the output will be (temporarily) stopped.
# @param ~on_start Callback executed when outputting starts.
# @param ~on_stop Callback executed when outputting stops.
# @param ~start Automatically start outputting whenever possible. If `true`, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
# @param s Source to play.
def output.video(~id=null(),~fallible=true,~on_start={()},~on_stop={()},~start=true,s)
  output.video(id=id, fallible=fallible, start=start, on_start=on_start, on_stop=on_stop, s)
end

# Output a stream with audio and video using the default operator. The input
# source does not need to be infallible, blank will just be played during
# failures.
# @category Source / Output
# @param ~id Force the value of the source ID.
# @param ~fallible Allow the child source to fail, in which case the output will be (temporarily) stopped.
# @param ~on_start Callback executed when outputting starts.
# @param ~on_stop Callback executed when outputting stops.
# @param ~start Automatically start outputting whenever possible. If `true`, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
# @param s Source to play.
def output.audio_video(~id=null(),~fallible=true,~on_start={()},~on_stop={()},~start=true,s)
  let {audio,video} = source.tracks(s)

  output(id=id, fallible=fallible, start=start, on_start=on_start, on_stop=on_stop, source({audio=audio}))
  output.video(id=id, fallible=fallible, start=start, on_start=on_start, on_stop=on_stop, source({video=video}))
end

def replaces input(~id=null(),~start=true,~on_start={()},~on_stop={()},~fallible=false)
  ignore(start)
  ignore(on_start)
  ignore(on_stop)
  ignore(fallible)
  blank(id=id)
end
%ifdef input.alsa
  let replaces input = input.alsa
%endif
%ifdef input.oss
  let replaces input = input.oss
%endif
%ifdef input.portaudio
  let replaces input = input.portaudio
%endif
%ifdef input.pulseaudio
  let replaces input = input.pulseaudio
%endif

# Input an audio stream using the default operator.
# @category Source / Input
# @param ~id Force the value of the source ID.
# @param ~fallible Allow the child source to fail, in which case the output will be (temporarily) stopped.
# @param ~on_start Callback executed when outputting starts.
# @param ~on_stop Callback executed when outputting stops.
# @param ~start Automatically start outputting whenever possible. If `true`, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
def replaces input(~id=null(),~start=true,~on_start={()},~on_stop={()},~fallible=false)
  input(id=id,start=start,on_start=on_start,on_stop=on_stop,fallible=fallible)
end
