
float = float_of_int

def mksafe(s)
  fallback(track_sensitive=false,[s,blank()])
end

output.prefered=output.dummy
%ifdef output.oss
  output.prefered=output.oss
%endif
%ifdef output.alsa
  output.prefered=output.alsa
%endif
%ifdef output.ao
  output.prefered=output.ao
%endif

in = fun () -> blank()
%ifdef input.oss
  in = fun () -> input.oss(id="oss_mic")
%endif
%ifdef input.alsa
  in = fun () -> input.alsa(id="alsa_mic")
%endif
%ifdef input.portaudio
  in = fun () -> input.portaudio(id="pa_mic")
%endif

# Output a stream using the 'output.prefered' operator. The input source does
# not need to be infallible, blank will just be played during failures.
def out(s)
  output.prefered(mksafe(s))
end

%ifdef output.icecast.mp3
# Output to shoutcast using the MP3 format.
def output.shoutcast.mp3 = output.icecast.mp3(mount = "/", protocol = "icy") end
%endif

# Special track insensitive fallback that
# always skip current song before switching.
# @param s The input source
# @param f The fallback source
def fallback.skip(s,f)
  def transition(a,b) =
    source.skip(a)
    # This eats the last remaining frame from a
    sequence([a,b])
  end
  fallback(track_sensitive=false,transitions=[transition,transition],[s,f])
end

# Compress and normalize, producing a more uniform and "full" sound.
# @param s The input source.
def nrj(s)
  compress(threshold=-15.,ratio=3.,gain=3.,normalize(s))
end

# Multiband-compression.
# @param s The input source.
def sky(s)
  # 3-band crossover
  low = filter.iir.eq.low(frequency = 168.)
  mh = filter.iir.eq.high(frequency = 100.)
  mid = filter.iir.eq.low(frequency = 1800.)
  high = filter.iir.eq.high(frequency = 1366.)

  # Add back
  add(normalize = false,
      [ compress(attack = 100., release = 200., threshold = -20.,
                 ratio = 6., gain = 6.7, knee = 0.3,
           low(s)),
        compress(attack = 100., release = 200., threshold = -20.,
                 ratio = 6., gain = 6.7, knee = 0.3,
           mid(mh(s))),
        compress(attack = 100., release = 200., threshold = -20.,
                 ratio = 6., gain = 6.7, knee = 0.3,
           high(s))
      ])
end

def crossfade(~start_next,~fade_in,~fade_out,s)
  s = fade.in(duration=fade_in,s)
  s = fade.out(duration=fade_out,s)
  fader = fun (a,b) -> add(normalize=false,[b,a])
  cross(duration=start_next,fader,s)
end

# Append speech-synthesized tracks reading the metadata.
def say_metadata
  p = 'say:$(if $(artist),"It was $(artist)$(if $(title),\", $(title)\").")'
  fun (s,~pattern=p) ->
    append(s,fun (m) -> request.queue(queue=[request(pattern % m)],
                                      interactive=false))
end

# Relay the audio stream of Dolebraï, a libre music netradio running liquidsoap.
def dolebrai
  input.http(id="dolebrai","http://dolebrai.net:8000/dolebrai.ogg")
end

%ifdef soundtouch
# Increases the pitch, making voices sound like on helium.
# @category Source / Sound Processing
# @param s The input source.
def helium(s)
  soundtouch(pitch=1.5,s)
end
%endif

%ifdef input.lastfm
# Utility to compose last.fm URIs.
# @category String
# @param ~user      Lastfm user
# @param ~password  Lastfm password
# @param ~discovery Allow lastfm suggestions
# @param radio      URI, e.g. user/toots5446/playlist, globaltags/rocksteady.
def lastfm.uri(~user="",~password="",~discovery=false,
               radio="globaltags/creative-commons")
  auth = if user == "" then "" else "#{user}:#{password}@" end
  discovery = if discovery == true then "1" else "0" end
  "lastfm://#{auth}#{radio}?discovery=#{discovery}"
end
%endif

# Remove low frequencies often produced by microphones.
def mic_filter(s)
  filter(freq=200.,q=1.,mode="high",s)
end

# Creates a source that fails to produce anything.
def fail()
  fallback([])
end

# Creates a source that plays only one track of the input source.
def once(s)
  sequence([s,fail()])
end

# Crossfade between tracks, taking the respective volume levels into account in 
# the choice of the transition.
# @category Source / Track Processing
# @param ~start_next   Crossing duration, if any.
# @param ~fade_in      Fade-in duration, if any.
# @param ~fade_out     Fade-out duration, if any.
# @param ~width        Width of the volume analysis window.
# @param ~conservative Always prepare for a premature end-of-track.
# @param s             The input source.
def smart_crossfade (~start_next=5.,~fade_in=3.,~fade_out=3.,
                     ~width=2.,~conservative=false,s)
  high   = -20.
  medium = -32.
  margin = 4.
  fade.out = fade.out(type="sin",duration=fade_out)
  fade.in  = fade.in(type="sin",duration=fade_in)
  add = fun (a,b) -> add(normalize=false,[b,a])
  log = log(label="smart_crossfade")

  def transition(a,b,ma,mb,sa,sb)

    list.iter(fun(x)-> log(level=4,"Before: #{x}"),ma)
    list.iter(fun(x)-> log(level=4,"After : #{x}"),mb)

    if
      # If A and B and not too loud and close, fully cross-fade them.
      a <= medium and b <= medium and abs(a - b) <= margin
    then
      log("Transition: crossed, fade-in, fade-out.")
      add(fade.out(sa),fade.in(sb))

    elsif
      # If B is significantly louder than A, only fade-out A.
      # We don't want to fade almost silent things, ask for >medium.
      b >= a + margin and a >= medium and b <= high
    then
      log("Transition: crossed, fade-out.")
      add(fade.out(sa),sb)

    elsif
      # Do not fade if it's already very low.
      b >= a + margin and a <= medium and b <= high
    then
      log("Transition: crossed, no fade-out.")
      add(sa,sb)

    elsif
      # Opposite as the previous one.
      a >= b + margin and b >= medium and a <= high
    then
      log("Transition: crossed, fade-in.")
      add(sa,fade.in(sb))

    # What to do with a loud end and a quiet beginning ?
    # A good idea is to use a jingle to separate the two tracks,
    # but that's another story.

    else
      # Otherwise, A and B are just too loud to overlap nicely,
      # or the difference between them is too large and overlapping would 
      # completely mask one of them.
      log("No transition: just sequencing.")
      sequence([sa, sb])
    end
  end

  smart_cross(width=width, duration=start_next, conservative=conservative,
              transition,s)
end

# Mixes two streams, with faded transitions between the state when only the 
# normal stream is available and when the special stream gets added on top of 
# it.
# @param ~delay   Delay before starting the special source.
# @param ~p       Portion of amplitude of the normal source in the mix.
# @param ~normal  The normal source, which could be called the carrier too.
# @param ~special The special source.
def smooth_add(~delay=0.5,~p=0.2,~normal,~special)
  d = delay
  fade.final = fade.final(duration=d*2.)
  fade.initial = fade.initial(duration=d*2.)
  q = 1. - p
  c = amplify
  fallback(track_sensitive=false,
           [special,normal],
           transitions=[
             fun(normal,special)->
               add(normalize=false,
                   [c(p,normal),
                    c(q,fade.final(type="sin",normal)),
                    sequence([blank(duration=d),c(q,special)])]),
             fun(special,normal)->
               add(normalize=false,
                   [c(p,normal),
                    c(q,fade.initial(type="sin",normal))])
           ])
end
