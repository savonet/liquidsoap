
# Turn a source into an infaillible source.
# by adding blank when the source is not available.
# @param s the source to turn infaillible
# @category Source / Input
def mksafe(s)
  fallback(track_sensitive=false,[s,blank()])
end

# Backward compatible function for request.create().
# @category Deprecated
def request(s)
  request.create(audio=true,s)
end

# Alias for the <code>l[k]</code> notation.
# @category List
# @param a Key to look for
# @param l List of pairs (key,value)
def list.assoc(a,l)
  l[a]
end

# list.mem_assoc(key,l) returns true if l contains a pair
# (key,value)
# @category List
# @param a Key to look for
# @param l List of pairs (key,value)
def list.mem_assoc(a,l)
  v = list.assoc(a,l)
  # We check for existence, since "" may indicate
  # either a binding (a,"") or no binding..
  list.mem((a,v),l)
end

# Remove a pair from an associative list
# @category List
# @param a Key of pair to be removed
# @param l List of pairs (key,value)
def list.remove_assoc(a,l)
  list.remove((a,list.assoc(a,l)),l)
end

# Rewrite metadata on the fly using a list of (target,rules).
# @category Source / Track Processing
# @param l list of (target,value) rewriting rules
# @param ~insert_missing Treat track beginnings without metadata as having empty ones. The operational order is: create empty if needed, map and strip if enabled.
# @param ~update Only update metadata. If false, only returned values will be set as metadata.
# @param ~strip Completly remove empty metadata. Operates on both empty values and empty metadata chunk.
def rewrite_metadata(l,~insert_missing=true,
                       ~update=true,~strip=false,
		       s)
  # We don't need to return all values, since
  # map_metadata only update returned values.
  # So, we simply apply all rewrite rules !
  def map(m)
    def apply(x)
      label = fst(x)
      value = snd(x)
      (label,value % m)
    end
    list.map(apply,l)
  end
  map_metadata(map,insert_missing=insert_missing,
               update=update,strip=strip,s)
end

# Removes all metadata coming from a source
# @category Source / Track Processing
def clear_metadata(s)
  def map(m)
    []
  end
  map_metadata(map,update=false,strip=true,s)
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
# Output to local audio card using the first available driver in this list:
#   ao, alsa, oss, dummy
# @category Source / Output
def output.prefered(~id="",s)
  output.prefered(id=id,s)
end

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
# Create a source from the first available input driver in this list:
#   portaudio, alsa, oss, blank
# @category Source / Input
def in()
  in()
end

# Output a stream using the 'output.prefered' operator. The input source does
# not need to be infallible, blank will just be played during failures.
# @param s the source to output
# @category Source / Output
def out(s)
  output.prefered(mksafe(s))
end

%ifdef output.icecast.mp3
# Output to shoutcast using the MP3 format.
# @category Source / Output
def output.shoutcast.mp3 = output.icecast.mp3(mount = "/", protocol = "icy") end
%endif

# Special track insensitive fallback that
# always skip current song before switching.
# @category Source / Track Processing
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
# @category Source / Sound Processing
# @param s The input source.
def nrj(s)
  compress(threshold=-15.,ratio=3.,gain=3.,normalize(s))
end

# Multiband-compression.
# @category Source / Sound Processing
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

# Simple crossfade.
# @category Source / Track Processing
# @param ~start_next Duration in seconds of the crossed end of track.
# @param ~fade_in Duration of the fade in for next track
# @param ~fade_out Duration of the fade out for previous track
# @param s The source to use
def crossfade(~start_next,~fade_in,~fade_out,s)
  s = fade.in(duration=fade_in,s)
  s = fade.out(duration=fade_out,s)
  fader = fun (a,b) -> add(normalize=false,[b,a])
  cross(duration=start_next,fader,s)
end

# Append speech-synthesized tracks reading the metadata.
# @category Source / Track Processing
# @param ~pattern Pattern to use
# @param s The source to use
def say_metadata
  p = 'say:$(if $(artist),"It was $(artist)$(if $(title),\", $(title)\").")'
  fun (s,~pattern=p) ->
    append(s,fun (m) -> request.queue(queue=[request.create(pattern % m)],
                                      interactive=false))
end

# Relay the audio stream of Dolebraï, a libre music netradio running liquidsoap.
# @category Source / Input
def dolebrai ()
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

# Return true if process exited with 0 code.
# Command should return quickly.
# @category System
# @param command    Command to test
def test_process(command)
  "0" == list.hd(get_process_lines("(" ^ command ^ " >/dev/null 2>&1 && echo 0) || echo 1"))
end

# Get the base name of a path.
# Implemented using the corresponding shell command.
# @category System
# @param s Path
def basename(s)
  list.hd(get_process_lines("basename " ^ quote(s)))
end

# Get the directory name of a path.
# Implemented using the corresponding shell command.
# @category System
# @param s Path
def dirname(s)
  list.hd(get_process_lines("dirname " ^ quote(s)))
end

# Read some value from standard input (console).
# @category System
# @param ~hide Hide typed characters (for passwords).
def read(~hide=false)
  if hide then
    system("stty -echo")
  end
  s = list.hd(get_process_lines("read BLA && echo $BLA"))
  if hide then
    system("stty echo")
  end
  print("")
  s
end

# Remove low frequencies often produced by microphones.
# @category Source / Sound Processing
# @param s The input source.
def mic_filter(s)
  filter(freq=200.,q=1.,mode="high",s)
end

# Creates a source that fails to produce anything.
# @category Source / Input
def fail()
  fallback([])
end

# Creates a source that plays only one track of the input source.
# @category Source / Track Processing
# @param s The input source.
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

# Custom playlist source written using the script language.
# Will read directory or playlist, play all files and stop
# @category Source / Input
# @param ~random Randomize playlist content
# @param uri Playlist URI
def playlist.once(~random=false,uri)
  x = string.ref("0")
  get = fst(x)
  set = snd(x)
  def playlist.custom(files)
    length = list.length(files)
    if length == 0 then
      log("Empty playlist..")
      fail ()
    else
      files =
        # randomize if told to
        if random then
          list.sort(fun (x,y) -> int_of_float(random.float()),
                    files)
        else
          files
        end
      def next () = 
        state = int_of_string(get())
	file = 
	  if state < length then
            set(string_of(state + 1))
            list.nth(files,state)
          else
            # Playlist finished
            ""
          end
        request.create(audio=true,file)
      end
      request.dynamic(next)
    end
  end
  if test_process("test -d #{quote(uri)}") then
    files = get_process_lines("find #{quote(uri)} -type f | sort")
    playlist.custom(files)
  else
    playlist = request.create(audio=false,uri)
    if request.resolve(playlist) then
      playlist = request.filename(playlist)
      files = playlist.parse(playlist)
      files = list.map(snd,files)
      playlist.custom(files)
    else
      log("Couldn't read playlist: request resolution failed.")
      fail ()
    end
  end
end

# Mixes two streams, with faded transitions between the state when only the
# normal stream is available and when the special stream gets added on top of
# it.
# @category Source / Track Processing
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

# Restrict a source to play only when a predicate is true.
# @category Source / Track Processing
# @param pred The predicate, typically a time interval such as <code>{10h-10h30}</code>.
def at(pred,s)
  switch([(pred,s)])
end
