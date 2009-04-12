
# Turn a source into an infaillible source.
# by adding blank when the source is not available.
# @param s the source to turn infaillible
# @category Source / Input
def mksafe(s)
  fallback(id="mksafe",track_sensitive=false,[s,blank(id="safe_blank")])
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

# Output the stream using aplay.
# Using this turns "root.sync" to false
# since aplay will do the synchronisation
# @category Source / Output
# @param ~id Output's ID
# @param ~device Alsa pcm device name
# @param ~restart_on_crash Restart external process on crash. If false, liquidsoap will stop.
# @param s Source to play
def output.aplay(~id="output.aplay",~device="default",
                 ~restart_on_crash=false,s)
  def aplay_p(m) = 
    "aplay -D #{device}"
  end
  log(label=id,level=3,"Setting root.sync to false")
  set("root.sync",false)
  output.pipe.external(id=id,
                       restart_on_crash=restart_on_crash,
                       restart_on_new_track=false,
                       process=aplay_p,s)
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
# @param ~id Output's ID
# @param ~start Start output threads on operator initialization.
# @param ~restart Restart output after a failure. By default, liquidsoap will stop if the output failed.
# @param ~restart_delay Delay, in seconds, before attempting new connection, if restart is enabled.
# @param ~user User for shout source connection. Useful only in special cases, like with per-mountpoint users.
# @param ~bitrate Encoder bitrate
# @param ~dumpfile Dump stream to file, for debugging purpose. Disabled if empty.
# @param s The source to output
def output.shoutcast.mp3(
  ~id="output.shoutcast.mp3",~start=true,
  ~restart=true,~restart_delay=3,
  ~host="localhost",~port=8000,
  ~user="source",~password="hackme",
  ~genre="Misc",~url="http://savonet.sf.net/",
  ~description="OCaml Radio!",~public=true,
  ~dumpfile="",~name="Use [mount]",
  ~aim="",~icq="",~irc="",
  ~bitrate=128,~samplerate=44100,~stereo=true,
  s) = 
  headers = [("icy-aim",aim),("icy-irc",irc),("icy-icq",icq)]
  output.icecast.mp3(
    id=id, headers=headers,
    bitrate=bitrate, start=start,
    restart=restart, restart_delay=restart_delay,
    host=host, port=port, user=user, password=password,
    genre=genre, url=url, description=description,
    public=public, dumpfile=dumpfile,
    name=name, mount="/", protocol="icy",
    samplerate=samplerate,stereo=stereo,
    s)
end
%endif

%ifdef output.icecast.external
# Output to icecast using the lame command line encoder.
# @category Source / Output
# @param ~id Output's ID
# @param ~start Start output threads on operator initialization.
# @param ~restart Restart output after a failure. By default, liquidsoap will stop if the output failed.
# @param ~restart_delay Delay, in seconds, before attempting new connection, if restart is enabled.
# @param ~restart_on_crash Restart external process on crash. If false, liquidsoap will stop.
# @param ~restart_on_new_track Restart encoder upon new track.
# @param ~restart_encoder_delay Restart the encoder after this delay, in seconds.
# @param ~user User for shout source connection. Useful only in special cases, like with per-mountpoint users.
# @param ~lame The lame binary
# @param ~bitrate Encoder bitrate
# @param ~swap Swap audio samples. Depends on local machine's endianess and lame's version. Test this parameter if you experience garbaged mp3 audio data. On intel 32 and 64 architectures, the parameter should be "true" for lame prior 3.98.
# @param ~dumpfile Dump stream to file, for debugging purpose. Disabled if empty.
# @param ~protocol Protocol of the streaming server: 'http' for Icecast, 'icy' for Shoutcast.
# @param s The source to output
def output.icecast.lame(
  ~id="output.icecast.lame",~start=true,
  ~restart=true,~restart_delay=3,
  ~host="localhost",~port=8000,
  ~user="source",~password="hackme",
  ~genre="Misc",~url="http://savonet.sf.net/",
  ~description="OCaml Radio!",~public=true,
  ~dumpfile="",~mount="Use [name]",
  ~name="Use [mount]",~protocol="http",
  ~lame="lame",~bitrate=128,~swap=false,
  ~restart_on_crash=false,~restart_on_new_track=false,
  ~restart_encoder_delay=3600,~headers=[],s)
  samplerate = get(default=44100,"frame.samplerate")
  samplerate = float_of_int(samplerate) / 1000.
  channels = get(default=2,"frame.channels")
  swap = if swap then "-x" else "" end
  mode = 
    if channels == 2 then
      "j" # Encoding in joint stereo..
    else
      "m"
    end
  # Metadata update is set by ICY with icecast
  def lame_p(m)
    "#{lame} -b #{bitrate} -r --bitwidth 16 -s #{samplerate} \
       --signed -m #{mode} --nores #{swap} -t - -"
  end
  output.icecast.external(id=id,
    process=lame_p,bitrate=bitrate,start=start,
    restart=restart,restart_delay=restart_delay,
    host=host,port=port,user=user,password=password,
    genre=genre,url=url,description=description,
    public=public,dumpfile=dumpfile,restart_encoder_delay=restart_encoder_delay,
    name=name,mount=mount,protocol=protocol,
    header=false,restart_on_crash=restart_on_crash,
    restart_on_new_track=restart_on_new_track,headers=headers,
    s)
end

# Output to shoutcast using the lame encoder.
# @category Source / Output
# @param ~id Output's ID
# @param ~start Start output threads on operator initialization.
# @param ~restart Restart output after a failure. By default, liquidsoap will stop if the output failed.
# @param ~restart_delay Delay, in seconds, before attempting new connection, if restart is enabled.
# @param ~restart_on_crash Restart external process on crash. If false, liquidsoap will stop.
# @param ~restart_on_new_track Restart encoder upon new track.
# @param ~restart_encoder_delay Restart the encoder after this delay, in seconds.
# @param ~user User for shout source connection. Useful only in special cases, like with per-mountpoint users.
# @param ~lame The lame binary
# @param ~bitrate Encoder bitrate
# @param ~dumpfile Dump stream to file, for debugging purpose. Disabled if empty.
# @param s The source to output
def output.shoutcast.lame(
  ~id="output.shoutcast.mp3",~start=true,
  ~restart=true,~restart_delay=3,
  ~host="localhost",~port=8000,
  ~user="source",~password="hackme",
  ~genre="Misc",~url="http://savonet.sf.net/",
  ~description="OCaml Radio!",~public=true,
  ~dumpfile="",~name="Use [mount]",
  ~lame="lame",~aim="",~icq="",~irc="",
  ~restart_on_crash=false,~restart_on_new_track=false,
  ~restart_encoder_delay=3600,~bitrate=128,s) =
  headers = [("icy-aim",aim),("icy-irc",irc),("icy-icq",icq)]
  output.icecast.lame(
    id=id, headers=headers, lame=lame,
    bitrate=bitrate, start=start,
    restart=restart, restart_encoder_delay=restart_encoder_delay,
    host=host, port=port, user=user, password=password,
    genre=genre, url=url, description=description,
    public=public, dumpfile=dumpfile,
    restart_on_crash=restart_on_crash,
    restart_on_new_track=restart_on_new_track,
    name=name, mount="/", protocol="icy",
    s)
end

# Output to icecast using the flac command line encoder.
# @category Source / Output
# @param ~id Output's ID
# @param ~start Start output threads on operator initialization.
# @param ~restart Restart output after a failure. By default, liquidsoap will stop if the output failed.
# @param ~restart_delay Delay, in seconds, before attempting new connection, if restart is enabled.
# @param ~restart_on_crash Restart external process on crash. If false, liquidsoap will stop.
# @param ~restart_on_new_track Restart encoder upon new track. If false, the resulting stream will have a single track.
# @param ~restart_encoder_delay Restart the encoder after this delay, in seconds.
# @param ~user User for shout source connection. Useful only in special cases, like with per-mountpoint users.
# @param ~flac The flac binary
# @param ~quality Encoder quality (0..8)
# @param ~dumpfile Dump stream to file, for debugging purpose. Disabled if empty.
# @param ~protocol Protocol of the streaming server: 'http' for Icecast, 'icy' for Shoutcast.
# @param s The source to output
def output.icecast.flac(
  ~id="output.icecast.flac",~start=true,
  ~restart=true,~restart_delay=3,
  ~host="localhost",~port=8000,
  ~user="source",~password="hackme",
  ~genre="Misc",~url="http://savonet.sf.net/",
  ~description="OCaml Radio!",~public=true,
  ~dumpfile="",~mount="Use [name]",
  ~name="Use [mount]",~protocol="http",
  ~flac="flac",~quality=6,
  ~restart_on_crash=false,
  ~restart_on_new_track=true,
  ~restart_encoder_delay=(-1),s)
  # We will use raw format, to 
  # bypass input length value in WAV
  # header (input length is not known)
  channels = get(default=2,"frame.channels")
  samplerate = get(default=44100,"frame.samplerate")
  def flac_p(m)=
    def option(x) = 
      "-T #{quote(fst(x))}=#{quote(snd(x))}"
    end
    m = list.map(option,m)
    m = string.concat(separator=" ",m) 
    "#{flac} --force-raw-format --endian=little --channels=#{channels} \
      --bps=16 --sample-rate=#{samplerate} --sign=signed #{m} \
      -#{quality} --ogg -c -"
  end
  output.icecast.external(id=id,
    process=flac_p,bitrate=(-1),start=start,
    restart=restart,restart_delay=restart_delay,
    host=host,port=port,user=user,password=password,
    genre=genre,url=url,description=description,
    public=public,dumpfile=dumpfile,
    name=name,mount=mount,protocol=protocol,
    restart_on_new_track=restart_on_new_track,
    format="ogg",header=false,icy_metadata=false,
    restart_on_crash=restart_on_crash,
    restart_encoder_delay=restart_encoder_delay,
    s)
end

# Output to icecast using the aacplusenc command line encoder.
# @category Source / Output
# @param ~id Output's ID
# @param ~start Start output threads on operator initialization.
# @param ~restart Restart output after a failure. By default, liquidsoap will stop if the output failed.
# @param ~restart_delay Delay, in seconds, before attempting new connection, if restart is enabled.
# @param ~restart_on_crash Restart external process on crash. If false, liquidsoap will stop.
# @param ~restart_on_new_track Restart encoder upon new track.
# @param ~restart_encoder_delay Restart the encoder after this delay, in seconds.
# @param ~user User for shout source connection. Useful only in special cases, like with per-mountpoint users.
# @param ~aacplusenc The aacplusenc binary
# @param ~bitrate Encoder bitrate
# @param ~dumpfile Dump stream to file, for debugging purpose. Disabled if empty.
# @param ~protocol Protocol of the streaming server: 'http' for Icecast, 'icy' for Shoutcast.
# @param s The source to output
def output.icecast.aacplusenc(
  ~id="output.icecast.aacplusenc",~start=true,
  ~restart=true,~restart_delay=3,
  ~host="localhost",~port=8000,
  ~user="source",~password="hackme",
  ~genre="Misc",~url="http://savonet.sf.net/",
  ~description="OCaml Radio!",~public=true,
  ~dumpfile="",~mount="Use [name]",
  ~name="Use [mount]",~protocol="http",
  ~aacplusenc="aacplusenc",~bitrate=64,
  ~restart_on_crash=false,~restart_on_new_track=false,
  ~restart_encoder_delay=3600,~headers=[],s)
  # Metadata update is set by ICY with icecast
  def aacplusenc_p(m)
    "#{aacplusenc} - - #{bitrate}"
  end
  output.icecast.external(id=id,
    process=aacplusenc_p,bitrate=bitrate,start=start,
    restart=restart,restart_delay=restart_delay,
    host=host,port=port,user=user,password=password,
    genre=genre,url=url,description=description,
    public=public,dumpfile=dumpfile,
    name=name,mount=mount,protocol=protocol,
    header=true,restart_on_crash=restart_on_crash,
    restart_on_new_track=restart_on_new_track,headers=headers,
    restart_encoder_delay=restart_encoder_delay,format="audio/aacp",s)
end

# Output to shoutcast using the aacplusenc encoder.
# @category Source / Output
# @param ~id Output's ID
# @param ~start Start output threads on operator initialization.
# @param ~restart Restart output after a failure. By default, liquidsoap will stop if the output failed.
# @param ~restart_delay Delay, in seconds, before attempting new connection, if restart is enabled.
# @param ~restart_on_crash Restart external process on crash. If false, liquidsoap will stop.
# @param ~restart_on_new_track Restart encoder upon new track.
# @param ~restart_encoder_delay Restart the encoder after this delay, in seconds.
# @param ~user User for shout source connection. Useful only in special cases, like with per-mountpoint users.
# @param ~aacplusenc The aacplusenc binary
# @param ~bitrate Encoder bitrate
# @param ~dumpfile Dump stream to file, for debugging purpose. Disabled if empty.
# @param s The source to output
def output.shoutcast.aacplusenc(
  ~id="output.shoutcast.aacplusenc",~start=true,
  ~restart=true,~restart_delay=3,
  ~host="localhost",~port=8000,
  ~user="source",~password="hackme",
  ~genre="Misc",~url="http://savonet.sf.net/",
  ~description="OCaml Radio!",~public=true,
  ~dumpfile="",~name="Use [mount]",
  ~aim="",~icq="",~irc="",~aacplusenc="aacplusenc",
  ~restart_on_crash=false,~restart_on_new_track=false,
  ~restart_encoder_delay=3600,~bitrate=64,s) =
  headers = [("icy-aim",aim),("icy-irc",irc),("icy-icq",icq)]
  output.icecast.aacplusenc(
    id=id, headers=headers, aacplusenc=aacplusenc,
    bitrate=bitrate, start=start,
    restart=restart, restart_delay=restart_delay,
    host=host, port=port, user=user, password=password,
    genre=genre, url=url, description=description,
    public=public, dumpfile=dumpfile,
    restart_on_crash=restart_on_crash, restart_encoder_delay=restart_encoder_delay,
    restart_on_new_track=restart_on_new_track,
    name=name, mount="/", protocol="icy",
    s)
end
%endif

# Special track insensitive fallback that
# always skip current song before switching.
# @category Source / Track Processing
# @param ~input The input source
# @param f The fallback source
def fallback.skip(~input,f)
  def transition(a,b) =
    source.skip(a)
    # This eats the last remaining frame from a
    sequence([a,b])
  end
  fallback(track_sensitive=false,transitions=[transition,transition],[input,f])
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
  cross(conservative=true,duration=start_next,fader,s)
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
  lines =
    get_process_lines("(" ^ command ^ " >/dev/null 2>&1 && echo 0) || echo 1")
  if list.length(lines) == 0 then
    false
  else
    "0" == list.hd(lines)
  end
end

# Decoders, enabled when the binary is detected
if test_process("which flac") then
  def flac_p(file)=
    "flac -d -c #{quote(file)} 2>/dev/null"
  end
  add_decoder("FLAC",flac_p)
else
  log(level=3,"flac binary not found: flac decoder disabled.")
end
if test_process("which metaflac") then
  def flac_meta(~format,file)
    if format != "FLAC" then
      []
    else
      ret = get_process_lines("metaflac --export-tags-to=- \
                   #{quote(file)} 2>/dev/null")
      ret = list.map(string.split(separator="="),ret)
      # Could be made better..
      def f(l',l)=
        if list.length(l) >= 2 then
          list.append([(list.hd(l),list.nth(l,1))],l')
        else
          if list.length(l) >= 1 then
            list.append([(list.hd(l),"")],l')
          else
            l'
          end
        end
      end
      list.fold(f,[],ret)
    end
  end
  add_metadata_resolver("FLAC",flac_meta)
else
  log(level=3,"metaflac binary not found: flac metadata resolver disabled.")
end
if test_process("which faad") then
  def faad_p(file)=
    "faad -o /dev/stdout #{quote(file)} 2>/dev/null"
  end
  add_decoder("FAAD",faad_p)
  def faad_meta(~format,file)
    if format != "FAAD" then
      []
    else
      ret = get_process_lines("faad -i \
                   #{quote(file)} 2>&1")
      # Yea, this is tuff programming (again) !
      def get_meta(l,s)=
        ret = string.extract(pattern="^(\w+):\s(.+)$",s)
        if list.length(ret) > 0 then
          list.append([(ret["1"],ret["2"])],l)
        else
          l
        end
      end
      list.fold(get_meta,[],ret)
    end
  end
  add_metadata_resolver("FAAD",faad_meta)
else
  log(level=3,"faad binary not found: faad decoder disabled.")
end
# Enable mplayer decoder (needs mplayer binary in path)
# @category Liquidsoap
def enable_mplayer()=
  def mplayer_p(file)=
    "mplayer -really-quiet -ao pcm:file=/dev/stdout \
           -vc null -vo null #{quote(file)} 2>/dev/null"
  end
  if test_process("which mplayer") then
    add_decoder("MPLAYER",mplayer_p)
  else
    log(label="utils.liq",level=2,"couldn't enable mplayer \
          decoder: no binary found")
  end
end

# Stream data from mplayer
# @category Source / Input
# @param s data URI.
# @param ~restart restart on exit.
# @param ~restart_on_error restart on exit with error.
# @param ~buffer Duration of the pre-buffered data.
# @param ~max Maximum duration of the buffered data.
def input.mplayer(~id="input.mplayer",
         ~restart=true,~restart_on_error=false,
         ~buffer=0.2,~max=10.,s) = 
  input.external(id=id,restart=restart,
                 restart_on_error=restart_on_error,
                 buffer=buffer,max=max,
                 "mplayer -really-quiet -ao pcm:file=/dev/stdout \
                    -vc null -vo null #{quote(s)} 2>/dev/null")
end

# Get the base name of a path.
# Implemented using the corresponding shell command.
# @category System
# @param s Path
def basename(s)
  lines = get_process_lines("basename #{quote(s)}")
  if list.length(lines) > 0 then
    list.hd(lines)
  else
    # Don't know what to do.. output s
    s
  end
end

# Get the directory name of a path.
# Implemented using the corresponding shell command.
# @category System
# @param s Path
def dirname(s)
  lines = get_process_lines("dirname #{quote(s)}")
  if list.length(lines) > 0 then
    list.hd(lines)
  else
    # ?
    s
  end
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
# @param ~default      Default transition when no rule can apply (default: sequence)
# @param ~high         Value, in dB, for loud sound level
# @param ~medium       Value, in dB, for medium sound level
# @param ~margin       Margin to detect sources that have too different sound level
# @param s             The input source.
def smart_crossfade (~start_next=5.,~fade_in=3.,~fade_out=3.,
                     ~default=(fun (a,b) -> sequence([a, b])),
                     ~high=-15., ~medium=-32., ~margin=4.,
                     ~width=2.,~conservative=false,s)
  fade.out = fade.out(type="sin",duration=fade_out)
  fade.in  = fade.in(type="sin",duration=fade_in)
  add = fun (a,b) -> add(normalize=false,[b, a])
  log = log(label="smart_crossfade")

  def transition(a,b,ma,mb,sa,sb)

    list.iter(fun(x)-> log(level=4,"Before: #{x}"),ma)
    list.iter(fun(x)-> log(level=4,"After : #{x}"),mb)

    if
      # If A and B are not too loud and close, fully cross-fade them.
      a <= medium and b <= medium and abs(a - b) <= margin
    then
      log("Old <= medium, new <= medium and |old-new| <= margin.")
      log("Old and new source are not too loud and close.")
      log("Transition: crossed, fade-in, fade-out.")
      add(fade.out(sa),fade.in(sb))

    elsif
      # If B is significantly louder than A, only fade-out A.
      # We don't want to fade almost silent things, ask for >medium.
      b >= a + margin and a >= medium and b <= high
    then
      log("new >= old + margin, old >= medium and new <= high.")
      log("New source is significantly louder than old one.")
      log("Transition: crossed, fade-out.")
      add(fade.out(sa),sb)

    elsif
      # Opposite as the previous one.
      a >= b + margin and b >= medium and a <= high
    then
      log("old >= new + margin, new >= medium and old <= high")
      log("Old source is significantly louder than new one.")
      log("Transition: crossed, fade-in.")
      add(sa,fade.in(sb))

    elsif
      # Do not fade if it's already very low.
      b >= a + margin and a <= medium and b <= high
    then
      log("new >= old + margin, old <= medium and new <= high.")
      log("Do not fade if it's already very low.")
      log("Transition: crossed, no fade.")
      add(sa,sb)

    # What to do with a loud end and a quiet beginning ?
    # A good idea is to use a jingle to separate the two tracks,
    # but that's another story.

    else
      # Otherwise, A and B are just too loud to overlap nicely,
      # or the difference between them is too large and overlapping would
      # completely mask one of them.
      log("No transition: using default.")
      default(sa, sb)
    end
  end

  smart_cross(width=width, duration=start_next, conservative=conservative,
              transition,s)
end

# Custom playlist source written using the script language.
# Will read directory or playlist, play all files and stop
# @category Source / Input
# @param ~random Randomize playlist content
# @param ~on_done Function to execute when the playlist is finished
# @param uri Playlist URI
def playlist.once(~random=false,~on_done={()},uri)
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
            on_done ()
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

# Execute a given action when a predicate is true. This will be run in background.
# @category System
# @param ~freq Frequency for checking the predicate, in seconds.
# @param ~pred Predicate, typically a time interval such as <code>{10h-10h30}</code>.
# @param f Function to execute when the predicate is true.
def exec_at(~freq=1.,~pred,f)
  def check() = 
    if pred() then 
      f()
    end
    freq
  end
  add_timeout(freq,check)
end
