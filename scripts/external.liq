
# Output the stream using aplay.
# Using this turns "root.sync" to false
# since aplay will do the synchronisation
# @category Source / Output
# @param ~id Output's ID
# @param ~device Alsa pcm device name
# @param ~restart_on_crash Restart external process on crash. If false, liquidsoap will stop.
# @param ~fallible Allow the child source to fail, in which case the output will be (temporarily) stopped.
# @param ~on_start Callback executed when outputting starts.
# @param ~on_stop Callback executed when outputting stops.
# @param s Source to play
def output.aplay(~id="output.aplay",~device="default",
                 ~fallible=false,~on_start={()},~on_stop={()},
                 ~restart_on_crash=false,s)
  def aplay_p(m) = 
    "aplay -D #{device}"
  end
  log(label=id,level=3,"Setting root.sync to false")
  set("root.sync",false)
  output.pipe.external(id=id,
                       fallible=fallible,on_start=on_start,on_stop=on_stop,
                       restart_on_crash=restart_on_crash,
                       restart_on_new_track=false,
                       process=aplay_p,s)
end

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
# @param ~swap Swap audio samples. Depends on local machine's endianess and lame's version. Test this parameter if you experience garbaged mp3 audio data. On intel 32 and 64 architectures, the parameter should be "true" for lame version >= 3.98.
# @param ~dumpfile Dump stream to file, for debugging purpose. Disabled if empty.
# @param ~protocol Protocol of the streaming server: 'http' for Icecast, 'icy' for Shoutcast.
# @param ~fallible Allow the child source to fail, in which case the output will be (temporarily) stopped.
# @param ~on_start Callback executed when outputting starts.
# @param ~on_stop Callback executed when outputting stops.
# @param s The source to output
def output.icecast.lame(
  ~id="output.icecast.lame",~start=true,
  ~restart=false,~restart_delay=3,
  ~host="localhost",~port=8000,
  ~user="source",~password="hackme",
  ~genre="Misc",~url="http://savonet.sf.net/",
  ~description="OCaml Radio!",~public=true,
  ~dumpfile="",~mount="Use [name]",
  ~name="Use [mount]",~protocol="http",
  ~lame="lame",~bitrate=128,~swap=false,
  ~fallible=false,~on_start={()},~on_stop={()},
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
    fallible=fallible,on_start=on_start,on_stop=on_stop,
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
# @param ~icy_reset Reset shoutcast source buffer upon connecting (necessary for NSV).
# @param ~dumpfile Dump stream to file, for debugging purpose. Disabled if empty.
# @param ~fallible Allow the child source to fail, in which case the output will be (temporarily) stopped.
# @param ~on_start Callback executed when outputting starts.
# @param ~on_stop Callback executed when outputting stops.
# @param s The source to output
def output.shoutcast.lame(
  ~id="output.shoutcast.mp3",~start=true,
  ~restart=false,~restart_delay=3,
  ~host="localhost",~port=8000,
  ~user="source",~password="hackme",
  ~genre="Misc",~url="http://savonet.sf.net/",
  ~description="OCaml Radio!",~public=true,
  ~dumpfile="",~name="Use [mount]",~icy_reset=true,
  ~lame="lame",~aim="",~icq="",~irc="",
  ~fallible=false,~on_start={()},~on_stop={()},
  ~restart_on_crash=false,~restart_on_new_track=false,
  ~restart_encoder_delay=3600,~bitrate=128,s) =
  icy_reset = if icy_reset then "1" else "0" end
  headers = [("icy-aim",aim),("icy-irc",irc),
             ("icy-icq",icq),("icy-reset",icy_reset)]
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
    fallible=fallible,on_start=on_start,on_stop=on_stop,
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
# @param ~fallible Allow the child source to fail, in which case the output will be (temporarily) stopped.
# @param ~on_start Callback executed when outputting starts.
# @param ~on_stop Callback executed when outputting stops.
# @param s The source to output
def output.icecast.flac(
  ~id="output.icecast.flac",~start=true,
  ~restart=false,~restart_delay=3,
  ~host="localhost",~port=8000,
  ~user="source",~password="hackme",
  ~genre="Misc",~url="http://savonet.sf.net/",
  ~description="OCaml Radio!",~public=true,
  ~dumpfile="",~mount="Use [name]",
  ~name="Use [mount]",~protocol="http",
  ~flac="flac",~quality=6,
  ~restart_on_crash=false,
  ~restart_on_new_track=true,
  ~restart_encoder_delay=(-1),
  ~fallible=false,~on_start={()},~on_stop={()},
  s)
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
    fallible=fallible,on_start=on_start,on_stop=on_stop,
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
# @param ~fallible Allow the child source to fail, in which case the output will be (temporarily) stopped.
# @param ~on_start Callback executed when outputting starts.
# @param ~on_stop Callback executed when outputting stops.
# @param s The source to output
def output.icecast.aacplusenc(
  ~id="output.icecast.aacplusenc",~start=true,
  ~restart=false,~restart_delay=3,
  ~host="localhost",~port=8000,
  ~user="source",~password="hackme",
  ~genre="Misc",~url="http://savonet.sf.net/",
  ~description="OCaml Radio!",~public=true,
  ~dumpfile="",~mount="Use [name]",
  ~name="Use [mount]",~protocol="http",
  ~aacplusenc="aacplusenc",~bitrate=64,
  ~fallible=false,~on_start={()},~on_stop={()},
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
    fallible=fallible,on_start=on_start,on_stop=on_stop,
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
# @param ~icy_reset Reset shoutcast source buffer upon connecting (necessary for NSV).
# @param ~dumpfile Dump stream to file, for debugging purpose. Disabled if empty.
# @param ~fallible Allow the child source to fail, in which case the output will be (temporarily) stopped.
# @param ~on_start Callback executed when outputting starts.
# @param ~on_stop Callback executed when outputting stops.
# @param s The source to output
def output.shoutcast.aacplusenc(
  ~id="output.shoutcast.aacplusenc",~start=true,
  ~restart=false,~restart_delay=3,
  ~host="localhost",~port=8000,
  ~user="source",~password="hackme",
  ~genre="Misc",~url="http://savonet.sf.net/",
  ~description="OCaml Radio!",~public=true,
  ~fallible=false,~on_start={()},~on_stop={()},
  ~dumpfile="",~name="Use [mount]",~icy_reset=true,
  ~aim="",~icq="",~irc="",~aacplusenc="aacplusenc",
  ~restart_on_crash=false,~restart_on_new_track=false,
  ~restart_encoder_delay=3600,~bitrate=64,s) =
  icy_reset = if icy_reset then "1" else "0" end
  headers = [("icy-aim",aim),("icy-irc",irc),
             ("icy-icq",icq),("icy-reset",icy_reset)]
  output.icecast.aacplusenc(
    id=id, headers=headers, aacplusenc=aacplusenc,
    bitrate=bitrate, start=start,
    restart=restart, restart_delay=restart_delay,
    host=host, port=port, user=user, password=password,
    genre=genre, url=url, description=description,
    public=public, dumpfile=dumpfile,
    fallible=fallible,on_start=on_start,on_stop=on_stop,
    restart_on_crash=restart_on_crash, restart_encoder_delay=restart_encoder_delay,
    restart_on_new_track=restart_on_new_track,
    name=name, mount="/", protocol="icy",
    s)
end
%endif

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

# Enable faad decoding (needs faad binary in path)
# @category Liquidsoap
def enable_faad() =
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
end

# Enable mplayer decoder (needs mplayer binary in path)
# @category Liquidsoap
def enable_mplayer(~debug=false)=
  def mplayer_p(file)=
    s = "mplayer -really-quiet -ao pcm:file=/dev/stdout \
           -vc null -vo null #{quote(file)}"
    if debug then
      log(label="utils.liq",level=2,"Mplayer external decoder called: \n#{s}")
      s
    else
      "#{s} 2>/dev/null"
    end
  end
  if test_process("which mplayer") then
    add_decoder("MPLAYER",mplayer_p)
  else
    log(label="utils.liq",level=2,"couldn't enable mplayer \
          decoder: no binary found")
  end
end

# Enable gstreamer decoder (needs gst-launch binary in path)
# @category Liquidsoap
def enable_gstreamer(~debug=false)=
  def gstreamer_p(file)=
    s = "gst-launch filesrc location=#{quote(file)} ! \
           decodebin ! wavenc ! fdsink fd=1"
    if debug then
      log(label="utils.liq",level=2,"Gstreamer external decoder called: \n#{s}")
      s
    else
      "#{s} 2>/dev/null"
    end
  end
  if test_process("which gst-launch") then
    add_decoder("GSTREAMER",gstreamer_p)
  else
    log(label="utils.liq",level=2,"couldn't enable gstreamer \
          decoder: no gst-launch binary found")
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

# Enable replay gain metadata resolver. This resolver will
# process any file decoded by liquidsoap and add a @replay_gain@
# metadata when this value could be computed. For a finer-grained
# replay gain processing, use the @replay_gain@ protocol.
# @category Liquidsoap
# @param ~extract_replaygain The extraction program
def enable_replaygain_metadata(
       ~extract_replaygain="#{configure.libdir}/extract-replaygain")
  def replaygain_metadata(~format,file)
    x = get_process_lines("#{extract_replaygain} \
                              #{quote(file)}")
    if list.hd(x) != "" then
      [("replay_gain",list.hd(x))]
    else
      []
    end
  end
  add_metadata_resolver("replay_gain", replaygain_metadata)
end
