# These operators need to be updated..


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
  ~description="Liquidsoap Radio!",~public=true,
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
  ~description="Liquidsoap Radio!",~public=true,
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
  ~description="Liquidsoap Radio!",~public=true,
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
  ~description="Liquidsoap Radio!",~public=true,
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
  ~description="Liquidsoap Radio!",~public=true,
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

