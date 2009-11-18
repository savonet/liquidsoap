
%ifdef output.icecast.mp3
# Output to shoutcast using the MP3 format.
# @category Source / Output
# @param ~id Output's ID
# @param ~start Start output threads on operator initialization.
# @param ~restart Restart output after a failure. By default, liquidsoap will stop if the output failed.
# @param ~restart_delay Delay, in seconds, before attempting new connection, if restart is enabled.
# @param ~user User for shout source connection. Useful only in special cases, like with per-mountpoint users.
# @param ~bitrate Encoder bitrate
# @param ~icy_reset Reset shoutcast source buffer upon connecting (necessary for NSV).
# @param ~dumpfile Dump stream to file, for debugging purpose. Disabled if empty.
# @param ~fallible Allow the child source to fail, in which case the output will be (temporarily) stopped.
# @param ~on_start Callback executed when outputting starts.
# @param ~on_stop Callback executed when outputting stops.
# @param s The source to output
def output.shoutcast.mp3(
  ~id="output.shoutcast.mp3",~start=true,
  ~restart=false,~restart_delay=3,
  ~host="localhost",~port=8000,
  ~user="source",~password="hackme",
  ~genre="Misc",~url="http://savonet.sf.net/",
  ~description="OCaml Radio!",~public=true,
  ~dumpfile="",~name="Use [mount]",
  ~aim="",~icq="",~irc="",~icy_reset=true,
  ~bitrate=128,~samplerate=44100,~stereo=true,
  ~fallible=false,~on_start={()},~on_stop={()},
  s) = 
  icy_reset = if icy_reset then "1" else "0" end
  headers = [("icy-aim",aim),("icy-irc",irc),
             ("icy-icq",icq),("icy-reset",icy_reset)]
  output.icecast.mp3(
    id=id, headers=headers,
    bitrate=bitrate, start=start,
    restart=restart, restart_delay=restart_delay,
    host=host, port=port, user=user, password=password,
    genre=genre, url=url, description=description,
    public=public, dumpfile=dumpfile,
    name=name, mount="/", protocol="icy",
    samplerate=samplerate,stereo=stereo,
    fallible=fallible,on_start=on_start,on_stop=on_stop,
    s)
end
%endif

%ifdef output.icecast.aacplus
# Output to shoutcast using the AAC+ format.
# @category Source / Output
# @param ~id Output's ID
# @param ~start Start output threads on operator initialization.
# @param ~restart Restart output after a failure. By default, liquidsoap will stop if the output failed.
# @param ~restart_delay Delay, in seconds, before attempting new connection, if restart is enabled.
# @param ~user User for shout source connection. Useful only in special cases, like with per-mountpoint users.
# @param ~bitrate Encoder bitrate
# @param ~icy_reset Reset shoutcast source buffer upon connecting (necessary for NSV).
# @param ~dumpfile Dump stream to file, for debugging purpose. Disabled if empty.
# @param ~fallible Allow the child source to fail, in which case the output will be (temporarily) stopped.
# @param ~on_start Callback executed when outputting starts.
# @param ~on_stop Callback executed when outputting stops.
# @param s The source to output
def output.shoutcast.aacplus(
  ~id="output.shoutcast.aacplus",~start=true,
  ~restart=false,~restart_delay=3,
  ~host="localhost",~port=8000,
  ~user="source",~password="hackme",
  ~genre="Misc",~url="http://savonet.sf.net/",
  ~description="OCaml Radio!",~public=true,
  ~dumpfile="",~name="Use [mount]",
  ~aim="",~icq="",~irc="",~icy_reset=true,
  ~bitrate=64,~samplerate=44100,
  ~fallible=false,~on_start={()},~on_stop={()},
  ~format="audio/aacp",s) =
  icy_reset = if icy_reset then "1" else "0" end
  headers = [("icy-aim",aim),("icy-irc",irc),
             ("icy-icq",icq),("icy-reset",icy_reset)]
  output.icecast.aacplus(
    id=id, headers=headers,
    bitrate=bitrate, start=start,
    restart=restart, restart_delay=restart_delay,
    host=host, port=port, user=user, password=password,
    genre=genre, url=url, description=description,
    public=public, dumpfile=dumpfile,
    name=name, mount="/", protocol="icy",
    samplerate=samplerate,format=format,
    fallible=fallible,on_start=on_start,on_stop=on_stop,
    s)
end
%endif
