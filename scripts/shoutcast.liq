
%ifdef output.icecast
# Output to shoutcast.
# @category Source / Output
# @param ~id Output's ID
# @param ~start Start output threads on operator initialization.
# @param ~user User for shout source connection. Useful only in special cases, like with per-mountpoint users.
# @param ~icy_reset Reset shoutcast source buffer upon connecting (necessary for NSV).
# @param ~dumpfile Dump stream to file, for debugging purpose. Disabled if empty.
# @param ~fallible Allow the child source to fail, in which case the output will be (temporarily) stopped.
# @param ~on_start Callback executed when outputting starts.
# @param ~on_stop Callback executed when outputting stops.
# @param ~on_error Callback executed when an error happens. If returned value is positive, connection wll be tried again after this amount of time (in seconds).
# @param ~on_connect Callback executed when connection starts.
# @param ~on_disconnect Callback executed when connection stops.
# @param ~icy_metadata Send new metadata using the ICY protocol. One of: "guess", "true", "false"
# @param ~format Format, e.g. "audio/ogg". When empty, the encoder is used to guess.
# @param e Endoding format. For shoutcast, should be mp3 or AAC(+).
# @param s The source to output
def output.shoutcast(
  ~id="output.shoutcast",~start=true,
  ~host="localhost",~port=8000,
  ~user="source",~password="hackme",
  ~genre="Misc",~url="http://savonet.sf.net/",
  ~name="Liquidsoap Radio!",~public=true, ~format="",
  ~dumpfile="", ~icy_metadata="guess",
  ~on_connect={()}, ~on_disconnect={()},
  ~aim="",~icq="",~irc="",~icy_reset=true,
  ~fallible=false,~on_start={()},~on_stop={()},
  ~on_error=fun(_)->3., e,s) = 
  icy_reset = if icy_reset then "1" else "0" end
  headers = [("icy-aim",aim),("icy-irc",irc),
             ("icy-icq",icq),("icy-reset",icy_reset)]
  output.icecast(
    e, format=format,
    id=id, headers=headers,
    start=start,icy_metadata=icy_metadata,
    on_connect=on_connect, on_disconnect=on_disconnect,
    host=host, port=port, user=user, password=password,
    genre=genre, url=url, description="UNUSED",
    public=public, dumpfile=dumpfile,encoding="ISO-8859-1",
    name=name, mount="/", protocol="icy",on_error=on_error,
    fallible=fallible,on_start=on_start,on_stop=on_stop,
    s)
end
%endif
