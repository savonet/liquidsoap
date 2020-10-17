
%ifdef output.icecast
# Output to shoutcast.
# @category Source / Output
# @argsof output.icecast[id,start,user,password,host,port,url,genre,name,public,dumpfile,encoding,fallible,on_start,on_stop,on_error,on_connect,on_disconnect,icy_metadata,format]
# @param ~icy_reset Reset shoutcast source buffer upon connecting (necessary for NSV).
# @param ~icy_id Shoutcast source ID. Only supported by Shoutcast v2.
# @param ~dj Callback to set dj name.
# @param e Encoding format. For shoutcast, should be mp3 or AAC(+).
# @param s The source to output
def output.shoutcast(
  %argsof(
   output.icecast[id,start,user,password,host,port,url,genre,public,name,
                  dumpfile,encoding,fallible,on_start,on_stop,on_error,
                  on_connect,on_disconnect,icy_metadata,format]
  ),
  ~icy_reset=true,~icy_id=1,~dj={""},
  ~aim="",~icq="",~irc="",
  e, s) =
  icy_reset = if icy_reset then "1" else "0" end

  headers = [("icy-aim",aim),("icy-irc",irc),
             ("icy-icq",icq),("icy-reset",icy_reset)]

  def map(m) =
    dj = dj()
    if dj != "" then
      list.add(("dj",dj),m)
    else
      m
    end
  end
  s = map_metadata(map,s)

  output.icecast(
    %argsof(
     output.icecast[id,start,user,password,host,port,url,genre,public,name,
                    dumpfile,encoding,fallible,on_start,on_stop,on_error,
                    on_connect,on_disconnect,icy_metadata,format]
    ),
    headers=headers,description="UNUSED",protocol="icy",
    e, s)
end
%endif
