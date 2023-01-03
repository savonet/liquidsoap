%ifdef output.icecast
# Output to shoutcast.
# @category Source / Output
# @argsof output.icecast[!headers,!description,!protocol]
# @param ~icy_reset Reset shoutcast source buffer upon connecting (necessary for NSV).
# @param ~icy_id Shoutcast source ID. Only supported by Shoutcast v2.
# @param ~dj Callback to set dj name.
# @param e Encoding format. For shoutcast, should be mp3 or AAC(+).
# @param s The source to output
def output.shoutcast(
  %argsof(
   output.icecast[!headers,!description,!protocol]
  ),
  ~icy_reset=true,~dj={""},
  ~aim="",~icq="",~irc="",
  e, s) =
  icy_reset = if icy_reset then "1" else "0" end

  headers = [("icy-aim",aim),("icy-irc",irc),
             ("icy-icq",icq),("icy-reset",icy_reset)]

  def map(m) =
    dj = dj()
    if dj != "" then
      ("dj",dj)::m
    else
      m
    end
  end
  s = metadata.map(map,s)

  output.icecast(
    %argsof(
     output.icecast[!headers,!description,!protocol]
    ),
    headers=headers,description="UNUSED",protocol="icy",
    e, s)
end
%endif
