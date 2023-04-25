%ifdef output.icecast
# Encode and output the stream to a shoutcast server.
# @category Source / Output
# @argsof output.icecast[!method,!mount,!description,!protocol]
# @param ~icy_reset Reset shoutcast source buffer upon connecting (necessary for NSV).
# @param ~dj Callback to set dj name.
# @param e Encoding format. Should be mp3 or AAC(+).
# @param s The source to output
def output.shoutcast(
  %argsof(
   output.icecast[!method,!mount,!description,!protocol]
  ),
  ~icy_reset=true,~dj={""},
  ~aim="",~icq="",~irc="",
  e, s) =
  icy_reset = if icy_reset then "1" else "0" end

  headers = [("icy-aim",aim),("icy-irc",irc),
             ("icy-icq",icq),("icy-reset",icy_reset),
             ...headers]

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
     output.icecast[!method,!mount,!headers,!description,!protocol]
    ),
    mount="",
    headers=headers,
    protocol="icy",
    e, s)
end

# Encode and output the stream to an icecast server.
# @category Source / Output
# @argsof output.icecast[!protocol, !icy_id]
# @param e Encoding format.
# @param s The source to output
def output.icecast(%argsof(output.icecast[!protocol, !icy_id]), e, s) =
  output.icecast(
    %argsof(output.icecast[!protocol, !icy_id]),
    protocol="http",
    e, s)
end
%endif
