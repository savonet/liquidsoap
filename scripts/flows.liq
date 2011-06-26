# BIG TODO:
# - Check for errors
# - Unregister radio and streams

# Register a radio on Liquidsoap Flows.
# @category Liquidsoap
# @param ~radio   Name of the radio.
# @param ~website URL of the website of the radio.
# @param ~description Description of the radio.
# @param ~genre   Genre of the radio (rock or rap or etc.).
# @param ~streams List of streams for the radio described by \
#                 a pair of strings consisting of the format of the stream \
#                 and the url of the stream. The format should be \
#                 of the form "ogg/128k" consisting of the codec and \
#                 the bitrate, separated by "/".
def register_flow(~server="",~user="default",~password="default",
                  ~email="",~radio,~website,~description,~genre,
                  ~streams,s)

  # If the server is "", we get the server from sf.net
  server =
    if server == "" then
      server = http.get("http://savonet.sourceforge.net/flows_server")
      html_status = snd(fst(fst(fst(server))))
      if html_status == 200 then
        snd(server)
      else
        # If sf is down, we use the hardcoded server
        "http://savonet.rastageeks.org/liqflows.py"
      end
    else
      server
    end
  log(level=4,"Flows server: #{server}")

  # Initial variables
  ping_period = 600. # Pinging period in seconds

  # Fix default parameters
  # and set request function.
  base_params = [("v", "0.0"),
                 ("user",user),
                 ("password",password),
                 ("email",email),
                 ("radio",radio)]
  def request(~cmd,~params) =
    log = log(label=radio)
    log(level=4,"Processing command #{cmd} with arguments:")
    def log_arg(x) =
      label = fst(x)
      value = snd(x)
      log(level=4,"  #{label}: #{value}")
    end
    list.iter(log_arg,params)

    cmd = url.encode(cmd)
    params = list.append(base_params,params)
    def f(z) =
      x = fst(z)
      y = url.encode(snd(z))
      "#{x}=#{y}"
    end
    params = string.concat(separator="&",list.map(f,params))
    url = "#{server}?cmd=#{cmd}&#{params}"

    # TODO: do something with errors!
    answer = http.get(url)
    x = fst(answer)
    status = fst(x)
    y = fst(status)
    protocol = fst(y)
    code = snd(y)
    desc = snd(status)
    headers = snd(x)
    data = snd(answer)
    log(level=4,"Response status: #{protocol} #{code} #{desc}")
    log(level=4,"Response headers:")
    list.iter(log_arg,headers)
    log(level=4,"Response content: #{data}")
  end

  # Register radio
  params = [("radio_website",website),
            ("radio_description",description),
            ("radio_genre",genre)]
  request(cmd="add radio",params=params)

  # Ping
  def ping() =
    ignore(request(cmd="ping radio",params=[]))
    ping_period
  end
  add_timeout(fast=false,ping_period,ping)

  # Register streams
  def register_stream(format_url)
    format = fst(format_url);
    url = snd(format_url);
    params = [("stream_format",format),("stream_url",url)]
    request(cmd="add stream",params=params)
  end
  request(cmd="clear streams",params=[])
  list.iter(register_stream,streams)

  # Metadata update
  def metadata(m) =
    artist = m["artist"]
    title = m["title"]
    params = [("m_title",title),("m_artist",artist)]
    request(cmd="metadata",params=params)
  end
  on_metadata(metadata,s)
end
