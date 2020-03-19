# Deprecated APIs.

# Mark a function as deprecated.
# @flag hidden
# @category Liquidsoap
# @param old Old function name.
# @param new New function name.
def deprecated(old, new)
  new = if new == "" then "" else " Please use \"#{new}\" instead." end
  log.severe(label="lang.deprecated", "WARNING: \"#{old}\" is deprecated and will be removed in future version.#{new}")
end

# @flag hidden
def on_change_harbor_bind_addr(v) =
  deprecated("harbor.bind_addr", "harbor.bind_addrs")
  set("harbor.bind_addrs",[v])
end

register(name="[DEPRECATED] Harbor bind_addr",
         descr="IP addresses on which the harbor should listen.",
         on_change=on_change_harbor_bind_addr,"harbor.bind_addr","0.0.0.0")

# Deprecated: this function has been replaced by `input.external.rawaudio`.
# @flag hidden
def input.external(~id="",~buffer=2.0,~channels=2,~max=10.0,
                   ~restart=true,~restart_on_error=false,
                   ~samplerate=44100,cmd) =
  deprecated("input.external", "input.external.rawaudio")
  input.external.rawaudio(id=id,buffer=buffer,channels=channels,
                          max=max,restart=restart,restart_on_error=restart_on_error,
                          samplerate=samplerate,cmd)
end

# Deprecated: this function has been replaced by `quote`.
# @flag hidden
def quote(s) =
  deprecated("quote", "string.quote")
  string.quote(s)
end

# Deprecated: use mksafe and playlist instead.
# @flag hidden
def playlist.safe(~id="", ~mime_type="", ~mode="randomize", ~on_track={()}, ~prefix="", ~reload=0, ~reload_mode="seconds", uri)
  deprecated("playlist.safe", "")
  mksafe(playlist(id=id, mime_type=mime_type, mode=mode, prefix=prefix, reload=reload, reload_mode=reload_mode, uri))
end

# Deprecated: this function has been replaced by `thread.run.recurrent`.
# @flag hidden
def add_timeout(~fast=true, delay, f)
  deprecated("add_timeout", "thread.run.recurrent")
  thread.run.recurrent(fast=fast, delay=delay, f)
end

# Deprecated: this function has been replaced by `thread.when`.
# @flag hidden
def exec_at(~freq=1., ~pred, f)
  deprecated("exec_at", "thread.when")
  thread.when(every=freq, pred, f)
end

# Deprecated: this function has been replaced by `thread.mutexify`.
# @flag hidden
def mutexify(f)
  deprecated("mutexify", "thread.mutexify")
  thread.mutexify(f)
end

# Deprecated: this function has been replaced by `file.which`.
# @flag hidden
def which(f)
  deprecated("which", "file.which")
  file.which(f)
end

# Deprecated: this function has been replaced by `string.base64.decode`.
# @flag hidden
def base64.decode(s)
  deprecated("base64.decode", "string.base64.decode")
  string.base64.decode(s)
end

# Deprecated: this function has been replaced by `string.base64.encode`.
# @flag hidden
def base64.encode(s)
  deprecated("base64.encode", "string.base64.encode")
  string.base64.encode(s)
end

# Deprecated: this function has been replaced with `playlist`, setting
# `reload_mode` argument to `"never"` and `loop` to `false`.
def playlist.once(~id="",~random=false,~reload_mode="",
                  ~default_duration=30.,~length=10.,~conservative=false,
                  ~filter=fun(_)->true,uri)
  deprecated("playlist.once", "playlist")
  mode = if random then "randomize" else "normal" end
  playlist(reload_mode="never", loop=false, id=id, mode=mode, conservative=conservative, default_duration=default_duration, check_next=filter, uri)
end

# Deprecated: this function has been replaced by `map_metadata`.
# @flag hidden
def rewrite_metadata(l,~insert_missing=true,~update=true,~strip=false,s)
  deprecated("rewrite_metadata", "map_metadata")
  def map(m)
    def apply(x)
      label = fst(x)
      value = snd(x)
      (label,value % m)
    end
    list.map(apply,l)
  end
  map_metadata(map,insert_missing=insert_missing,update=update,strip=strip,s)
end

# Deprecated: this function will be removed in a future release
# @flag hidden
def id(~id="",s)
 deprecated("id","")
 s
end 

# Deprecated: flow is no longer maintained
# Register a radio on Liquidsoap Flows.
# @category Liquidsoap
# @flag hidden
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
  deprecated("register_flow", "")

  # If the server is "", we get the server from sf.net
  server =
    if server == "" then
      let ((_,code,_),_,data) = http.get("http://liquidsoap.info/flows_server")
      if code == 200 then
        data
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
      let (label, value) = x
      log(level=4,"  #{label}: #{value}")
    end
    list.iter(log_arg,params)

    cmd = url.encode(cmd)
    params = list.append(base_params,params)
    def f(z) =
      let (x, y) = z
      y = url.encode(y)
      "#{x}=#{y}"
    end
    params = string.concat(separator="&",list.map(f,params))
    url = "#{server}?cmd=#{cmd}&#{params}"

    # TODO: do something with errors!
    answer = http.get(url)
    let ((protocol, code, desc), headers, data) = answer
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
  thread.run.recurrent(fast=false, delay=ping_period, ping)

  # Register streams
  def register_stream(format_url)
    let (format, url) = format_url
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
    thread.run(fast=false, {request(cmd="metadata",params=params)})
  end
  on_metadata(metadata,s)
end
