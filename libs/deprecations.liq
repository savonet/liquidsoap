# Deprecated APIs.

# Mark a function as deprecated.
# @flag deprecated
# @category Liquidsoap
# @param old Old function name.
# @param new New function name.
def deprecated(old, new)
  new = if new == "" then "" else " Please use \"#{new}\" instead." end
  log.severe(label="lang.deprecated", "WARNING: \"#{old}\" is deprecated and will be removed in future version.#{new}")
end

# Deprecated: this function has been replaced by `input.external.rawaudio`.
# @flag deprecated
def replaces input.external(%argsof(input.external.rawaudio), cmd)
  deprecated("input.external", "input.external.rawaudio")
  input.external.rawaudio(%argsof(input.external.rawaudio), cmd)
end

# Deprecated: this function has been replaced by `string.quote`.
# @flag deprecated
def quote(s) =
  deprecated("quote", "string.quote")
  string.quote(s)
end

# Deprecated: this function has been replaced by `string.quote`.
# @flag deprecated
def process.quote(s) =
  deprecated("process.quote", "string.quote")
  string.quote(s)
end

# Deprecated: use mksafe and playlist instead.
# @flag deprecated
def playlist.safe(~id=null(), ~mime_type="", ~mode="randomize", ~on_track={()}, ~prefix="", ~reload=0, ~reload_mode="seconds", uri)
  deprecated("playlist.safe", "")
  mksafe(playlist(id=id, mime_type=mime_type, mode=mode, prefix=prefix, reload=reload, reload_mode=reload_mode, uri))
end

# Deprecated: this function has been replaced by `thread.run.recurrent`.
# @flag deprecated
def add_timeout(~fast=true, delay, f)
  deprecated("add_timeout", "thread.run.recurrent")
  thread.run.recurrent(fast=fast, delay=delay, f)
end

# Deprecated: this function has been replaced by `thread.when`.
# @flag deprecated
def exec_at(~freq=1., ~pred, f)
  deprecated("exec_at", "thread.when")
  thread.when(every=freq, pred, f)
end

# Deprecated: this function has been replaced by `thread.mutexify`.
# @flag deprecated
def mutexify(f)
  deprecated("mutexify", "thread.mutexify")
  thread.mutexify(f)
end

# Deprecated: this function has been replaced by `file.which`.
# @flag deprecated
def which(f)
  deprecated("which", "file.which")
  file.which(f) ?? ""
end

base64 = ()

# Deprecated: this function has been replaced by `string.base64.decode`.
# @flag deprecated
def base64.decode(s)
  deprecated("base64.decode", "string.base64.decode")
  string.base64.decode(s)
end

# Deprecated: this function has been replaced by `string.base64.encode`.
# @flag deprecated
def base64.encode(s)
  deprecated("base64.encode", "string.base64.encode")
  string.base64.encode(s)
end

# Deprecated: this function has been replaced with `playlist`, setting
# `reload_mode` argument to `"never"` and `loop` to `false`.
def playlist.once(~id=null(),~random=false,~reload_mode="",
                  ~default_duration=30.,~length=10.,~conservative=false,
                  ~filter=fun(_)->true,uri)
  deprecated("playlist.once", "playlist")
  mode = if random then "randomize" else "normal" end
  playlist(reload_mode="never", loop=false, id=id, mode=mode, conservative=conservative, default_duration=default_duration, check_next=filter, uri)
end

# Deprecated: this function has been replaced by `map_metadata`.
# @flag deprecated
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
# @flag deprecated
def id(~id=null(),s)
 deprecated("id","")
 s
end 

%ifdef http.get
# Deprecated: flow is no longer maintained
# Register a radio on Liquidsoap Flows.
# @category Liquidsoap
# @flag deprecated
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
      let data = http.get("http://liquidsoap.info/flows_server")
      if data.status_code == 200 then
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
    log(level=4,"Response status: #{answer.protocol_version} #{answer.status_code} #{answer.status_message}")
    log(level=4,"Response headers:")
    list.iter(log_arg, answer.headers)
    log(level=4,"Response content: #{answer}")
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
  s.on_metadata(metadata)
end
%endif

# Deprecated: this function has been replaced by `source.fail`.
# @flag deprecated
def empty(~id=null())
  deprecated("empty", "source.fail")
  source.fail()
end

# Deprecated: use `request.create` instead.
# @flag deprecated
def request.create.raw(%argsof(request.create), uri)
  deprecated("request.create.raw", "request.create")
  request.create(%argsof(request.create), uri)
end

# Deprecated: use `file.remove` instead.
# @flag deprecated
def file.unlink(filename)
  deprecated("file.unlink", "file.remove")
  file.remove(filename)
end

# Deprecated: was designed for transitions only and is not
# needed anymore. Use `file.out` instead.
# @flag deprecated
def fade.final(~id="fade.final",~duration=3.,~type="lin",s) =
  deprecated("fade.final", "fade.out")

  fn = mkfade(start=1.,stop=0.,type=type,duration=duration,s)
  should_play = ref(true)

  def fn() =
    v = fn()
    if v == 0. then
      should_play := false
    end
    v
  end

  s = fade.scale(id=id,fn,s)
  switch(track_sensitive=false,[({!should_play},s)])
end

# Deprecated: was designed for transitions only and is not
# needed anymore. Use `fade.in` instead.
# @flag deprecated
def fade.initial(~id="fade.initial",~duration=3.,~type="lin",s) =
  deprecated("fade.initial", "fade.in")  

  fn = mkfade(start=0.,stop=1.,type=type,duration=duration,s)
  fade.scale(id=id,fn,s)
end

# Deprecated: use `process.read` instead.
# @flag deprecated
def get_process_output(%argsof(process.read), cmd)
  deprecated("get_process_output", "process.read")
  process.read(%argsof(process.read), cmd)
end

# Deprecated: use `process.read.lines` instead.
# @flag deprecated
def get_process_lines(%argsof(process.read.lines), cmd)
  deprecated("get_process_lines", "process.read.lines")
  process.read.lines(%argsof(process.read.lines), cmd)
end

# Deprecated: use `process.test` instead.
# @flag deprecated
def test_process(%argsof(process.test), cmd)
  deprecated("test_process", "process.test")
  process.test(%argsof(process.test), cmd)
end

# Deprecated: use `process.run` instead.
# @flag deprecated
def system(command) =
  deprecated("system", "process.run")
  process.run(command)
end

# Deprecated: use `blank.detect` instead
# @flag deprecated
def on_blank(%argsof(blank.detect), s)
  deprecated("on_blank", "blank.detect")
  blank.detect(%argsof(blank.detect), s)
end

# Deprecated: use `blank.skip` instead
# @flag deprecated
def skip_blank(%argsof(blank.skip), s)
  deprecated("skip_blank", "blank.skip")
  blank.skip(%argsof(blank.skip), s)
end

# Deprecated: use `blank.eat` instead
# @flag deprecated
def eat_blank(%argsof(blank.eat), s)
  deprecated("eat_blank", "blank.eat")
  blank.eat(%argsof(blank.eat), s)
end

# Deprecated: use `blank.strip` instead
# @flag deprecated
def strip_blank(%argsof(blank.strip), s)
  deprecated("strip_blank", "blank.strip")
  blank.strip(%argsof(blank.strip), s)
end

# Deprecated: use `time.local` instead
# @flag deprecated
def localtime(t)
  deprecated("localtime", "time.local")
  time.local(t)
end

# Deprecated: use `time.utc` instead
# @flag deprecated
def gmtime(t)
  deprecated("gmtime", "time.utc")
  time.utc(t)
end

# Deprecated: use `time` instead
# @flag deprecated
def gettimeofday()
  deprecated("gettimeofday", "time")
  time()
end

# @flag deprecated
def output.preferred(~id=null(),~fallible=false,
                    ~on_start={()},~on_stop={()},~start=true,s)
  deprecated("output.preferred","output")
  output(id=id,fallible=fallible,
         start=start,on_start=on_start,on_stop=on_stop,
         s)
end

# Deprecated: use `output` instead.
# @flag deprecated
def out(s)
  deprecated("out","output")
  output(mksafe(s))
end

# Deprecated: use `input` instead.
# @flag deprecated
def in(~id=null(),~start=true,~on_start={()},~on_stop={()},~fallible=false)
  deprecated("in", "input")
  input(id=id,start=start,on_start=on_start,on_stop=on_stop,fallible=fallible)
end

# Deprecated: use `source.available` instead.
# @flag deprecated
def mkavailable(~id="mkavailable", ~track_sensitive=getter(true), ~active=getter(false), ~available=getter(true), s)
  deprecated("mkavailable", "source.available")
  output.dummy(switch([(getter.function(active),s)]),fallible=true)
  switch(id=id,track_sensitive=getter.function(track_sensitive),[(getter.function(available),s)])
end

# Deprecated: use `at` instead.
# @flag deprecated
def at(pred,s)
  deprecated("at", "source.available")
  source.available(s,pred)
end

# Deprecated: use `source.on_end` instead.
# @flag deprecated
def on_end(%argsof(source.on_end), s, f)
  deprecated("on_end", "source.on_end")
  source.on_end(%argsof(source.on_end), s, f)
end

https = ()

%ifdef http.get
# Deprecated: use `http.get` instead.
# @flag deprecated
def https.get(%argsof(http.get), url)
  deprecated("https.get", "http.get")
  http.get(%argsof(http.get), url)
end
%endif

%ifdef http.put
# Deprecated: use `http.put` instead.
# @flag deprecated
def https.put(%argsof(http.put), url)
  deprecated("https.put", "http.put")
  http.put(%argsof(http.put), url)
end
%endif

%ifdef http.post
# Deprecated: use `http.post` instead.
# @flag deprecated
def https.post(%argsof(http.post), url)
  deprecated("https.post", "http.post")
  http.post(%argsof(http.post), url)
end
%endif

%ifdef http.head
# Deprecated: use `http.head` instead.
# @flag deprecated
def https.head(%argsof(http.head), url)
  deprecated("https.head", "http.head")
  http.head(%argsof(http.head), url)
end
%endif

%ifdef http.delete
# Deprecated: use `http.delete` instead.
# @flag deprecated
def https.delete(%argsof(http.delete), url)
  deprecated("https.delete", "http.delete")
  http.delete(%argsof(http.delete), url)
end
%endif

%ifdef input.http
# Deprecated: use `input.http` instead
# @flag deprecated
def input.https(%argsof(input.http), url)
  deprecated("input.https", "input.http")
  input.http(%argsof(input.http), url)
end
%endif

# Deprecated: use `source.say_metadata` instead
# @flag deprecated
def say_metadata(s, ~pattern) =
  def pattern(m) = pattern % m end
  source.say_metadata(pattern=pattern, s)
end

# Deprecated: use `playlist.parse.register` instead
# @flag deprecated
def add_playlist_parser(%argsof(playlist.parse.register), s) =
  deprecated("add_playlist_parser", "playlist.parse.register")
  playlist.parse.register(%argsof(playlist.parse.register), s)
end
