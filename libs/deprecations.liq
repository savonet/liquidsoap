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
def on_metadata(~id="",fn,s) =
  deprecated("on_metadata", "the <source>.on_metadata(..) method")
  s.on_metadata(fn)
  s
end

# @flag hidden
def on_track(~id="",fn,s) =
  deprecated("on_track", "the <source>.on_track(..) method")
  s.on_track(fn)
  s
end

# @flag hidden
def source.on_shutdown(s, fn) =
  deprecated("source.on_shutdown", "the <source>.on_shutdown() method")
  s.on_shutdown(fn)
end

# @flag hidden
def source.is_up(s) =
  deprecated("source.is_up", "the <source>.is_up() method")
  s.is_up()
end

# @flag hidden
def source.is_ready(s) =
  deprecated("source.is_ready", "the <source>.is_ready() method")
  s.is_ready()
end

# @flag hidden
def source.id(s) =
  deprecated("source.id", "the <source>.id() method")
  s.id()
end

# @flag hidden
def source.shutdown(s) =
  deprecated("source.shutdown", "the <source>.shutdown() method")
  s.shutdown()
end

# @flag hidden
def source.seek(s, f) =
  deprecated("source.seek", "the <source>.seek(..) method")
  s.seek(f)
end

# @flag hidden
def source.skip(s) =
  deprecated("source.skip", "the <source>.skip() method")
  s.skip()
end

# @flag hidden
def source.fallible(s) =
  deprecated("source.fallible", "the <source>.fallible() method")
  s.fallible()
end

# @flag hidden
def source.remaining(s) =
  deprecated("source.remaining", "the <source>.remaining() method")
  s.remaining()
end

# @flag hidden
def source.time(s) =
  deprecated("source.time", "the <source>.time() method")
  s.time()
end

# @flag hidden
def on_change_harbor_bind_addr(v) =
  deprecated("harbor.bind_addr", "harbor.bind_addrs")
  set("harbor.bind_addrs",[v])
end

register(name="[DEPRECATED] Harbor bind_addr",
         descr="IP addresses on which the harbor should listen.",
         on_change=on_change_harbor_bind_addr,"harbor.bind_addr","0.0.0.0")

# @flag hidden
def on_change_video_samplerate(r) =
  deprecated("frame.video.samplerate", "frame.video.framerate")
  set("frame.video.framerate",r)
end

register(name="[DEPRECATED] Framerate",
         on_change=on_change_video_samplerate,"frame.video.samplerate",25)

# Deprecated: this function has been replaced by `input.external.rawaudio`.
# @flag hidden
def replaces input.external(~id="",~buffer=2.0,~channels=2,~max=10.0,
                   ~restart=true,~restart_on_error=false,
                   ~samplerate=44100,cmd) =
  deprecated("input.external", "input.external.rawaudio")
  input.external.rawaudio(id=id,buffer=buffer,channels=channels,
                          max=max,restart=restart,restart_on_error=restart_on_error,
                          samplerate=samplerate,cmd)
end

# Deprecated: this function has been replaced by `string.quote`.
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
  file.which(f) ?? ""
end

base64 = ()

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

# Deprecated: this function has been replaced by `fail`.
# @flag hidden
def empty(~id="")
  fail()
end

# Deprecated: use `request.create` instead.
# @flag hidden
def request.create.raw(~indicators=[], ~persistent=false, uri)
  deprecated("request.create.raw", "request.create")
  request.create(indicators=indicators, persistent=persistent, uri)
end

# Deprecated: use `file.remove` instead.
# @flag hidden
def file.unlink(filename)
  deprecated("file.unlink", "file.remove")
  file.remove(filename)
end

# Deprecated: was designed for transitions only and is not
# needed anymore
# @flag hidden
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
# needed anymore.
# @flag hidden
def fade.initial(~id="fade.initial",~duration=3.,~type="lin",s) =
  deprecated("fade.initial", "fade.in")  

  fn = mkfade(start=0.,stop=1.,type=type,duration=duration,s)
  fade.scale(id=id,fn,s)
end

# Deprecated: use `process.read` instead.
# @flag hidden
def get_process_output(~timeout=(-1.),~env=[],~inherit_env=true,command) =
  deprecated("get_process_output", "process.read")

  process.read(timeout=timeout, env=env, inherit_env=inherit_env, command)
end

# Deprecated: use `process.read.lines` instead.
# @flag hidden
def get_process_lines(~timeout=(-1.),~env=[],~inherit_env=true,command) =
  deprecated("get_process_lines", "process.read.lines")

  process.read.lines(timeout=timeout, env=env, inherit_env=inherit_env, command)
end

# Deprecated: use `process.test` instead.
# @flag hidden
def test_process(~timeout=(-1.),~env=[],~inherit_env=true,command) =
  deprecated("test_process", "process.test")

  process.test(timeout=timeout, env=env, inherit_env=inherit_env, command)
end

# Deprecated: use `process.run` instead.
# @flag hidden
def system(command) =
  deprecated("system", "process.run")

  process.run(command)
end

# Deprecated: use `blank.detect` instead
# @flag hidden
def on_blank(~id="on_blank", ~max_blank=20., ~min_noise=0.,
             ~on_noise, ~start_blank=false, ~threshold=(-40.),
             ~track_sensitive=true, cb, s) =
  deprecated("on_blank", "blank.detect")

  blank.detect(id=id, max_blank=max_blank, min_noise=min_noise,
               on_noise=on_noise, start_blank=start_blank, threshold=threshold,
               track_sensitive=track_sensitive, cb, s)
end

# Deprecated: use `blank.skip` instead
# @flag hidden
def skip_blank(~id="",~threshold=-40.,~max_blank=20.,~min_noise=0.,~track_sensitive=true,s) =
  deprecated("skip_blank", "blank.skip")

  blank.skip(id=id, threshold=threshold, max_blank=max_blank, min_noise=min_noise, track_sensitive=track_sensitive, s)
end

# Deprecated: use `blank.eat` instead
# @flag hidden
def eat_blank(~id="", ~at_beginning=false, ~max_blank=20., ~min_noise=0.,
              ~start_blank=false, ~threshold=(-40.), ~track_sensitive=true,
              s) =
  deprecated("eat_blank", "blank.eat")

  blank.eat(id=id, at_beginning=at_beginning, max_blank=max_blank, min_noise=min_noise,
            start_blank=start_blank, threshold=threshold, track_sensitive=track_sensitive,
            s)
end

# Deprecated: use `blank.strip` instead
# @flag hidden
def strip_blank(~id="", ~max_blank=20., ~min_noise=0., ~start_blank=false,
                ~threshold=(-40.), ~track_sensitive=true, s)
  deprecated("strip_blank", "blank.strip")

  blank.strip(id=id, max_blank=max_blank, min_noise=min_noise,
              start_blank=start_blank, threshold=threshold, track_sensitive=track_sensitive,
              s)
end

# Deprecated: use `time.local` instead
# @flag hidden
def localtime(t)
  deprecated("localtime", "time.local")
  time.local(t)
end

# Deprecated: use `time.utc` instead
# @flag hidden
def gmtime(t)
  deprecated("gmtime", "time.utc")
  time.utc(t)
end

# Deprecated: use `time` instead
# @flag hidden
def gettimeofday()
  deprecated("gettimeofday", "time")
  time()
end

%ifdef input.gstreamer.audio_video
# @docof input.gstreamer.audio_video
def input.gstreamer.audio_video(%argsof(input.gstreamer.audio_video)) =
  s = input.gstreamer.audio_video(%argsof(input.gstreamer.audio_video))

  server.register(namespace=s.id(), description="Set gstreamer pipeline state to play",
                  "play", fun (_) -> try
                    s.play()
                    "Done."
                  catch err do
                    "Error while setting state to play: #{err}"
                  end)

  server.register(namespace=s.id(), description="Set gstreamer pipeline state to paused",
                  "pause", fun (_) -> try
                    s.pause()
                    "Done."
                  catch err do
                    "Error while setting state to paused: #{err}"
                  end)

  server.register(namespace=s.id(), description="Restart gstreamer pipeline",
                  "restart", fun (_) -> try
                    s.restart()
                    "Done."
                  catch err do
                    "Error while restarting pipeline: #{err}"
                  end)

  s
end
%endif

# @docof blank.strip
def blank.strip(%argsof(blank.strip), s) =
  s = blank.strip(%argsof(blank.strip), s)

  server.register(namespace=s.id(), description="Check if the source is stripping.",
                  "is_stripping", fun (_) -> begin
                    "#{s.is_blank()}"
                  end)

  s
end

# @docof output.external
def output.external(%argsof(output.external), f, p, s) =
  s = output.external(%argsof(output.external), f, p, s)

  server.register(namespace=s.id(), description="Re-open the output.",
                  "reopen", fun (_) -> begin
                    s.reopen()
                    "Done."
                  end)

  s
end

# @docof input.external.avi
def input.external.avi(%argsof(input.external.avi), s) =
  s = input.external.avi(%argsof(input.external.avi), s)

  server.register(namespace=s.id(), description="Show internal buffer length (in seconds).",
                  "buffer_length", fun (_) -> begin
                    let (audio, video, total) = s.buffer_length()
                    "audio buffer length: #{audio}\nvideo buffer length: #{video}\ntotal buffer length: #{total}"
                  end)

  s
end

# @docof input.harbor
def input.harbor(%argsof(input.harbor), s) =
  s = input.harbor(%argsof(input.harbor), s)

  server.register(namespace=s.id(), description="Stop current source client, if connected.",
                  "stop", fun (_) -> begin
                    s.stop()
                    "Done"
                  end)

  server.register(namespace=s.id(), description="Display current status.",
                  "status", fun (_) -> begin
                    s.status()
                  end)

  server.register(namespace=s.id(), description="Get the buffer's length, in seconds.",
                  "buffer_length", fun (_) -> begin
                    "#{s.buffer_length()}"
                  end)

  s
end

# @docof input.http
def input.http(%argsof(input.http), s) =
  s = input.http(%argsof(input.http), s)

  server.register(namespace=s.id(), description="Start the source, if needed.",
                  "start", fun (_) -> begin
                    s.start()
                    "Done!"
                  end)

  server.register(namespace=s.id(), description="Stop the source if connected.",
                  "stop", fun (_) -> begin
                    s.stop()
                    "Done!"
                  end)

  server.register(namespace=s.id(), description="Get or set the stream's HTTP URL. Setting a new URL will not affect an ongoing connection.",
                  usage="url [url]", "url", fun (url) -> begin
                    if url == "" then
                      s.url()
                    else begin
                      s.set_url({url})
                      "Done!"
                    end end
                  end)

  server.register(namespace=s.id(), description="Return the current status of the source, either \"stopped\" \
                                                 (the source isn't trying to relay the HTTP stream), \"polling\" \
                                                 (attempting to connect to the HTTP stream) or \"connected <url>\" \
                                                 (connected to <url>, buffering or playing back the stream).",
                  "status", fun (_) -> begin
                    s.status()
                  end)

  server.register(namespace=s.id(), description="Get the buffer's length, in seconds.",
                  "buffer_length", fun (_) -> begin
                    "#{s.buffer_length()}"
                  end)

  s
end

let replaces output=output.dummy
%ifdef output.ao
  let replaces output=output.ao
%endif
%ifdef output.alsa
  let replaces output=output.alsa
%endif
%ifdef output.oss
  let replaces output=output.oss
%endif
%ifdef output.portaudio
  let replaces output=output.portaudio
%endif
%ifdef output.pulseaudio
  let replaces output=output.pulseaudio
%endif

# @flag hidden 
def replaces output(~id="",~fallible=false,
           ~on_start={()},~on_stop={()},~start=true,s)
  output(id=id,fallible=fallible,
         start=start,on_start=on_start,on_stop=on_stop,
         mksafe(s))
end

let output.video = output.dummy
%ifdef output.sdl
  let output.video = output.sdl
%endif
%ifdef output.graphics
  let output.video = output.graphics
%endif

# @flag hidden
def output.audio_video(~id="",~fallible=false,~on_start={()},~on_stop={()},~start=true,s)
  output(id=id, fallible=fallible, start=start, on_start=on_start, on_stop=on_stop, s)
  output.video(id=id, fallible=fallible, start=start, on_start=on_start, on_stop=on_stop, s)
end


def replaces input(~id="",~start=true,~on_start={()},~on_stop={()},~fallible=false)
  blank(id=id)
end
%ifdef input.alsa
  let replaces input = input.alsa
%endif
%ifdef input.oss
  let replaces input = input.oss
%endif
%ifdef input.portaudio
  let replaces input = input.portaudio
%endif
%ifdef input.pulseaudio
  let replaces input = input.pulseaudio
%endif

# @flag hidden
def replaces input(~id="",~start=true,~on_start={()},~on_stop={()},~fallible=false)
  input(id=id,start=start,on_start=on_start,on_stop=on_stop,fallible=fallible)
end

# @flag hidden
def output.prefered(~id="",~fallible=false,
                    ~on_start={()},~on_stop={()},~start=true,s)
  deprecated("output.prefered","output")
  output(id=id,fallible=fallible,
         start=start,on_start=on_start,on_stop=on_stop,
         s)
end

# Deprecated: use `output` instead.
# @flag hidden
def out(s)
  deprecated("out","output")
  output.prefered(mksafe(s))
end

# Deprecated: use `input` instead.
# @flag hidden
def in(~id="",~start=true,~on_start={()},~on_stop={()},~fallible=false)
  deprecated("in", "input")
  input(id=id,start=start,on_start=on_start,on_stop=on_stop,fallible=fallible)
end
