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

%ifdef input.external.rawaudio
# Deprecated: this function has been replaced by `input.external.rawaudio`.
# @flag deprecated
def replaces input.external(%argsof(input.external.rawaudio), cmd)
  deprecated("input.external", "input.external.rawaudio")
  input.external.rawaudio(%argsof(input.external.rawaudio), cmd)
end
%endif

# Deprecated: this function has been replaced by `string.quote`.
# @flag deprecated
def quote(s) =
  deprecated("quote", "string.quote")
  string.quote(s)
end

let string.utf8 = ()

# Deprecated: this function has been replaced by `string.escape`.
# @flag deprecated
def string.utf8.escape(s) =
  deprecated("string.utf8.escape", "string.escape")
  string.escape(s)
end

# Deprecated: use mksafe and playlist instead.
# @flag deprecated
def playlist.safe(~id=null(), ~mime_type="", ~mode="randomize", ~on_track={()}, ~prefix="", ~reload=0, ~reload_mode="seconds", uri)
  deprecated("playlist.safe", "")
  ignore(on_track)
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
# @flag deprecated
def playlist.once(~id=null(),~random=false,~reload_mode="",
                  ~prefetch=1,~filter=fun(_)->true,uri)
  deprecated("playlist.once", "playlist")
  mode = if random then "randomize" else "normal" end
  reload_mode = if reload_mode == "" then "never" else reload_mode end
  playlist(reload_mode=reload_mode, loop=false, id=id, mode=mode, prefetch=prefetch, check_next=filter, uri)
end

# Deprecated: use metadata.map
# @flag deprecated
def map_metadata(%argsof(metadata.map), fn, s)
  deprecated("map_metadata", "metadata.map")
  metadata.map(%argsof(metadata.map), fn, s)
end

# Deprecated: this function has been replaced by `metadata.map`.
# @flag deprecated
def rewrite_metadata(l,~insert_missing=true,~update=true,~strip=false,s)
  deprecated("rewrite_metadata", "metadata.map")
  def map(m)
    def apply(x)
      label = fst(x)
      value = snd(x)
      (label,value % m)
    end
    list.map(apply,l)
  end
  metadata.map(map,insert_missing=insert_missing,update=update,strip=strip,s)
end

# Deprecated: this function will be removed in a future release
# @flag deprecated
def id(~id=null(),s)
 deprecated("id","")
 ignore(id)
 s
end

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
    def log(~level,x) = log(label=radio,level=level,x) end
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
    log(level=4,"Response status: #{answer.http_version} #{answer.status_code} #{answer.status_message}")
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

# Deprecated: this function has been replaced by `source.fail`.
# @flag deprecated
def empty(~id=null())
  deprecated("empty", "source.fail")
  source.fail(id=id)
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
  switch(track_sensitive=false,[(should_play,s)])
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
def on_blank(%argsof(blank.detect), f, s)
  deprecated("on_blank", "blank.detect")
  blank.detect(%argsof(blank.detect), f, s)
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

# Deprecated: use `http.get` instead.
# @flag deprecated
def https.get(%argsof(http.get), url)
  deprecated("https.get", "http.get")
  http.get(%argsof(http.get), url)
end

# Deprecated: use `http.put` instead.
# @flag deprecated
def https.put(%argsof(http.put), url)
  deprecated("https.put", "http.put")
  http.put(%argsof(http.put), url)
end

# Deprecated: use `http.post` instead.
# @flag deprecated
def https.post(%argsof(http.post), url)
  deprecated("https.post", "http.post")
  http.post(%argsof(http.post), url)
end

# Deprecated: use `http.head` instead.
# @flag deprecated
def https.head(%argsof(http.head), url)
  deprecated("https.head", "http.head")
  http.head(%argsof(http.head), url)
end

# Deprecated: use `http.delete` instead.
# @flag deprecated
def https.delete(%argsof(http.delete), url)
  deprecated("https.delete", "http.delete")
  http.delete(%argsof(http.delete), url)
end

%ifdef input.http
# Deprecated: use `input.http` instead
# @flag deprecated
def input.https(%argsof(input.http), url)
  deprecated("input.https", "input.http")
  input.http(%argsof(input.http), url)
end
%endif

%ifdef source.say_metadata
# Deprecated: use `source.say_metadata` instead
# @flag deprecated
def say_metadata(s, ~pattern) =
  def pattern(m) = pattern % m end
  source.say_metadata(pattern=pattern, s)
end
%endif

# Deprecated: use `playlist.parse.register` instead
# @flag deprecated
def add_playlist_parser(%argsof(playlist.parse.register), s) =
  deprecated("add_playlist_parser", "playlist.parse.register")
  playlist.parse.register(%argsof(playlist.parse.register), s)
end

# Deprecated: use `json.stringify` instead
# @flag deprecated
# @argsof json.stringify
def json_of(%argsof(json.stringify), v) =
  deprecated("json_of", "json.stringify")
  json.stringify(%argsof(json.stringify), v)
end

# Deprecated: use `json.parse` instead
# @flag deprecated
# @argsof json.parse
def of_json(%argsof(json.parse), v) =
  deprecated("of_json", "json.parse")
  json.parse(%argsof(json.parse), v)
end

# Deprecated: use error method instead
# @flag deprecated
def error.kind(err) =
  deprecated("error.kind", "")
  err.kind
end

# Deprecated: use error method instead
# @flag deprecated
def error.message(err) =
  deprecated("error.message", "")
  err.message
end

# Deprecated: use `playlist` instead
# @flag deprecated
def playlist.reloadable(~id=null(), ~mime_type="", ~mode="randomize", ~on_track={()}, ~prefix="", ~reload=0, ~reload_mode="seconds", uri) =
  deprecated("playlist.reloadable", "playlist")
  ignore(on_track)
  playlist(id=id, mime_type=mime_type, mode=mode, prefix=prefix, reload=reload, reload_mode=reload_mode, uri)
end

# Deprecated: use `request.dynamic` instead
# @flag deprecated
def request.dynamic.list(%argsof(request.dynamic), f) =
  deprecated("request.dynamic.list", "request.dynamic")

  add = ref (fun (_) -> ())

  def f() =
    l = f()
    if l != [] then
      let [r, ...q] = list.rev(l)
      r = (r:request)
      list.iter(add(), list.rev(q))
      r
    else
      null()
    end
  end

  s = request.dynamic(%argsof(request.dynamic), f)

  add := fun (r) -> ignore(s.add(r))

  s
end

# Deprecated: use `process.run` instead
# @flag deprecated
def run_process(%argsof(process.run), p) =
  deprecated("run_process", "process.run")
  x = process.run(%argsof(process.run), p)
  (x.stdout, x.stderr, ("#{x.status}", x.status.description))
end

# Deprecated: use `list.assoc.mem` instead
# @flag deprecated
def list.mem_assoc(x,y) =
  deprecated("list.mem_assoc", "list.assoc.mem")
  list.assoc.mem(x,y)
end

# Deprecated: use `runtime.gc.full_major` instead
# @flag deprecated
def garbage_collect() =
  deprecated("garbage_collect", "runtime.gc.full_major")
  runtime.gc.full_major()
end

%ifdef output.youtube.live.gstreamer
# Deprecated: use `output.youtube.live.rtmp`
# @flag deprecated
def replaces output.youtube.live(%argsof(output.youtube.live.gstreamer), s) =
   deprecated("output.youtube.live", "output.youtube.live.rtmp")
   output.youtube.live.gstreamer(%argsof(output.youtube.live.gstreamer), s)
end
%endif

# Deprecated: use `video.alpha.of_color`
# @flag deprecated
def video.transparent(%argsof(video.alpha.of_color), s)
  deprecated("video.transparent", "video.alpha.of_color")
  video.alpha.of_color(%argsof(video.alpha.of_color), s)
end

# Deprecated: use `request.resolved`
# @flag deprecated
def request.ready(r) =
  deprecated("request.ready", "request.resolved")
  request.resolved(r)
end

# Deprecated: use `environment.get`
# @flag deprecated
def getenv(default="", v) =
  deprecated("getenv", "environment.get")
  environment.get(default=default, v)
end

# Deprecated: use `environment.set`
# @flag deprecated
def setenv(k, v) =
  deprecated("setenv", "environment.set")
  environment.set(k, v)
end

# Deprecated: use `getpid`
# @flag deprecated
def getpid() =
  deprecated("getpid", "process.pid")
  process.pid()
end

# Deprecated: use `file.mime`
# @flag deprecated
def get_mime(fname)
  deprecated("get_mime", "file.mime")
  file.mime(fname) ?? ""
end

# Deprecated: use `string`.
# @flag deprecated
def string_of(s)
  deprecated("string_of", "string")
  string(s)
end

# Deprecated: use `string.float`.
# @flag deprecated
def string_of_float(x)
  deprecated("string_of_float", "string.float")
  string.float(x)
end

# Deprecated: use `protocol.add`.
# @flag deprecated
def add_protocol(%argsof(protocol.add),name,fn) =
  deprecated("add_protocol", "protocol.add")
  protocol.add(%argsof(protocol.add),name,fn)
end

# Deprecated: use `decoder.add`.
# @flag deprecated
def add_decoder(%argsof(decoder.add),process) =
  deprecated("add_decoder", "decoder.add")
  decoder.add(%argsof(decoder.add),process)
end

# Deprecated: use `decoder.metadata.add`.
# @flag deprecated
def add_metadata_resolver(name, fn) =
  deprecated("add_metadata_resolver", "decoder.metadata.add")
  decoder.metadata.add(name, fn)
end

# Deprecated: use `decoder.oblivious.add`.
# @flag deprecated
def add_oblivious_decoder(%argsof(decoder.oblivious.add),process) =
  deprecated("add_oblivious_decoder", "decoder.oblivious.add")
  decoder.oblivious.add(%argsof(decoder.oblivious.add),process)
end

# Deprecated: use `source.mux.audio`.
# @flag deprecated
def mux_audio(%argsof(source.mux.audio), s)
  deprecated("mux_audio", "source.mux.audio")
  source.mux.audio(%argsof(source.mux.audio), s)
end

# Deprecated: use `source.mux.video`.
# @flag deprecated
def mux_video(%argsof(source.mux.video), s)
  deprecated("mux_video", "source.mux.video")
  source.mux.video(%argsof(source.mux.video), s)
end

# Deprecated: use `source.mux.midi`
# @flag deprecated
def mux_midi(%argsof(source.mux.midi), s)
  deprecated("mux_midi", "source.mux.midi")
  source.mux.midi(%argsof(source.mux.midi), s)
end

# Deprecated: use `source.drop.audio`
# @flag deprecated
def drop_audio(%argsof(source.drop.audio), s)
  deprecated("drop_audio", "source.drop.audio")
  source.drop.audio(%argsof(source.drop.audio), s)
end

# Deprecated: use `source.drop.video`
# @flag deprecated
def drop_video(%argsof(source.drop.video), s)
  deprecated("drop_video", "source.drop.video")
  source.drop.video(%argsof(source.drop.video), s)
end

# Deprecated: use `source.drop.midi`
# @flag deprecated
def drop_midi(%argsof(source.drop.midi), s)
  deprecated("drop_midi", "source.drop.midi")
  source.drop.midi(%argsof(source.drop.midi), s)
end

# Deprecated: use `source.drop.metadata`
# @flag deprecated
def drop_metadata(%argsof(source.drop.metadata), s)
  deprecated("drop_metadata", "source.drop.metadata")
  source.drop.metadata(%argsof(source.drop.metadata), s)
end

# Deprecated: use `source.stereo`
# @flag deprecated
def audio_to_stereo(~id=null(), s) =
  deprecated("audio_to_sterep", "stereo")
  stereo(id=id, s)
end

# Deprecated: use `source.tracks` and `source`
# @flag deprecated
def merge_tracks(~id=null(), s) =
  deprecated("merge_tracks", "source")
  let {track_marks=_, ...tracks} = source.tracks(s)
  source(id=id, tracks)
end

%ifdef compress
# Deprecated: use `compress`
# @flag deprecated
def compress.old(~id=null(), s) =
  deprecated("compress.old", "compress")
  compress(id=id, s)
end
%endif

# Deprecated: use `thread.pause`
# @flag deprecated
def sleep(s) =
  deprecated("sleep", "thread.pause")
  thread.pause(s)
end

# Deprecated: use `video.add_rectangle`.
# @flag deprecated
def video.rectangle(%argsof(video.add_rectangle), s)
  video.add_rectangle(%argsof(video.add_rectangle), s)
end

# Deprecated: use `video.add_line`.
# @flag deprecated
def video.line(%argsof(video.add_line), src, tgt, s)
  video.add_line(%argsof(video.add_line), src, tgt, s)
end
