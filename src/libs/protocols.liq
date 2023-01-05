# @flag hidden
def settings.make.protocol(name) =
  settings.make.void(
    "Settings for the #{name} protocol"
  )
end

let settings.protocol = settings.make.void(
  "Settings for registered protocols"
)

let settings.protocol.replaygain = settings.make.protocol("ReplayGain")
let settings.protocol.replaygain.tag = settings.make(
  description="Tag used to annotate ReplayGain",
  "replaygain_track_gain"
)

# Register the replaygain protocol.
# @flag hidden
def replaygain_protocol(~rlog=_,~maxtime=_,arg)
  gain = file.replaygain(arg)
  tag = settings.protocol.replaygain.tag()
  if null.defined(gain) then
    ["annotate:#{tag}=\"#{null.get(gain)} dB\":#{arg}"]
  else
    [arg]
  end
end
protocol.add("replaygain", replaygain_protocol,
             syntax="replaygain:uri",
             doc="Compute ReplayGain value. \
                  Adds returned value as `\"replaygain_track_gain\"` metadata")

let settings.protocol.process = settings.make.protocol("process")

let settings.protocol.process.env = settings.make(
  description="List of environment variables \
               passed down to the executed process.",
  []
)

let settings.protocol.process.inherit_env = settings.make(
  description="Inherit calling process's environment when `env` parameter is empty.",
  true
)

# Register the process protocol. Syntax:
# process:<output ext>,<cmd>:uri where <cmd> is interpolated with:
# [("input",<input file>),("output",<output file>),("colon",":")]
# See say: protocol for an example.
# @flag hidden
def process_protocol(~rlog=_,~maxtime,arg)
  log.info("Processing #{arg}")

  x = r/:/.split(arg)
  uri = string.concat(separator=":",list.tl(x))
  x = r/,/.split(list.hd(default="",x))
  a = list.hd(default="liq",x)
  x = list.tl(x)

  let (timeout, extname, cmd) =
    # Allow specifying the timeout before the extension
    if string.contains(prefix="timeout=", a) then
      timeout = string.residual(prefix="timeout=", a)
      timeout = null.map(string.to_float, timeout)
      timeout = timeout??(maxtime - time())
      extname = list.hd(default="liq",x)
      cmd = string.concat(separator=",", list.tl(x))
      (timeout, extname, cmd)
    else
      timeout = maxtime - time()
      extname = a
      cmd = string.concat(separator=",", list.tl(x))
      (timeout, extname, cmd)
    end

  output = file.temp("liq-process", ".#{extname}")

  def resolve(input) =
    cmd = cmd % [("input",process.quote(input)),
                 ("output",process.quote(output)),
                 ("colon",":")]

    log.info("Executing #{cmd}")

    env_vars = settings.protocol.process.env()
    env = environment()
    def get_env(k) = (k,env[k]) end
    env = list.map(get_env,env_vars)
    inherit_env = settings.protocol.process.inherit_env()

    p = process.run(timeout=timeout, env=env, inherit_env=inherit_env, cmd)

    if p.status == "exit" and p.status.code == 0 then
      [output]
    else
      log.important("Failed to execute #{cmd}: #{p.status} (#{p.status.code})")
      log.info("Standard output:\n#{p.stdout}")
      log.info("Error output:\n#{p.stderr}")
      log.info("Removing #{output}.")
      file.remove(output)
      []
    end
  end

  if uri == "" then
    resolve("")
  else
    r = request.create(uri)
    delay = maxtime - time()
    if request.resolve(timeout=delay,r) then
      res = resolve(request.filename(r))
      request.destroy(r)
      res
    else
      log(level=3,"Failed to resolve #{uri}")
      []
    end
  end
end
protocol.add(temporary=true, "process", process_protocol,
             doc="Resolve a request using an arbitrary process. \
                  `<cmd>` is interpolated with: \
                  `[(\"input\",<input>),(\"output\",<output>),\
                  (\"colon\",\":\")]`. `uri` is an optional child request, \
                  `<output>` is the name of a fresh temporary file and has \
                  extension `.<extname>`. `<input>` is an optional input \
                  file name as returned while resolving `uri`.",
             syntax="process:<extname>,<cmd>[:uri]")

# Create a process: uri, replacing `:` with `$(colon)`.
# @category Liquidsoap
# @param cmd Command line to execute
# @param ~extname Output file extension (with no leading '.')
# @param ~uri Input uri
def process_uri(~timeout=null(),~extname,~uri="",cmd) =
  timeout = null.case(timeout, {""}, fun(t) -> "timeout=" ^ string(t) ^ ",")
  cmd = r/:/.replace(fun (_) -> "$(colon)",cmd)
  uri = if uri != "" then ":#{uri}" else "" end
  "process:#{timeout}#{extname},#{cmd}#{uri}"
end

%ifdef http.head
# Resolve http(s) URLs using curl
# @flag hidden
def http_protocol(proto,~rlog,~maxtime,arg) =
  uri = "#{proto}:#{arg}"

  def log(~level,s) =
    rlog(s)
    log(label="procol.external",level=level,s)
  end

  timeout = maxtime - time()

  ret = http.head(timeout_ms=int_of_float(timeout*1000.), uri)
  code = ret.status_code ?? 999
  headers = ret.headers

  mime =
    if 200 <= code and code < 300 then
      headers["content-type"]
    else
      log(level=3,"Failed to fetch mime-type for #{uri}.")
      log(level=4,"Request response: #{ret}")
      null()
    end

  extname = null.case(mime, (fun () -> "osb"), (fun (mime) ->
    if list.mem(mime, ["audio/mpeg", "audio/mp3"]) then
      "mp3"
    elsif list.mem(mime,["application/ogg", "application/x-ogg",
                         "audio/x-ogg", "audio/ogg", "video/ogg"]) then
      "ogg"
    elsif list.mem(mime, ["audio/flac", "audio/x-flac"]) then
      "flac"
    elsif list.mem(mime,["audio/mp4", "application/mp4", "video/mp4"]) then
      "mp4"
    elsif list.mem(mime,["audio/vnd.wave", "audio/wav",
                         "audio/wave", "audio/x-wav"]) then
      "wav"
    else
      log(level=3,"No known file extension for mime: #{mime}")
      "osb"
    end))

  output = file.temp("liq-process", ".#{extname}")

  file_writer = file.write.stream(output)

  timeout = maxtime - time()

  try
    response = http.get.stream(
      on_body_data=file_writer,
      timeout_ms=int_of_float(timeout * 1000.),
      uri
    )

    if response.status_code < 400 then
      [output]
    else
      log(level=3, "Error while fetching http data: #{response.status_code} - #{response.status_message}")
      []
    end
  catch err do
    log(level=3, "Error while fetching http data: #{err}")
    []
  end
end

# Register download protocol.
# @flag hidden
def add_http_protocol(proto) =
  def http_protocol(~rlog,~maxtime,arg) = http_protocol(proto,rlog=rlog,maxtime=maxtime,arg) end
  protocol.add(temporary=true,syntax="#{proto}://...",doc="Download http URLs using curl",proto,http_protocol)
end
list.iter(add_http_protocol,["http","https"])
%endif

let settings.protocol.youtube_dl = settings.make.protocol("youtube-dl")

let settings.protocol.youtube_dl.path = settings.make(
  description="Path of the youtube-dl binary",
  "youtube-dl"
)

let settings.protocol.youtube_dl.timeout = settings.make(
  description="Timeout (in seconds) for youtube-dl executions.",
  300.
)

# Register the youtube-dl protocol, using youtube-dl.
# Syntax: youtube-dl:<ID>
# @flag hidden
def youtube_dl_protocol(~rlog,~maxtime,arg)
  binary = settings.protocol.youtube_dl.path()
  timeout = settings.protocol.youtube_dl.timeout()

  def log(~level,s) =
    rlog(s)
    log(label="protocol.youtube-dl",level=level,s)
  end

  delay = maxtime - time()
  cmd = "#{binary} --get-title --get-filename -- #{process.quote(arg)}"
  log(level=4,"Executing #{cmd}")
  x = process.read.lines(timeout=delay,cmd)

  x =
    if list.length(x) >= 2 then
      x
    else
      ["",".osb"]
    end

  title = list.hd(default="",x)
  ext   = file.extension(leading_dot=false,list.nth(default="",x,1))

  cmd = "rm -f $(output) && #{binary} -q -f best --no-playlist -o $(output) -- #{process.quote(arg)}"
  cmd = process_uri(timeout=timeout, extname=ext, cmd)

  if title != "" then
    ["annotate:title=#{string.quote(title)}:#{cmd}"]
  else
    [cmd]
  end
end
protocol.add("youtube-dl", youtube_dl_protocol,
              doc="Resolve a request using youtube-dl.",
              syntax="youtube-dl:uri")

# Register the youtube-pl protocol.
# Syntax: youtube-pl:<ID>
# @flag hidden
def youtube_pl_protocol(~rlog=_,~maxtime,arg)
  binary = settings.protocol.youtube_dl.path()
  delay = maxtime - time()
  cmd = "#{binary} -i -s --get-id --flat-playlist -- #{process.quote(arg)}"
  log(level=4,"Executing #{cmd}")
  l = process.read.lines(timeout=delay,cmd)
  l = list.map(fun(s) -> "youtube-dl:https://www.youtube.com/watch?v="^s, l)
  l = string.concat(separator="\n", l) ^ "\n"
  tmp = file.temp("youtube-pl","")
  file.write(data=l,tmp)
  [tmp]
end
protocol.add("youtube-pl", youtube_pl_protocol,
              doc="Resolve a request as a youtube playlist using youtube-dl. You typically want to use this as `playlist(\"youtube-pl:...\")`.",
              temporary=true,syntax="youtube-pl:uri")

# Register tmp
# @flag hidden
def tmp_protocol(~rlog=_, ~maxtime=_, arg) =
  [arg]
end
protocol.add("tmp",tmp_protocol,
             doc="Mark the given uri as temporary. Useful when chaining protocols",
             temporary=true,syntax="tmp:uri")

let settings.protocol.ffmpeg = settings.make.protocol("FFmpeg")

let settings.protocol.ffmpeg.path = settings.make(
  description="Path to the ffmpeg binary",
  "ffmpeg"
)

let settings.protocol.ffmpeg.metadata = settings.make(
  description="Should the protocol extract metadata",
  true
)

let settings.protocol.ffmpeg.replaygain = settings.make(
  description="Should the protocol adjust ReplayGain",
  false
)

# Register ffmpeg
# @flag hidden
def ffmpeg_protocol(~rlog,~maxtime,arg) =
  ffmpeg   = settings.protocol.ffmpeg.path()
  metadata = settings.protocol.ffmpeg.metadata()
  replaygain = settings.protocol.ffmpeg.replaygain()

  def log(~level,s) =
    rlog(s)
    log(label="protocol.ffmpeg",level=level,s)
  end

  def annotate(m) =
    def f(x) =
      let (key,value) = x
      "#{key}=#{string.quote(value)}"
    end
    m = string.concat(separator=",",list.map(f,m))
    if string.length(m) > 0 then
      "annotate:#{m}:"
    else
      ""
    end
  end

  def parse_metadata(file) =
    cmd = "#{ffmpeg} -i #{process.quote(file)} -f ffmetadata - 2>/dev/null | grep -v '^;'"
    delay = maxtime - time()
    log(level=4,"Executing #{cmd}")
    lines = process.read.lines(timeout=delay,cmd)
    def f(cur,line) =
      m = r/=/.split(line)
      if list.length(m) >= 2 then
        key = list.hd(default="",m)
        value = string.concat(separator="=",list.tl(m))
        (key,value)::cur
      else
        cur
      end
    end
    list.fold(f,[],lines)
  end

  def replaygain_filter(fname) =
    if replaygain then
      gain = file.replaygain(fname)
      if null.defined(gain) then
        "-af \"volume=#{null.get(gain)} dB\""
      else
        ""
      end
    else
      ""
    end
  end

  def cue_points(m) =
    cue_in = float_of_string(default=0., list.assoc(default="0.","liq_cue_in",m))
    cue_out = float_of_string(default=0., list.assoc(default="","liq_cue_out",m))

    args =
      if cue_in > 0. then
        "-ss #{cue_in}"
      else
        ""
      end

    if cue_out > cue_in then
      "#{args} -t #{cue_out-cue_in}"
    else
      args
    end
  end

  def fades(r) =
    m = request.metadata(r)

    fade_type = list.assoc(default="","liq_fade_type",m)
    fade_in = list.assoc(default="","liq_fade_in",m)
    cue_in = list.assoc(default="","liq_cue_in",m)
    fade_out = list.assoc(default="","liq_fade_out",m)
    cue_out = list.assoc(default="","liq_cue_out",m)

    curve =
      if fade_type == "lin" then
        ":curve=tri"
      elsif fade_type == "sin" then
        ":curve=qsin"
      elsif fade_type == "log" then
        ":curve=log"
      elsif fade_type == "exp" then
        ":curve=exp"
      else
        ""
      end


    args =
      if fade_in != "" then
        fade_in = float_of_string(default=0.,fade_in)
        start_time =
          if cue_in != "" then
            float_of_string(default=0.,cue_in)
          else
            0.
        end
        if fade_in > 0. then
          ["afade=in:st=#{start_time}:d=#{fade_in}#{curve}"]
        else
          []
        end
      else
        []
      end

    args =
      if fade_out != "" then
        fade_out = float_of_string(default=0.,fade_out)
        end_time =
          if cue_out != "" then
            float_of_string(default=0.,cue_out)
          else
            null.get(request.duration(request.filename(r)))
          end
        if fade_out > 0. then
          list.append(args,["afade=out:st=#{end_time-fade_out}:d=#{fade_out}#{curve}"])
        else
          args
        end
      else
        args
      end

    if list.length(args) > 0 then
      args = string.concat(separator=",",args)
      "-af #{args}"
    else
      ""
    end
  end

  r = request.create(arg)
  delay = maxtime - time()
  if request.resolve(timeout=delay,r) then
    filename = request.filename(r)
    m = request.metadata(r)

    m = if metadata then
      list.append(m,parse_metadata(filename))
    else
      m
    end

    annotate = annotate(m)
    request.destroy(r)

    # Now parse the audio
    wav = file.temp("liq-process", ".wav")

    cue_points = cue_points(request.metadata(r))
    fades = fades(r)
    replaygain_filter = replaygain_filter(filename)

    cmd = "#{ffmpeg} -y -i $(input) #{cue_points} #{fades} #{replaygain_filter} #{process.quote(wav)}"

    uri = process_uri(extname="wav",uri=filename,cmd)

    wav_r = request.create(uri)
    delay = maxtime - time()
    if request.resolve(timeout=delay,wav_r) then
      request.destroy(wav_r)
      ["#{annotate}tmp:#{wav}"]
    else
      log(level=3,"Failed to resolve #{uri}")
      []
    end
  else
    log(level=3,"Failed to resolve #{arg}")
    []
  end
end
protocol.add("ffmpeg",ffmpeg_protocol,
             doc="Decode any file to wave using ffmpeg",
             syntax="ffmpeg:uri")

# Register stereo protocol which converts a file to stereo (currently decodes as
# wav).
# @flag hidden
def stereo_protocol(~rlog=_, ~maxtime=_, arg)
  file = file.temp("liq-stereo", ".wav")
  r = request.create(arg)
  if not request.resolve(r) then
    log.info("Stereo: failed to resolve request #{arg}")
    []
  else
    # TODO: the following sometimes hangs, so we resolve twice...
    # source.dump(%wav, file, source.stereo(once(request.queue(queue=[r]))))
    source.dump(%wav, file, stereo(once(single(arg))))
    [file]
  end
end
protocol.add(static=true, temporary=true, "stereo", stereo_protocol, doc="Convert a file to stereo (currently decodes to wav).", syntax="stereo:<uri>")

let settings.protocol.text2wave = settings.make.protocol("text2wave")

let settings.protocol.text2wave.path = settings.make(
  description="Path to the text2wave binary",
  "text2wave"
)

# Register the text2wave: protocol using text2wav
# @flag hidden
def text2wave_protocol(~rlog=_, ~maxtime=_, arg) =
  binary = settings.protocol.text2wave.path()
  [process_uri(extname="wav", "echo #{process.quote(arg)} | #{binary} -scale 1.9 > $(output)")]
end
protocol.add(static=true,"text2wave",text2wave_protocol,
             doc="Generate speech synthesis using text2wave. Result may be mono.",
             syntax="text2wave:Text to read")

let settings.protocol.gtts = settings.make.protocol("gtts")

let settings.protocol.gtts.path = settings.make(
  description="Path to the gtts binary",
  "gtts-cli"
)

# Register the gtts: protocol using gtts
# @flag hidden
def gtts_protocol(~rlog=_, ~maxtime=_, arg) =
  binary = settings.protocol.gtts.path()
  [process_uri(extname="mp3", "#{binary} -o $(output) #{process.quote(arg)}")]
end
protocol.add(static=true,"gtts",gtts_protocol,
             doc="Generate speech synthesis using Google translate's text-to-speech API. This requires the `gtts-cli` binary.  Result may be mono.",
             syntax="gtts:Text to read")

# Register the legacy say: protocol
# @flag hidden
def say_protocol(~rlog=_, ~maxtime=_, arg) =
  ["stereo:gtts:#{arg}", "stereo:text2wave:#{arg}"]
end
protocol.add(static=true,"say",say_protocol,
             doc="Generate speech synthesis using text2wave. Result is always stereo.",
             syntax="say:Text to read")

let settings.protocol.aws = settings.make.protocol("AWS")

let settings.protocol.aws.profile = settings.make(
  description="Use a specific profile from your credential file.",
  null()
)

let settings.protocol.aws.endpoint = settings.make(
  description="Alternative endpoint URL (useful for other S3 implementations).",
  null()
)

let settings.protocol.aws.region = settings.make(
  description="AWS Region",
  null()
)

let settings.protocol.aws.path = settings.make(
  description="Path to aws CLI binary",
  "aws"
)

let settings.protocol.aws.polly = settings.make.protocol("polly")

let settings.protocol.aws.polly.format = settings.make(
  description="Output format",
  "mp3"
)

let settings.protocol.aws.polly.voice = settings.make(
  description="Voice ID",
  "Joanna"
)

# Build a aws base call
# @flag hidden
def aws_base() =
  aws = settings.protocol.aws.path()

  region = settings.protocol.aws.region()

  aws =
    if null.defined(region) then
      "#{aws} --region #{null.get(region)}"
    else
      aws
    end

  endpoint = settings.protocol.aws.endpoint()

  aws =
    if null.defined(endpoint) then
      "#{aws} --endpoint-url #{process.quote(null.get(endpoint))}"
    else
      aws
    end

  profile = settings.protocol.aws.profile()

  if null.defined(profile) then
    "#{aws} --profile #{process.quote(null.get(profile))}"
  else
    aws
  end
end

# Register the s3:// protocol
# @flag hidden
def s3_protocol(~rlog=_, ~maxtime=_, arg) =
  extname = file.extension(leading_dot=false,dir_sep="/",arg)
  arg = process.quote("s3:#{arg}")
  [process_uri(extname=extname,"#{aws_base()} s3 cp #{arg} $(output)")]
end
protocol.add("s3",s3_protocol,doc="Fetch files from s3 using the AWS CLI",
             syntax="s3://uri")

# Register the polly: protocol using AWS Polly
# speech synthesis services. Syntax: polly:<text>
# @flag hidden
def polly_protocol(~rlog=_, ~maxtime=_, text) =
  aws = aws_base()

  format = settings.protocol.aws.polly.format()

  extname =
    if format == "mp3" then
      "mp3"
    elsif format == "ogg_vorbis" then
      "ogg"
    else
      "wav"
    end

  aws = "#{aws} polly synthesize-speech --output-format #{format}"

  voice_id = settings.protocol.aws.polly.voice()

  cmd = "#{aws} --text #{process.quote(text)} --voice-id #{process.quote(voice_id)} $(output)"

  [process_uri(extname=extname,cmd)]
end
protocol.add(static=true,"polly",polly_protocol,
             doc="Generate speech synthesis using AWS polly service. \
                  Result might be mono, needs aws binary in the path.",
             syntax="polly:Text to read")

# Protocol to synthesize audio.
# @flag hidden
def synth_protocol(~rlog=_, ~maxtime=_, text) =
  log.debug(label="synth", "Synthesizing request: #{text}")
  args = r/,/.split(text)
  args = list.map(r/=/.split, args)
  if list.exists(fun(l)-> list.length(l) != 2, args) then
    []
  else
    args = list.map(fun(l) -> (list.hd(default="",l), list.hd(default="",list.tl(l))), args)
    shape = ref("sine")
    duration = ref(10.)
    frequency = ref(440.)
    def set(p)
      let (k,v) = p
      if k == "d" or k == "duration" then duration := float_of_string(v)
      elsif k == "f" or k == "freq" or k == "frequency" then frequency := float_of_string(v)
      elsif k == "s" or k == "shape" then shape := v
      end
    end
    list.iter(set, args)
    def synth(s)
      file = file.temp("liq-synth",".wav")
      log.info(label="synth", "Synthesizing #{!shape} in #{file}.")
      source.dump(%wav, file, once(s))
      [file]
    end
    if !shape == "sine" then
      synth(sine(duration=!duration, !frequency))
    elsif !shape == "saw" then
      synth(saw(duration=!duration, !frequency))
    elsif !shape == "square" then
      synth(square(duration=!duration, !frequency))
    elsif !shape == "blank" then
      synth(blank(duration=!duration))
    else
      []
    end
  end
end

protocol.add(static=true, temporary=true, "synth", synth_protocol,
doc="Syntesize audio. Parameters are optional.",
syntax="synth:shape=sine,frequency=440.,duration=10.")

# File protocol
# @flag hidden
def file_protocol(~rlog=_, ~maxtime=_, arg) =
  path = list.nth(default="", r/:/.split(arg), 1)
  segments = r/\//.split(path)

  segments =
    if list.length(segments) > 3 and
       list.nth(default="foo",segments,0) == "" and
       list.nth(default="foo",segments,1) == ""
    then
      list.tl(list.tl(segments))
    else
      segments
    end

  [url.decode(string.concat(separator="/",segments))]
end

protocol.add(static=true, temporary=false, "file", file_protocol,
             doc="File protocol. Only local files are supported",
             syntax="file:///path/to/file")
