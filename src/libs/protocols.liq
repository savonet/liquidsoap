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
def protocol.replaygain(~rlog=_,~maxtime=_,arg)
  gain = file.replaygain(arg)
  tag = settings.protocol.replaygain.tag()
  if null.defined(gain) then
    ["annotate:#{tag}=\"#{null.get(gain)} dB\":#{arg}"]
  else
    [arg]
  end
end
protocol.add("replaygain", protocol.replaygain,
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

let protocol.process = ()

# Parse process protocol arguments
# @flag hidden
def protocol.process.parse(~default_timeout,arg) =
  let [args, ...uri] = r/:/.split(arg)
  uri = string.concat(separator=":", uri)
  args = r/,/.split(args)

  let args =
    if string.contains(prefix="timeout=", list.hd(args)) then
      let [timeout, extname, ...cmd] = args
      timeout = string.residual(prefix="timeout=", timeout)
      timeout = null.map(string.to_float, timeout) ?? default_timeout
      timeout = min(default_timeout, timeout)
      {timeout=timeout, extname=extname, cmd=cmd}
    else
      let [extname, ...cmd] = args
      {timeout=default_timeout,  extname=extname, cmd=cmd}
    end

  args.{uri=uri,cmd=string.concat(separator=",",args.cmd)}
end

# Register the process protocol. Syntax:
# process:[timeout=<seconds>],<output ext>,<cmd>:uri where `timeout` argument is optional and
# cannot exceed the underlying time and <cmd> is interpolated with:
# [("input",<input file>),("output",<output file>),("colon",":")]
# See say: protocol for an example.
# @flag hidden
def replaces protocol.process(~rlog=_,~maxtime,arg)
  log.info("Processing #{arg}")

  let {uri, timeout, extname, cmd} = protocol.process.parse(default_timeout=maxtime-time(),arg)

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
protocol.add(temporary=true, "process", protocol.process,
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
def process.uri(~timeout=null(),~extname,~uri="",cmd) =
  timeout = null.case(timeout, {""}, fun(t) -> "timeout=" ^ string(t) ^ ",")
  cmd = r/:/.replace(fun (_) -> "$(colon)",cmd)
  uri = if uri != "" then ":#{uri}" else "" end
  "process:#{timeout}#{extname},#{cmd}#{uri}"
end

# Resolve http(s) URLs using curl
# @flag hidden
def protocol.http(proto,~rlog,~maxtime,arg) =
  uri = "#{proto}:#{arg}"

  def log(~level,s) =
    rlog(s)
    log(label="procol.external",level=level,s)
  end

  timeout = maxtime - time()

  ret = http.head(timeout=timeout, uri)
  code = ret.status_code ?? 999
  extname = 200 <= code and code < 300 ? http.headers.extname(ret.headers) : null()

  extname =
    if null.defined(extname) then null.get(extname) else begin
      content_type = http.headers.content_type(ret.headers)
      extra_log =
        if null.defined(content_type) and null.get(content_type).mime != "" then begin
          content_type = null.get(content_type).mime
          " Response has unknown mime-type: #{string.quote(content_type)} you may want to add it to `settings.http.mime.extnames` and report to us if it is a common one."
        end else
          ""
        end
      log(level=3, "Failed to find a file extension for #{string.quote(uri)}.#{extra_log}")
      ".osb"
    end
  end

  output = file.temp("liq-process", extname)

  file_writer = file.write.stream(output)

  timeout = maxtime - time()

  try
    response = http.get.stream(
      on_body_data=file_writer,
      timeout=timeout,
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
def protocol.add.http(proto) =
  def protocol.http(~rlog,~maxtime,arg) = protocol.http(proto,rlog=rlog,maxtime=maxtime,arg) end
  protocol.add(temporary=true,syntax="#{proto}://...",doc="Download http URLs using curl",proto,protocol.http)
end
list.iter(protocol.add.http,["http","https"])

let settings.protocol.youtube_dl = settings.make.protocol("youtube-dl")

let settings.protocol.youtube_dl.path = settings.make(
  description="Path of the youtube-dl (or yt-dlp) binary.",
  "yt-dlp"
)

let settings.protocol.youtube_dl.timeout = settings.make(
  description="Timeout (in seconds) for youtube-dl executions.",
  300.
)

# Register the youtube-dl protocol, using youtube-dl.
# Syntax: youtube-dl:<ID>
# @flag hidden
def protocol.youtube_dl(~rlog,~maxtime,arg)
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
  cmd = process.uri(timeout=timeout, extname=ext, cmd)

  if title != "" then
    ["annotate:title=#{string.quote(title)}:#{cmd}"]
  else
    [cmd]
  end
end
protocol.add("youtube-dl", protocol.youtube_dl,
              doc="Resolve a request using youtube-dl.",
              syntax="youtube-dl:uri")

# Register the youtube-pl protocol.
# Syntax: youtube-pl:<ID>
# @flag hidden
def protocol.youtube_pl(~rlog=_,~maxtime,arg)
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
protocol.add("youtube-pl", protocol.youtube_pl,
              doc="Resolve a request as a youtube playlist using youtube-dl. You typically want to use this as `playlist(\"youtube-pl:...\")`.",
              temporary=true,syntax="youtube-pl:uri")

# Register tmp
# @flag hidden
def protocol.tmp(~rlog=_, ~maxtime=_, arg) =
  [arg]
end
protocol.add("tmp",protocol.tmp,
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
def protocol.ffmpeg(~rlog,~maxtime,arg) =
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

    uri = process.uri(extname="wav",uri=filename,cmd)

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
protocol.add("ffmpeg",protocol.ffmpeg,
             doc="Decode any file to wave using ffmpeg",
             syntax="ffmpeg:uri")

# Register stereo protocol which converts a file to stereo (currently decodes as
# wav).
# @flag hidden
def protocol.stereo(~rlog=_, ~maxtime=_, arg)
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
protocol.add(static=true, temporary=true, "stereo", protocol.stereo, doc="Convert a file to stereo (currently decodes to wav).", syntax="stereo:<uri>")

# Text2wave

let settings.protocol.text2wave = settings.make.protocol("text2wave")

let settings.protocol.text2wave.path = settings.make(
  description="Path to the text2wave binary",
  "text2wave"
)

# Register the text2wave: protocol using text2wave
# @flag hidden
def protocol.text2wave(~rlog=_, ~maxtime=_, arg) =
  binary = settings.protocol.text2wave.path()
  [process.uri(extname="wav", "echo #{process.quote(arg)} | #{binary} -scale 1.9 > $(output)")]
end
protocol.add(static=true,"text2wave",protocol.text2wave,
             doc="Generate speech synthesis using text2wave. Result may be mono.",
             syntax="text2wave:Text to read")

# Pico2wave

let settings.protocol.pico2wave = settings.make.protocol("pico2wave")

let settings.protocol.pico2wave.path = settings.make(
  description="Path to the pico2wave binary",
  "pico2wave"
)

let settings.protocol.pico2wave.lang = settings.make(
  description="pico2wave language. One of: `\"en-US\"`, `\"en-GB\"`, `\"es-ES\"`, `\"de-DE\"`, `\"fr-FR\"` or `\"it-IT\"`.",
  "en-US"
)

# @flag hidden
def protocol.pico2wave(~rlog=_, ~maxtime=_, arg) =
  binary = settings.protocol.pico2wave.path()
  lang = settings.protocol.pico2wave.lang()
  [process.uri(extname="wav", "#{binary} -l #{lang} -w $(output) #{process.quote(arg)}")]
end
protocol.add(static=true,"pico2wave",protocol.pico2wave,
             doc="Generate speech synthesis using pico2wave. Result may be mono.",
             syntax="pico2wave:Text to read")

# GTTS

let settings.protocol.gtts = settings.make.protocol("gtts")

let settings.protocol.gtts.path = settings.make(
  description="Path to the gtts binary",
  "gtts-cli"
)

let settings.protocol.gtts.lang = settings.make(
  description="Language to speak in.",
  "en"
)

let settings.protocol.gtts.options = settings.make(
  description="Command line options.",
  ""
)

# Register the gtts: protocol using gtts
# @flag hidden
def protocol.gtts(~rlog=_, ~maxtime=_, arg) =
  binary = settings.protocol.gtts.path()
  lang = settings.protocol.gtts.lang()
  options = settings.protocol.gtts.options()
  [process.uri(extname="mp3", "#{binary} --lang #{lang} #{options} -o $(output) #{process.quote(arg)}")]
end
protocol.add(static=true,"gtts",protocol.gtts,
             doc="Generate speech synthesis using Google translate's text-to-speech API. This requires the `gtts-cli` binary.  Result may be mono.",
             syntax="gtts:Text to read")

# Say

# Register the legacy say: protocol
# @flag hidden
def protocol.say(~rlog=_, ~maxtime=_, arg) =
  ["pico2wave:#{arg}", "gtts:#{arg}", "text2wave:#{arg}"]
end
protocol.add(static=true,"say",protocol.say,
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
  [process.uri(extname=extname,"#{aws_base()} s3 cp #{arg} $(output)")]
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

  [process.uri(extname=extname,cmd)]
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
      log.info(label="synth", "Synthesizing #{shape()} in #{file}.")
      source.dump(%wav, file, once(s))
      [file]
    end
    if shape() == "sine" then
      synth(sine(duration=duration(), frequency()))
    elsif shape() == "saw" then
      synth(saw(duration=duration(), frequency()))
    elsif shape() == "square" then
      synth(square(duration=duration(), frequency()))
    elsif shape() == "blank" then
      synth(blank(duration=duration()))
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
