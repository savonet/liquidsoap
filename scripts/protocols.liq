register(name="Protocol Settings","protocol",())

register(name="Replay_gain protocol settings","protocol.replay_gain",())
register(name="Replay_gain path","protocol.replay_gain.path","#{configure.libdir}/extract-replaygain")

# Register the replaygain protocol.
# @flag hidden
def replaygain_protocol(~rlog,~maxtime,arg)
 delay = maxtime - gettimeofday()
 # The extraction program
 extract_replaygain =
   get(default="#{configure.libdir}/extract-replaygain","protocol.replay_gain.path")
 x = get_process_lines(timeout=delay,"#{extract_replaygain} #{quote(arg)}")
 if list.hd(default="",x) != "" then
  ["annotate:replay_gain=\"#{list.hd(default='',x)}\":#{arg}"]
 else
  [arg]
 end
end
add_protocol("replay_gain", replaygain_protocol,
             syntax="replay_gain:uri",
             doc="Compute replaygain value using the extract-replaygain script. \
                  Adds returned value as @\"replay_gain\"@ metadata")

register(name="Process protocol settings","protocol.process",())
register(name="Process Environment",
         descr="List of environment variables \
                passed down to the executed process.",
         "protocol.process.env",
         [])
register(name="Inherit Environment",
         descr="Inherit calling process's environment when @env@ parameter is empty.",
         "protocol.process.inherit_env",
         true)

# Register the process protocol. Syntax:
# process:<output ext>,<cmd>:uri where <cmd> is interpolated with:
# [("input",<input file>),("output",<output file>),("colon",":")]
# See say: protocol for an example.
# @flag hidden
def process_protocol(~rlog,~maxtime,arg)
  log = log(label="protocol.process")

  def log(~level,s) =
    rlog(s)
    log(level=level,s)
  end

  log(level=4,"Processing #{arg}")

  x = string.split(separator=":",arg)
  uri = string.concat(separator=":",list.tl(x))
  x = string.split(separator=",",list.hd(default="",x))
  extname = list.hd(default="liq",x)
  cmd = string.concat(separator=",",list.tl(x))

  output = file.temp("liq-process", ".#{extname}")

  def resolve(input) =
    cmd = cmd % [("input",quote(input)),
                 ("output",quote(output)),
                 ("colon",":")]

    log(level=4,"Executing #{cmd}")

    env_vars = get(default=[],"protocol.process.env")
    env = environment()
    def get_env(k) =
      (k,env[k])
    end
    env = list.map(get_env,env_vars)
    inherit_env = get(default=true,"protocol.process.inherit_env")

    delay = maxtime - gettimeofday()
    ret = run_process(timeout=delay,env=env,inherit_env=inherit_env,cmd)
    if snd(ret) == ("exit","0") then
      [output]
    else
      log(level=3,"Failed to execute #{cmd}: #{snd(ret)}")
      []
    end
  end

  if uri == "" then
    resolve("")
  else
    r = request.create.raw(uri)
    delay = maxtime - gettimeofday()
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
add_protocol(temporary=true, "process", process_protocol,
             doc="Resolve a request using an arbitrary process. \
                  @<cmd>@ is interpolated with: \
                  @[(\"input\",<input>),(\"output\",<output>),\
                  (\"colon\",\":\")]@. @uri@ is an optional child request, \
                  @<output>@ is the name of a fresh temporary file and has \
                  extension @.<extname>@. @<input>@ is an optional input \
                  file name as returned while resolving @uri@.",
             syntax="process:<extname>,<cmd>[:uri]")

# Create a process: uri, replacing @:@ with @$(colon)@
# @category Liquidsoap
# @param cmd Command line to execute
# @param ~extname Output file extension (with no leading '.')
# @param ~uri Input uri
def process_uri(~extname,~uri="",cmd) =
  cmd = string.replace(pattern=":",fun (_) -> "$(colon)",cmd)
  uri = if uri != "" then ":#{uri}" else "" end
  "process:#{extname},#{cmd}#{uri}"
end

register(name="External download protocol","protocol.external",true)
register(name="Path to curl","protocol.external.curl","curl")
register(name="External protocols","protocol.external.protocols",["http","https","ftp"])

# Resolve download protocols using curl
# @flag hidden
def download_protocol(proto,~rlog,~maxtime,arg) =
  curl = get(default="curl","protocol.external.curl")
  uri = "#{proto}:#{arg}"

  log = log(label="procol.external")

  def log(~level,s) =
    rlog(s)
    log(level=level,s)
  end

  env_vars = get(default=[],"protocol.process.env")
  env = environment()
  def get_env(k) =
    (k,env[k])
  end
  env = list.map(get_env,env_vars)
  inherit_env = get(default=true,"protocol.process.inherit_env")

  timeout = maxtime - gettimeofday()

  # First define using curl.
  def get_mime() =
    cmd = "#{curl} -sLI -X HEAD #{quote(uri)} | grep -i '^content-type' | tail -n 1 | cut -d':' -f 2 | cut -d';' -f 1"
    log(level=4,"Running #{cmd}")
    x = run_process(timeout=timeout,env=env,inherit_env=inherit_env,cmd)
    if fst(snd(x)) != "exit" or snd(snd(x)) != "0" then
      log(level=3,"Failed to fetch mime-type for #{uri}.")
      ""
    else
      lines = string.split(separator="\\n",fst(fst(x)))
      string.case(lower=true,string.trim(list.hd(default="",lines)))
    end
  end

  def head_mime(~name, ret) =
    def get_mime() =
      status = fst(fst(ret))
      headers = snd(fst(ret))
      code = snd(fst(status))
      if 200 <= code and code < 300 then
        headers["content-type"]
      else
        log(level=3,"Failed to fetch mime-type for #{uri}.")
        ""
      end
    end
    get_mime
  end

  sub = string.sub(uri,start=0,length=5)

%ifdef https.head
  get_mime =
    if sub == "https" then
      log(level=4,"Fetching https head for #{uri}")
      head_mime(name="https",https.head(timeout=timeout,uri))
    else
      get_mime
    end
%endif

  get_mime =
    if sub != "https" then
      log(level=4,"Fetching http head for #{uri}")
      head_mime(name="http",http.head(timeout=timeout,uri))
    else
      get_mime
    end

  mime = get_mime()

  extname =
    if list.mem(mime, ["audio/mpeg", "audio/mp3"]) then
      "mp3"
    elsif list.mem(mime,["application/ogg", "application/x-ogg",
                         "audio/x-ogg", "audio/ogg", "video/ogg"]) then
      "ogg"
    elsif mime == "audio/x-flac" then
      "flac"
    elsif list.mem(mime,["audio/mp4", "application/mp4"]) then
      "mp4"
    elsif list.mem(mime,["audio/vnd.wave", "audio/wav",
                         "audio/wave", "audio/x-wav"]) then
      "wav"
    else
      log(level=3,"No known file extension for mime: #{mime}")
      "osb"
    end
  [process_uri(extname=extname,"#{curl} -sL #{quote(uri)} -o $(output)")]
end

# Register download protocol
# @flag hidden
def add_download_protocol(proto) =
  add_protocol(syntax="#{proto}://...",doc="Download files using curl",proto,download_protocol(proto))
end
if get(default=true,"protocol.external") then
  list.iter(add_download_protocol,get(default=["http","https","ftp"],"protocol.external.protocols"))
end

register(name="Youtube_dl protocol settings","protocol.youtube-dl",())
register(name="Youtube-dl path","protocol.youtube-dl.path","youtube-dl")

# Register the youtube-dl protocol, using youtube-dl.
# Syntax: youtube-dl:<ID>
# @flag hidden
def youtube_dl_protocol(~rlog,~maxtime,arg)
  binary = get(default="youtube-dl","protocol.youtube-dl.path")

  log = log(label="protocol.youtube-dl")

  def log(~level,s) =
    rlog(s)
    log(level=level,s)
  end

  delay = maxtime - gettimeofday()
  cmd = "#{binary} --get-title --get-filename -- #{quote(arg)}"
  log(level=4,"Executing #{cmd}")
  x = get_process_lines(timeout=delay,cmd)

  x =
    if list.length(x) >= 2 then
      x
    else
      ["",".osb"]
    end

  title = list.hd(default="",x)
  ext   = file.extension(list.nth(default="",x,1))
  ext   = string.sub(ext,start=1,length=string.length(ext)-1)

  cmd   = "rm -f $(output) && #{binary} -q -f best --no-playlist -o $(output) -- #{quote(arg)}"
  process = process_uri(extname=ext,cmd)

  if title != "" then
    ["annotate:title=#{quote(title)}:#{process}"]
  else
    [process]
  end
end
add_protocol("youtube-dl", youtube_dl_protocol,
              doc="Resolve a request using youtube-dl.",
              syntax="youtube-dl:uri")

# Register tmp
# @flag hidden
def tmp_protocol(~rlog,~maxtime,arg) =
  [arg]
end
add_protocol("tmp",tmp_protocol,
             doc="Mark the given uri as temporary. Useful when chaining protocols",
             temporary=true,syntax="tmp:uri")

register(name="ffmpeg2wav protocol settings","protocol.ffmpeg2wav",())
register(name="Path to ffmpeg","protocol.ffmpeg2wav.path","ffmpeg")
register(name="Number of channels","protocol.ffmpeg2wav.channels",2)
register(name="Extract metadata","protocol.ffmpeg2wav.metadata",true)

# Register ffmpeg2wav
# @flag hidden
def ffmpeg2wav_protocol(~rlog,~maxtime,arg) =
  ffmpeg   = get(default="ffmpeg","protocol.ffmpeg2wav.path")
  channels = get(default=2,"protocol.ffmpeg2wav.channels")
  metadata = get(default=true,"protocol.ffmpeg2wav.metadata")

  log = log(label="protocol.ffmpeg2wav")

  def log(~level,s) =
    rlog(s)
    log(level=level,s)
  end

  def parse_metadata(file) =
    cmd = "#{ffmpeg} -i #{quote(file)} -f ffmetadata - 2>/dev/null | grep -v '^;'"
    delay = maxtime - gettimeofday()
    log(level=4,"Executing #{cmd}")
    lines = get_process_lines(timeout=delay,cmd)
    def f(cur,line) =
      m = string.split(separator="=",line)
      if list.length(m) >= 2 then
        key = list.hd(default="",m)
        value = string.concat(separator="=",list.tl(m))
        list.add("#{key}=#{quote(value)}",cur)
      else
        cur
      end
    end
    m = string.concat(separator=",",list.fold(f,[],lines))
    if string.length(m) > 0 then
      "annotate:#{m}:"
    else
      ""
    end
  end

  r = request.create.raw(arg)
  delay = maxtime - gettimeofday()
  if request.resolve(timeout=delay,r) then
    res = request.filename(r)

    annotate = if metadata then
      parse_metadata(res)
    else
      ""
    end
   
    # Now parse the audio
    wav = file.temp("liq-process", ".wav")
    cmd = "#{ffmpeg} -y -i $(input) -ac #{channels} #{quote(wav)}"
    uri = process_uri(extname="wav",uri=res,cmd)

    wav_r = request.create.raw(uri)
    delay = maxtime - gettimeofday()
    if request.resolve(timeout=delay,wav_r) then
      request.destroy(r)
      request.destroy(wav_r)
      ["#{annotate}tmp:#{wav}"]
    else
      log(level=3,"Failed to resolve #{uri}")
      request.destroy(r)
      []
    end
  else
    log(level=3,"Failed to resolve #{arg}")
    []
  end
end
add_protocol("ffmpeg2wav",ffmpeg2wav_protocol,
             doc="Decode any file to wave using ffmpeg",
             syntax="ffmpeg2wav:uri")

register(name="Text2wave protocol settings","protocol.text2wave",())
register(name="Text2wave path","protocol.text2wave.path","text2wave")

# Register the text2wave: protocol using text2wav
# @flag hidden
def text2wave_protocol(~rlog,~maxtime,arg) =
  binary = get(default="text2wave","protocol.text2wave.path")
  [process_uri(extname="wav","echo #{quote(arg)} | #{binary} -scale 1.9 > $(output)")]
end
add_protocol(static=true,"text2wave",text2wave_protocol,
             doc="Generate speech synthesis using text2wave. Result may be mono.",
             syntax="text2wav:Text to read")

register(name="Say protocol settings","protocol.say",())
register(name="Sox path","protocol.say.sox_path","sox")

# Register the legacy say: protocol using text2wav and sox
# @flag hidden
def say_protocol(~rlog,~maxtime,arg) =
  sox = get(default="sox","protocol.say.sox_path")
  [process_uri(extname="wav",uri="text2wave:#{arg}","#{sox} $(input) -c 2 $(output)")]
end
add_protocol(static=true,"say",say_protocol,
             doc="Generate speech synthesis using text2wave and sox. Result is always stereo.",
             syntax="say:Text to read")

register(name="AWS protocols settings","protocol.aws",())
register(name="Profile",descr="Use a specific profile from your credential file.",
         "protocol.aws.profile","")
register(name="Region",descr="AWS Region",
         "protocol.aws.region","")
register(name="Binary",descr="Path to aws CLI binary",
         "protocol.aws.path","aws")
register(name="Polly protocol settings","protocol.aws.polly",())
register(name="Format",descr="Output format",
         "protocol.aws.polly.format","mp3")
register(name="Voice",descr="Voice ID",
         "protocol.aws.polly.voice","Joanna")

# Build a aws base call
# @flag hidden
def aws_base() =
  aws = get(default="aws","protocol.aws.path")

  region = get(default="","protocol.aws.region")

  aws =
    if region !="" then
      "#{aws} --region #{region}"
    else
      aws
    end

  profile = get(default="","protocol.aws.profile")

  if profile !="" then
    "#{aws} --profile #{quote(profile)}"
  else
    aws
  end
end

# Register the s3:// protocol 
# @flag hidden
def s3_protocol(~rlog,~maxtime,arg) =
  extname = file.extension(dir_sep="/",arg)
  [process_uri(extname=extname,"#{aws_base()} s3 cp s3:#{arg} $(output)")]
end
add_protocol("s3",s3_protocol,doc="Fetch files from s3 using the AWS CLI",
             syntax="s3://uri")

# Register the polly: protocol using AWS Polly
# speech synthesis services. Syntax: polly:<text>
# @flag hidden
def polly_protocol(~rlog,~maxtime,text) =
  aws = aws_base()

  format = get(default="mp3","protocol.aws.polly.format")

  extname =
    if format == "mp3" then
      "mp3"
    elsif format == "ogg_vorbis" then
      "ogg"
    else
      "wav"
    end

  aws = "#{aws} polly synthesize-speech --output-format #{format}"

  voice_id = get(default="Joanna","protocol.aws.polly.voice")

  cmd = "#{aws} --text #{quote(text)} --voice-id #{quote(voice_id)} $(output)"

  [process_uri(extname=extname,cmd)]
end
add_protocol(static=true,"polly",polly_protocol,
             doc="Generate speech synthesis using AWS polly service. \
                  Result might be mono, needs aws binary in the path.",
             syntax="polly:Text to read")
