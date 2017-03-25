register(name="Protocol Settings","protocol",())

register(name="Replay_gain protocol settings","protocol.replay_gain",())
register(name="Replay_gain path","protocol.replay_gain.path","#{configure.libdir}/extract-replaygain")

# Register the replaygain protocol.
# @flag hidden
def replaygain_protocol(~rlog,arg,delay)
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
         ["LANG","PATH"])

# Register the process protocol. Syntax:
# process:<output ext>,<cmd>:uri where <cmd> is interpolated with:
# [("input",<input file>),("output",<output file>),("colon",":")]
# See say: protocol for an example.
# @flag hidden
def process_protocol(~rlog,arg,delay)
  log = log(label="procol.process")

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

    def get_env(name) =
       env = getenv(name)
      "#{name}=#{env}"
    end
    env = list.map(get_env,
      get(default=["LANG","PATH"],"protocol.process.env"))

    if test_process(timeout=delay,env=env,cmd) then
      [output]
    else
      log(level=3,"Failed to execute: #{cmd}")
      []
    end
  end

  if uri == "" then
    resolve("")
  else
    r = request.create.raw(uri)
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

register(name="Youtube_dl protocol settings","protocol.youtube-dl",())
register(name="Youtube-dl path","protocol.youtube-dl.path","youtube-dl")

# Register the youtube-dl protocol, using youtube-dl.
# Syntax: youtube-dl:<ID>
# @flag hidden
def youtube_dl_protocol(~rlog,arg,delay)
  binary = get(default="youtube-dl","protocol.youtube-dl.path")
  tmp = file.temp("liq-youtube",".%(ext)s")
  tmp_wav = string.replace(pattern="%\(ext\)s",fun (_) -> "wav",tmp)
  ["process:wav,#{binary} --quiet -f bestaudio -x --audio-format wav -o #{quote(tmp)} #{quote(arg)} && mv #{quote(tmp_wav)} $(output)"]
end
add_protocol("youtube-dl", youtube_dl_protocol,
              doc="Resolve a request using youtube-dl. Needs ffmpeg in the path.",
              syntax="youtube-dl:uri")

register(name="Text2wave protocol settings","protocol.text2wave",())
register(name="Text2wave path","protocol.text2wave.path","text2wave")

# Register the text2wave: protocol using text2wav
# @flag hidden
def text2wave_protocol(~rlog,arg,delay) =
  binary = get(default="text2wave","protocol.text2wave.path")
  ["process:wav,echo #{quote(arg)} | #{binary} -scale 1.9 > $(output)"]
end
add_protocol(static=true,"text2wave",text2wave_protocol,
             doc="Generate speech synthesis using text2wave. Result may be mono.",
             syntax="text2wav:Text to read")

register(name="Say protocol settings","protocol.say",())
register(name="Sox path","protocol.text2wave.sox_path","sox")

# Register the legacy say: protocol using text2wav and sox
# @flag hidden
def say_protocol(~rlog,arg,delay) =
  sox = get(default="sox","protocol.say.sox_path")
  ["process:wav,#{sox} $(input) -c 2 $(output):text2wave:#{arg}"]
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
# flag @hidden
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
def s3_protocol(~rlog,arg,delay) =
  extname = file.extension(dir_sep="/",arg)
  ["process:#{extname},#{aws_base()} s3 cp s3:#{arg} $(output)"]
end
add_protocol("s3",s3_protocol,doc="Fetch files from s3 using the AWS CLI",
             syntax="s3://uri")

# Register the polly: protocol using AWS Polly
# speech synthesis services. Syntax: polly:<text>
# @flag hidden
def polly_protocol(~rlog,text,delay) =
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

  ["process:#{extname},#{cmd}"]
end
add_protocol(static=true,"polly",polly_protocol,
             doc="Generate speech synthesis using AWS polly service. \
                  Result might be mono, needs aws binary in the path.",
             syntax="polly:Text to read")
