# Decoders, enabled when the binary is detected and the os is not Win32.

let settings.decoder.external = settings.make.void(
  "External decoders settings"
)

# @flag hidden
def settings.make.external(name) =
  settings.make.void("Settings for the #{name} external decoder")
end

let settings.decoder.external.ffmpeg = settings.make.external("FFmpeg")

let settings.decoder.external.ffmpeg.path = settings.make(
  description="Path to ffmpeg binary",
  "ffmpeg#{exe_ext}"
)

let settings.decoder.external.ffprobe = settings.make.external("FFprobe")

let settings.decoder.external.ffprobe.path = settings.make(
  description="Path to ffprobe binary",
  "ffprobe#{exe_ext}"
)

let settings.decoder.external.ffmpeg.mime_types = settings.make(
  description="Mime types supported by the external ffmpeg stream decoder",
  []
)

# Enable ffmpeg decoder.
# @category Liquidsoap
def enable_external_ffmpeg_decoder() =
  ffmpeg     = settings.decoder.external.ffmpeg.path()
  ffprobe    = settings.decoder.external.ffprobe.path()
  mime_types = settings.decoder.external.ffmpeg.mime_types()

  def ffprobe_test(fname) =
    j = process.read("#{ffprobe} -print_format json -show_streams #{process.quote(fname)}")

    let json.parse ( parsed : {
      streams : [{
        channels? : int
      }]
    }?) = j

    if null.defined(parsed) then
      parsed = null.get(parsed)

      streams = parsed.streams
      stream = list.find(
        default={},
        (fun (x) -> null.defined(x?.channels)),
        streams
      )

      stream.channels ?? 0
    else
      0
    end
  end

  if file.which(ffmpeg) != null() and file.which (ffprobe) != null() then
   log(label="external.decoder","Enabling FFMPEG decoder")
    decoder.add(
      name="FFMPEG",
      description="Decode files using the ffmpeg decoder binary",
      mimes=mime_types,
      test=ffprobe_test,
      "#{ffmpeg} -i - -f wav - ")
  else
    log(label="external.decoder","Could not find ffmpeg or ffprobe binary. Please adjust using the \"decoder.external.ffmpeg.path\" setting.")
  end
end

let settings.decoder.external.mpcdec = settings.make.external("mpcdec")

let settings.decoder.external.mpcdec.path = settings.make(
  description="Path to mpcdec binary",
  "mpcdec#{exe_ext}"
)

# Enable external Musepack decoder.
# @category Liquidsoap
def enable_external_mpc_decoder() =
  # A list of know extensions and content-type for Musepack.
  # Values from http://en.wikipedia.org/wiki/Musepack
  mpc_mimes = [ "audio/x-musepack", "audio/musepack" ]
  mpc_filexts = [ "mpc", "mp+", "mpp" ]

  mpcdec = settings.decoder.external.mpcdec.path()

  def test_mpc(f) =
    def get_channels(f) =
      int_of_string(
        list.hd(default="",
          process.read.lines("#{mpcdec} -i #{process.quote(f)} 2>&1 \
                             | grep channels | cut -d' ' -f 2")))
    end
    # Get the file's mime
    mime = file.mime(f)
    # Test mime
    if list.mem(mime,mpc_mimes) then
      get_channels(f)
    else
      # Otherwise test file extension
      ret = r/\.(?<ext>.+)$/.exec(f)
      if list.length(ret) != 0 then
        ext = ret.groups["ext"]
        if list.mem(ext,mpc_filexts) then
          get_channels(f)
        else
          0
        end
      else
        get_channels(f)
      end
    end
  end

  if file.which(mpcdec) != null() then
    log(label="external.decoder","Enabling MPCDEC external decoder.")
    mpcdec_p = fun(f) -> "#{mpcdec} #{process.quote(f)} -"
    decoder.oblivious.add(name="MPCDEC",description="Decode files using the mpcdec \
                musepack decoder binary",test=test_mpc,mpcdec_p)
  else
    log(label="external.decoder","Could not find mpcdec binary, please adjust using the \"decoder.external.mpcdec.path\" setting.")
  end
end

let settings.decoder.external.flac = settings.make.external("flac")

let settings.decoder.external.flac.path = settings.make(
  description="Path to flac binary",
  "flac#{exe_ext}"
)

let settings.decoder.external.metaflac = settings.make.external("metaflac")

let settings.decoder.external.metaflac.path = settings.make(
  description="Path to metaflac binary",
  "metaflac#{exe_ext}"
)

# Enable external FLAC decoders. Please note that built-in support for
# FLAC is available in liquidsoap if compiled and should be preferred
# over the external decoder.
# @category Liquidsoap
def enable_external_flac_decoder() =
  flac = settings.decoder.external.flac.path()
  metaflac = settings.decoder.external.metaflac.path()

  if file.which(flac) != null() then
    log(label="external.decoder","Enabling EXTERNAL_FLAC external decoder.")
    flac_p = "#{flac} -d -c -"
    def test_flac(fname) =
      if file.which(metaflac) != null() then
        channels = list.hd(default="",
                           process.read.lines("#{metaflac} \
                                              --show-channels #{process.quote(fname)}"))
        # If the value is not an int, this returns 0 and we are ok :)
        int_of_string(channels)
      else
        if r/flac/.test(fname) then
          # We do not know the number of audio channels
          # so setting to -1
          (-1)
        else
          # All tests failed: no audio decodable using flac..
          0
        end
      end
    end
    decoder.add(name="EXTERNAL_FLAC",description="Decode files using the flac \
                decoder binary.", mimes=["audio/flac", "audio/x-flac"], test=test_flac,flac_p)
  else
    log(label="decoder.external","Did not find flac binary, please adjust using the \"decoder.external.flac.path\" setting.")
  end

  if file.which(metaflac) != null() then
    log(label="decoder.external","Enabling EXTERNAL_FLAC metadata \
                resolver.")
    def flac_meta(~metadata=_, fname)
      ret = process.read.lines("#{metaflac} --export-tags-to=- \
                   #{process.quote(fname)}")
      ret = list.map(r/=/.split, ret)
      # Could be made better..
      def f(l',l)=
        if list.length(l) >= 2 then
          list.append([(list.hd(default="",l),list.nth(default="",l,1))],l')
        else
          if list.length(l) >= 1 then
            list.append([(list.hd(default="",l),"")],l')
          else
            l'
          end
        end
      end
      list.fold(f,[],ret)
    end
    decoder.metadata.add("EXTERNAL_FLAC",flac_meta)
  else
    log(label="decoder.external","Did not find metaflac binary. Please adjust using the \"decoder.external.metaflac.path\" setting.")
  end
end

let settings.decoder.external.faad = settings.make.external("faad")

let settings.decoder.external.faad.path = settings.make(
  description="Path to faad binary",
  "faad#{exe_ext}"
)

# Enable or disable external FAAD (AAC/AAC+/M4A) decoders. Does not work
# on Win32.
# Please note that built-in support for faad is available in liquidsoap if
# compiled and should be preferred over the external decoder.
# @category Liquidsoap
def enable_external_faad_decoder() =
  faad = settings.decoder.external.faad.path()

  # A list of know extensions and content-type for AAC.
  # Values from http://en.wikipedia.org/wiki/Advanced_Audio_Coding
  # TODO: can we register a setting for that ??
  aac_mimes =
    ["audio/aac", "audio/aacp", "audio/3gpp", "audio/3gpp2", "audio/mp4",
     "audio/MP4A-LATM", "audio/mpeg4-generic", "audio/x-hx-aac-adts"]
  aac_filexts = ["m4a", "m4b", "m4p", "m4v",
                 "m4r", "3gp", "mp4", "aac"]

  # Faad is not very selective so we are checking only file that end with a
  # known extension or mime type
  def faad_test(f) =
    # Get the file's mime
    mime = file.mime(f)
    # Test mime
    if list.mem(mime,aac_mimes) then
      true
    else
      # Otherwise test file extension
      ret = r/\.(?<ext>.+)$/.exec(f)
      if list.length(ret) != 0 then
        ext = ret.groups["ext"]
        list.mem(ext,aac_filexts)
      else
        false
      end
    end
  end

  if file.which(faad) != null() then
    log(label="decoder.external","Enabling EXTERNAL_FAAD decoder and \
                 metadata resolver.")
    faad_p = (fun (f) -> "#{faad} -w #{process.quote(f)}")
    def test_faad(file) =
      if faad_test(file) then
        channels = list.hd(default="",
                           process.read.lines("#{faad} -i #{process.quote(file)} 2>&1 | \
                                              grep 'ch,'"))
        ret = r/, (\d) ch,/.exec(channels)
        ret =
          if list.length(ret) == 0 then
            # If we pass the faad_test, chances are high that the file will
            # contain aac audio data..
            "-1"
          else
            ret[1]
          end
        int_of_string(default=(-1),ret)
      else
        0
      end
    end
    decoder.oblivious.add(name="EXTERNAL_FAAD",description="Decode files using \
                          the faad binary.", test=test_faad, faad_p)
    def faad_meta(~metadata=_, file) =
      if faad_test(file) then
        ret = process.read.lines("#{faad} -i #{process.quote(file)} 2>&1")
        # Yea, this is ugly programming (again)!
        def get_meta(l,s)=
          ret = r/^(\w+):\s(.+)$/.exec(s)
          if list.length(ret) > 0 then
            list.append([(ret[1],ret[2])],l)
          else
            l
          end
        end
        list.fold(get_meta,[],ret)
      else
        []
      end
    end
    decoder.metadata.add("EXTERNAL_FAAD",faad_meta)
  else
    log(label="external.decoder","Did not find faad binary. Please adjust using the \"decoder.external.faad.path\" setting.")
  end
end

# Standard function for displaying metadata.
# Shows artist and title, using "Unknown" when a field is empty.
# @param m Metadata packet to be displayed.
# @category String
def string_of_metadata(m)
  artist = m["artist"]
  title  = m["title"]
  artist = if ""==artist then "Unknown" else artist end
  title  = if ""==title  then "Unknown" else title  end
  "#{artist} -- #{title}"
end

# Use X On Screen Display to display metadata info.
# @flag extra
# @param ~color    Color of the text.
# @param ~position Position of the text (top|middle|bottom).
# @param ~font     Font used (xfontsel is your friend...)
# @param ~display  Function used to display a metadata packet.
# @category Source / Track processing
def osd_metadata(~color="green",~position="top",
        ~font="-*-courier-*-r-*-*-*-240-*-*-*-*-*-*",
        ~display=string_of_metadata,
        s)
  osd = 'osd_cat -p #{position} --font #{process.quote(font)}'
      ^ ' --color #{color}'
  def feedback(m)
    ignore(process.run("echo #{process.quote(display(m))} | #{osd} &"))
  end
  s.on_metadata(feedback)
end

# Use notify to display metadata info.
# @flag extra
# @param ~urgency Urgency (low|normal|critical).
# @param ~icon    Icon filename or stock icon to display.
# @param ~timeout Timeout in seconds.
# @param ~display Function used to display a metadata packet.
# @param ~title   Title of the notification message.
# @category Source / Track processing
def notify_metadata(~urgency="low",~icon="stock_smiley-22",~timeout=3.,
           ~display=string_of_metadata,
           ~title="Liquidsoap: new track",s)
  time = int_of_float(timeout * 1000.)
  send = 'notify-send -i #{icon} -u #{urgency}'
       ^ ' -t #{time} #{process.quote(title)} '
  s.on_metadata(fun (m) -> ignore(process.run(send^process.quote(display(m)))))
end

%ifdef input.external.wav
# Stream data from mplayer.
# @flag extra
# @category Source / Input
# @param s data URI.
# @param ~restart restart on exit.
# @param ~restart_on_error restart on exit with error.
# @param ~buffer Duration of the pre-buffered data.
# @param ~max Maximum duration of the buffered data.
# @category Source / Input
def input.mplayer(~id="input.mplayer",
         ~restart=true,~restart_on_error=false,
         ~buffer=0.2,~max=10.,s) =
  input.external.wav(id=id,restart=restart,
                     restart_on_error=restart_on_error,
                     buffer=buffer,max=max,
                     "mplayer -really-quiet \
                       -af resample=#{audio.samplerate()},channels=#{audio.channels()} \
                       -ao pcm:file=/dev/stdout \
                       -vc null -vo null #{process.quote(s)} 2>/dev/null")
end
%endif

# Input from ffmpeg.
# @category Source / Video processing
# @flag extra
# @param ~restart restart on exit.
# @param ~restart_on_error restart on exit with error.
# @param ~buffer Duration of the pre-buffered data.
# @param ~max Maximum duration of the buffered data.
# @param inputopts ffmpeg options specifying the input
def input.external.ffmpeg(~id="input.external.ffmpeg", ~show_command=false,
                          ~restart=true,~restart_on_error=false,
                          ~buffer=0.2,~max=10.,inputopts)
  inputopts = (inputopts : string)
  ffmpeg = null.get(settings.decoder.external.ffmpeg.path())
  cmd = "#{ffmpeg} #{inputopts} -f avi \
         -vf \"scale=#{video.frame.width()}:#{video.frame.height()}:\
               force_original_aspect_ratio=decrease,pad=#{video.frame.width()}:\
               #{video.frame.height()}:(ow-iw)/2:(oh-ih)/2\",\
               format=yuv420p,fps=fps=#{video.frame.rate()} \
         -c:v rawvideo -r #{video.frame.rate()} -c:a pcm_s16le -ac 2 \
         -ar #{audio.samplerate()} pipe:1"

  if show_command then log.important(label=id, "command: #{cmd}") end

  (input.external.avi(id=id,restart=restart,
                      restart_on_error=restart_on_error,
                      buffer=buffer,max=max,cmd):source(audio=pcm(stereo),video=canvas))
end

# ffmpeg's test source video (useful for testing and debugging).
# @param ~restart restart on exit.
# @param ~restart_on_error restart on exit with error.
# @param ~buffer Duration of the pre-buffered data.
# @param ~max Maximum duration of the buffered data.
# @category Source / Video processing
# @flag extra
def video.external.testsrc(~id="video.external.testsrc",~restart=true,~restart_on_error=false,~buffer=0.2,~max=10.,~framerate=0)
  framerate = if framerate <= 0 then video.frame.rate() else framerate end
  cmd = "-f lavfi -i testsrc=size=#{video.frame.width()}x#{video.frame.height()}:rate=#{framerate} \
         -f lavfi -i sine=frequency=440:beep_factor=2:sample_rate=#{audio.samplerate()}"

  input.external.ffmpeg(id=id,restart=restart,
                        restart_on_error=restart_on_error,
                        buffer=buffer,max=max,show_command=true,cmd)
end

# Output to ffmpeg.
# @category Source / Output
# @flag extra
# @param ~id Force the value of the source ID.
# @param ~fallible Allow the child source to fail, in which case the output will be (temporarily) stopped.
# @param ~flush Perform a flush after each write.
# @param ~on_start Callback executed when outputting starts.
# @param ~on_stop Callback executed when outputting stops.
# @param ~start Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
# @argsof output.external[reopen_on_metadata,reopen_on_error,reopen_when,reopen_delay,on_reopen]
def output.external.ffmpeg(~id=null(), ~show_command=false, ~flush=false, ~fallible=false,
                           ~on_start={()}, ~on_stop={()}, %argsof(output.external[reopen_on_metadata,reopen_on_error,reopen_when,reopen_delay,on_reopen]),
                           ~start=true, outputopts, s)
  outputopts = (outputopts : string)
  cmd = "ffmpeg -f avi -vcodec rawvideo -r #{video.frame.rate()} -acodec pcm_s16le -i pipe:0 #{outputopts}"
  if show_command then log.important(label="output.external.ffmpeg", "command: #{cmd}") end
  output.external(id=id, flush=flush, fallible=fallible, on_start=on_start, on_stop=on_stop, %argsof(output.external[reopen_on_metadata,reopen_on_error,reopen_when,reopen_delay,on_reopen]),
                  start=start, %avi, cmd, s)
end

# Output a HLS playlist using ffmpeg
# @category Source / Output
# @flag extra
# @param ~id Force the value of the source ID.
# @param ~fallible Allow the child source to fail, in which case the output will be (temporarily) stopped.
# @param ~flush Perform a flush after each write.
# @param ~on_start Callback executed when outputting starts.
# @param ~on_stop Callback executed when outputting stops.
# @param ~start Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
# @param ~playlist Playlist name
# @param ~directory Directory to write to
# @argsof output.external[reopen_on_metadata,reopen_on_error,reopen_when,reopen_delay,on_reopen]
def output.file.hls.ffmpeg(~id=null(), ~flush=false, ~fallible=false,
                           ~on_start={()}, ~on_stop={()}, %argsof(output.external[reopen_on_metadata,reopen_on_error,reopen_when,reopen_delay,on_reopen]),
                           ~start=true, ~playlist="stream.m3u8", ~directory, s)
  width = video.frame.width()
  height = video.frame.height()
  directory = (directory : string)
  cmd = "-profile:v baseline -pix_fmt yuv420p -level 3.0 -s #{width}x#{height} -start_number 0 -hls_time 10 -hls_list_size 0 -f hls #{directory}/#{playlist}"
  output.external.ffmpeg(id=id, flush=flush, fallible=fallible, on_start=on_start, on_stop=on_stop, %argsof(output.external[reopen_on_metadata,reopen_on_error,reopen_when,reopen_delay,on_reopen]),
                         start=start, cmd, s)
end

let output.file.dash = ()

# Output an MPEG-DASH playlist using ffmpeg
# @category Source / Output
# @flag extra
# @param ~id Force the value of the source ID.
# @param ~fallible Allow the child source to fail, in which case the output will be (temporarily) stopped.
# @param ~flush Perform a flush after each write.
# @param ~on_start Callback executed when outputting starts.
# @param ~on_stop Callback executed when outputting stops.
# @param ~start Automatically start outputting whenever possible. If true, an infallible (normal) output will start outputting as soon as it is created, and a fallible output will (re)start as soon as its source becomes available for streaming.
# @param ~playlist Playlist name
# @param ~directory Directory to write to
# @argsof output.external[reopen_on_metadata,reopen_on_error,reopen_when,reopen_delay,on_reopen]
def output.file.dash.ffmpeg(~id=null(), ~flush=false, ~fallible=false,
                           ~on_start={()}, ~on_stop={()},  %argsof(output.external[reopen_on_metadata,reopen_on_error,reopen_when,reopen_delay,on_reopen]),
                           ~start=true, ~playlist="stream.mpd", ~directory, s)
  width = video.frame.width()
  height = video.frame.height()
  samplerate = audio.samplerate()
  cmd = "-map 0 -map 0 -c:a libfdk_aac -c:v libx264 -b:v:0 800k -b:v:1 300k -s:v:1 #{width}x#{height} -profile:v:1 baseline -profile:v:0 main -bf 1 -keyint_min 120 -g 120 -sc_threshold 0 -b_strategy 0 -ar:a:1 #{samplerate} -use_timeline 1 -use_template 1 -window_size 5 -adaptation_sets \"id=0,streams=v id=1,streams=a\" -f dash #{(directory:string)}/#{playlist}"
  output.external.ffmpeg(id=id, flush=flush, fallible=fallible, on_start=on_start, on_stop=on_stop,  %argsof(output.external[reopen_on_metadata,reopen_on_error,reopen_when,reopen_delay,on_reopen]),
                         start=start, show_command=true, cmd, s)
end
