# Decoders, enabled when the binary is detected and the os is not Win32.

register(
  name="external decoders",
  descr="External decoders settings",
  "decoder.external",
  ())
register(
  name="ffmpeg path",
  descr="Path to ffmpeg binary",
  "decoder.external.ffmpeg.path",
  "ffmpeg#{exe_ext}")
register(
  name="ffprobe path",
  descr="Path to ffprobe binary",
  "decoder.external.ffprobe.path",
  "ffprobe#{exe_ext}")

register(
  name="Ffmpeg decoder stream mimes",
  descr="Mime types supported by the ffmpeg stream decoder",
  "decoder.mime_types.ffmpeg",
  [])

# Enable ffmpeg decoder.
# @category Liquidsoap
def enable_external_ffmpeg_decoder() =
  ffmpeg     = get(default="ffmpeg#{exe_ext}","decoder.external.ffmpeg.path")
  ffprobe    = get(default="ffprobe#{exe_ext}","decoder.external.ffprobe.path")
  mime_types = get(default=[],"decoder.mime_types.ffmpeg")

  def ffprobe_test(fname) =
    json = process.read("#{ffprobe} -print_format json -show_streams #{string.quote(fname)}")
    default = [("streams",[[("channels",0)]])]
    data = of_json(default=default,json)
    streams = list.assoc(default=[],"streams",data)
    stream = list.hd(default=[],streams)
    list.assoc(default=0,"channels",stream)
  end

  if file.which(ffmpeg) != "" and file.which (ffprobe) != "" then
   log(label="external.decoder","Enabling FFMPEG decoder")
    add_decoder(
      name="FFMPEG",
      description="Decode files using the ffmpeg decoder binary",
      mimes=mime_types,
      test=ffprobe_test,
      "#{ffmpeg} -i - -f wav - ")
  else
    log(label="external.decoder","Could not find ffmpeg or ffprobe binary. Please adjust using the \"decoder.external.ffmpeg.path\" setting.")
  end
end

register(
  name="mpcdec path",
  descr="Path to mpcdec binary",
  "decoder.external.mpcdec.path",
  "mpcdec#{exe_ext}")

# Enable external Musepack decoder.
# @category Liquidsoap
def enable_external_mpc_decoder() =
  # A list of know extensions and content-type for Musepack.
  # Values from http://en.wikipedia.org/wiki/Musepack
  mpc_mimes = [ "audio/x-musepack", "audio/musepack" ]
  mpc_filexts = [ "mpc", "mp+", "mpp" ]

  mpcdec = get(default="mpcdec#{exe_ext}","decoder.external.mpcdec.path") 

  def test_mpc(file) =
    def get_channels(file) =
      int_of_string(
        list.hd(default="",
          process.read.lines("#{mpcdec} -i #{string.quote(file)} 2>&1 \
                             | grep channels | cut -d' ' -f 2")))
    end
    # Get the file's mime
    mime = get_mime(file)
    # Test mime
    if list.mem(mime,mpc_mimes) then
      get_channels(file)
    else
      # Otherwise test file extension
      ret = string.extract(pattern='\\.(.+)$',file)
      if list.length(ret) != 0 then
        ext = ret[1]
        if list.mem(ext,mpc_filexts) then
          get_channels(file)
        else
          0
        end
      else
        get_channels(file)
      end
    end
  end

  if file.which(mpcdec) != "" then
    log(label="external.decoder","Enabling MPCDEC external decoder.")
    mpcdec_p = fun(f) -> "#{mpcdec} #{string.quote(f)} -"
    add_oblivious_decoder(name="MPCDEC",description="Decode files using the mpcdec \
                musepack decoder binary",test=test_mpc,mpcdec_p)
  else
    log(label="external.decoder","Could not find mpcdec binary, please adjust using the \"decoder.external.mpcdec.path\" setting.")
  end
end

register(
  name="flac path",
  descr="Path to flac binary",
  "decoder.external.flac.path",
  "flac#{exe_ext}")
register(
  name="metaflac path",
  descr="Path to metaflac binary",
  "decoder.external.metaflac.path",
  "metaflac#{exe_ext}")

# Enable external FLAC decoders. Please note that built-in support for 
# FLAC is available in liquidsoap if compiled and should be preferred
# over the external decoder.
# @category Liquidsoap
def enable_external_flac_decoder() =
  flac = get(default="flac#{exe_ext}","decoder.external.flac.path")
  metaflac = get(default="metaflac#{exe_ext}","decoder.external.metaflac.path")

  if file.which(flac) != "" then
    log(label="external.decoder","Enabling EXTERNAL_FLAC external decoder.")
    flac_p = "#{flac} -d -c -"
    def test_flac(fname) =
      if file.which(metaflac) != "" then
        channels = list.hd(default="",
                           process.read.lines("#{metaflac} \
                                              --show-channels #{string.quote(fname)}"))
        # If the value is not an int, this returns 0 and we are ok :)
        int_of_string(channels)
      else
        if string.match(pattern="flac",fname) then
          # We do not know the number of audio channels
          # so setting to -1
          (-1)
        else
          # All tests failed: no audio decodable using flac..
          0
        end
      end
    end
    add_decoder(name="EXTERNAL_FLAC",description="Decode files using the flac \
                decoder binary.", mimes=["audio/flac", "audio/x-flac"], test=test_flac,flac_p)
  else
    log(label="decoder.external","Did not find flac binary, please adjust using the \"decoder.external.flac.path\" setting.")
  end

  if file.which(metaflac) != "" then
    log(label="decoder.external","Enabling EXTERNAL_FLAC metadata \
                resolver.")
    def flac_meta(fname)
      ret = process.read.lines("#{metaflac} --export-tags-to=- \
                   #{string.quote(fname)}")
      ret = list.map(string.split(separator="="),ret)
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
    add_metadata_resolver("EXTERNAL_FLAC",flac_meta)
  else
    log(label="decoder.external","Did not find metaflac binary. Please adjust using the \"decoder.external.metaflac.path\" setting.")
  end
end

register(
  name="faad path",
  descr="Path to faad binary",
  "decoder.external.faad.path",
  "faad#{exe_ext}")

# Enable or disable external FAAD (AAC/AAC+/M4A) decoders. Does not work
# on Win32.
# Please note that built-in support for faad is available in liquidsoap if
# compiled and should be preferred over the external decoder.
# @category Liquidsoap
def enable_external_faad_decoder() =
  faad = get(default="faad#{exe_ext}","decoder.external.faad.path")

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
  def faad_test(file) =
    # Get the file's mime
    mime = get_mime(file)
    # Test mime
    if list.mem(mime,aac_mimes) then
      true
    else
      # Otherwise test file extension
      ret = string.extract(pattern='\\.(.+)$',file)
      if list.length(ret) != 0 then
        ext = ret[1]
        list.mem(ext,aac_filexts)
      else
        false
      end
    end
  end

  if file.which(faad) != "" then
    log(label="decoder.external","Enabling EXTERNAL_FAAD decoder and \
                 metadata resolver.")
    faad_p = (fun (f) -> "#{faad} -w #{string.quote(f)}")
    def test_faad(file) =
      if faad_test(file) then
        channels = list.hd(default="",
                           process.read.lines("#{faad} -i #{string.quote(file)} 2>&1 | \
                                              grep 'ch,'"))
        ret = string.extract(pattern=", (\\d) ch,",channels)
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
    add_oblivious_decoder(name="EXTERNAL_FAAD",description="Decode files using \
                          the faad binary.", test=test_faad, faad_p)
    def faad_meta(file) =
      if faad_test(file) then
        ret = process.read.lines("#{faad} -i #{string.quote(file)} 2>&1")
        # Yea, this is ugly programming (again)!
        def get_meta(l,s)=
          ret = string.extract(pattern="^(\\w+):\\s(.+)$",s)
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
    add_metadata_resolver("EXTERNAL_FAAD",faad_meta)
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
# @category Source / Track Processing
def osd_metadata(~color="green",~position="top",
        ~font="-*-courier-*-r-*-*-*-240-*-*-*-*-*-*",
        ~display=string_of_metadata,
        s)
  osd = 'osd_cat -p #{position} --font #{string.quote(font)}'
      ^ ' --color #{color}'
  def feedback(m)
    ignore(process.run("echo #{string.quote(display(m))} | #{osd} &"))
  end
  s.on_metadata(feedback)
end

# Use notify to display metadata info.
# @flag extra
# @param ~urgency Urgency (low|normal|critical).
# @param ~icon    Icon filename or stock icon to display.
# @param ~time    Timeout in milliseconds.
# @param ~display Function used to display a metadata packet.
# @param ~title   Title of the notification message.
# @category Source / Track Processing
def notify_metadata(~urgency="low",~icon="stock_smiley-22",~time=3000,
           ~display=string_of_metadata,
           ~title="Liquidsoap: new track",s)
  send = 'notify-send -i #{icon} -u #{urgency}'
       ^ ' -t #{time} #{string.quote(title)} '
  s.on_metadata(fun (m) -> ignore(process.run(send^string.quote(display(m)))))
end

%ifdef input.external.wav
# Stream data from mplayer
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
                       -vc null -vo null #{string.quote(s)} 2>/dev/null")
end
%endif
