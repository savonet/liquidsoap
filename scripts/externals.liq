# Decoders, enabled when the binary is detected and the os is not Win32.

%ifdef add_decoder
# Enable external Musepack decoder. Requires the 
# mpcdec binary in the path. Does not work on 
# Win32.
# @category Liquidsoap
def enable_external_mpc_decoder() =
  # A list of know extensions and content-type for Musepack.
  # Values from http://en.wikipedia.org/wiki/Musepack 
  mpc_mimes = [ "audio/x-musepack", "audio/musepack" ]
  mpc_filexts = [ "mpc", "mp+", "mpp" ]

  def test_mpc(file) =
    def get_channels(file) =
      int_of_string(
        list.hd(
          get_process_lines("mpcdec -i #{quote(file)} 2>&1 \
                             | grep channels | cut -d' ' -f 2")))
    end
    # Get the file's mime
    mime = get_mime(file)
    # Test mime
    if list.mem(mime,mpc_mimes) then
      get_channels(file)
    else
      # Otherwise test file extension
      ret = string.extract(pattern='\.(.+)$',file)
      if list.length(ret) != 0 then
         ext = ret["1"]
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

  if test_process("which mpcdec") then
    log(level=3,"Found mpcdec binary: enabling musepack external decoder.")
    mpcdec_p = fun(f) -> "mpcdec #{quote(f)} - 2>/dev/null"
    add_oblivious_decoder(name="MPCDEC",description="Decode files using the mpcdec \
                musepack decoder binary",test=test_mpc,mpcdec_p)
  else
    log(level=3,"Did not find mpcdec binary: musepack decoder disabled.")
  end

end 

# Enable external FLAC decoders. Requires flac binary
# in the path for audio decoding and metaflac binary for 
# metadata. Does not work on Win32. Default: disabled.
# Please note that built-in support for FLAC is available
# in liquidsoap if compiled and should be preferred over
# the external decoder.
# @category Liquidsoap
def enable_external_flac_decoder() = 
  if test_process("which flac") then
    log(level=3,"Found flac binary: enabling flac external decoder.")
    flac_p = "flac -d -c - 2>/dev/null"
    def test_flac(file) =
      if test_process("which metaflac") then
        channels = list.hd(get_process_lines("metaflac \
                                              --show-channels #{quote(file)} \
                                              2>/dev/null"))
        # If the value is not an int, this returns 0 and we are ok :)
        int_of_string(channels)
      else
        if string.match(pattern="flac",file) then
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
                decoder binary.", test=test_flac,flac_p)
  else
    log(level=3,"Did not find flac binary: flac decoder disabled.")
  end

  if test_process("which metaflac") then
   log(level=3,"Found metaflac binary: enabling flac external metadata \
                resolver.")
   def flac_meta(file)
     ret = get_process_lines("metaflac --export-tags-to=- \
                  #{quote(file)} 2>/dev/null")
     ret = list.map(string.split(separator="="),ret)
     # Could be made better..
     def f(l',l)=
       if list.length(l) >= 2 then
         list.append([(list.hd(l),list.nth(l,1))],l')
       else
         if list.length(l) >= 1 then
           list.append([(list.hd(l),"")],l')
         else
         l'
       end
     end
   end
   list.fold(f,[],ret)
   end
   add_metadata_resolver("EXTERNAL_FLAC",flac_meta)
  else
    log(level=3,"Did not find metaflac binary: flac metadata resolver disabled.")
  end
end
%endif

%ifdef add_oblivious_decoder
# Enable or disable external FAAD (AAC/AAC+/M4A) decoders. 
# Requires faad binary in the path for audio decoding and 
# metaflac binary for metadata. Does not work on Win32.
# Please note that built-in support for faad is available
# in liquidsoap if compiled and should be preferred over
# the external decoder.
# @category Liquidsoap
def enable_external_faad_decoder() =

  # A list of know extensions and content-type for AAC.
  # Values from http://en.wikipedia.org/wiki/Advanced_Audio_Coding 
  # TODO: can we register a setting for that ??
  aac_mimes =
    ["audio/aac", "audio/aacp", "audio/3gpp", "audio/3gpp2", "audio/mp4",
     "audio/MP4A-LATM", "audio/mpeg4-generic", "audio/x-hx-aac-adts"]
  aac_filexts = ["m4a", "m4b", "m4p", "m4v",
                 "m4r", "3gp", "mp4", "aac"]

  # Faad is not very selective so
  # We are checking only file that
  # end with a known extension or mime type
  def faad_test(file) = 
    # Get the file's mime
    mime = get_mime(file)
    # Test mime
    if list.mem(mime,aac_mimes) then
      true
    else
      # Otherwise test file extension
      ret = string.extract(pattern='\.(.+)$',file)
      if list.length(ret) != 0 then
         ext = ret["1"]
         list.mem(ext,aac_filexts)
       else
         false
       end
    end
  end

  if test_process("which faad") then
    log(level=3,"Found faad binary: enabling external faad decoder and \
                 metadata resolver.")
    faad_p = (fun (f) -> "faad -w #{quote(f)} 2>/dev/null")
    def test_faad(file) =
      if faad_test(file) then
        channels = list.hd(get_process_lines("faad -i #{quote(file)} 2>&1 | \
                                              grep 'ch,'"))
        ret = string.extract(pattern=", (\d) ch,",channels)
        ret = 
          if list.length(ret) == 0 then
          # If we pass the faad_test, chances are
          # high that the file will contain aac audio data..
            "-1"
          else
            ret["1"]
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
        ret = get_process_lines("faad -i \
                     #{quote(file)} 2>&1")
        # Yea, this is ugly programming (again) !
        def get_meta(l,s)=
          ret = string.extract(pattern="^(\w+):\s(.+)$",s)
          if list.length(ret) > 0 then
            list.append([(ret["1"],ret["2"])],l)
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
    log(level=3,"Did not find faad binary: faad decoder disabled.")
  end
end
%endif

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
# @param ~color    Color of the text.
# @param ~position Position of the text (top|middle|bottom).
# @param ~font     Font used (xfontsel is your friend...)
# @param ~display  Function used to display a metadata packet.
# @category Source / Track Processing
def osd_metadata(~color="green",~position="top",
        ~font="-*-courier-*-r-*-*-*-240-*-*-*-*-*-*",
        ~display=string_of_metadata,
        s)
  osd = 'osd_cat -p #{position} --font #{quote(font)}'
      ^ ' --color #{color}'
  def feedback(m)
    system("echo #{quote(display(m))} | #{osd} &")
  end
  on_metadata(feedback,s)
end

# Use notify to display metadata info.
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
       ^ ' -t #{time} #{quote(title)} '
  on_metadata(fun (m) -> system(send^quote(display(m))),s)
end

%ifdef input.external
# Stream data from mplayer
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
  input.external(id=id,restart=restart,
                 restart_on_error=restart_on_error,
                 buffer=buffer,max=max,
                 "mplayer -really-quiet -ao pcm:file=/dev/stdout \
                    -vc null -vo null #{quote(s)} 2>/dev/null")
end
%endif
