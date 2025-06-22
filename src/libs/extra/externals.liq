# Enable the external ffmpeg decoder.
# @category Liquidsoap
# @param ~file_extensions File extensions to decode. Should not be empty
# @param ~mimes Mime types to decode. Empty list means any type.
# @param ~binary Path to the `ffmpeg` binary.
def enable_external_ffmpeg_decoder(~binary="ffmpeg", ~mimes, ~file_extensions) =
  decoder.file.add(
    name="FFMPEG",
    description=
      "Decode files using the ffmpeg decoder binary",
    mimes=mimes,
    file_extensions=file_extensions,
    fun (~rlog, ~maxtime=_, fname) ->
      begin
        # File is cleaned up as part of the request workflow.
        outfile = file.temp(cleanup=false, "ffmpeg", ".wav")
        try
          let {status = {code}} =
            process.run(
              "#{binary} -i #{process.quote(fname)} #{process.quote(outfile)}"
            )
          code == 0 ? outfile : null
        catch err do
          file.remove(outfile)
          rlog(
            "Error while decoding #{fname} using ffmpeg: #{err}"
          )
          null
        end
      end
  )
end

# Enable the external openmpt123 decoder
# @category Liquidsoap
# @param ~file_extensions File extensions to decode.
# @param ~mimes Mime types to decode. Empty list means any type.
# @param ~options Extra options.
# @param ~binary Path to the `ffmpeg` binary.
def enable_external_openmpt123_decoder(
  ~binary="openmpt123",
  ~mimes=[
    "audio/it",
    "audio/xm",
    "audio/s3m",
    "audio/x-mod",
    "audio/mod",
    "audio/module-xm",
    "audio/x-mod",
    "application/playerpro",
    "audio/x-s3m",
    "application/soundapp",
    "audio/med",
    "audio/x-xm"
  ],
  ~file_extensions=[
    "xm",
    "mtm",
    "amf",
    "stm",
    "ult",
    "wow",
    "dmf",
    "it",
    "s3m",
    "far",
    "mod",
    "mt2",
    "okt",
    "med",
    "669"
  ],
  ~options=""
) =
  decoder.file.add(
    name="OPENMPT123",
    description=
      "Decode files using the openmpt123 decoder binary",
    mimes=mimes,
    file_extensions=file_extensions,
    fun (~rlog, ~maxtime=_, infile) ->
      begin
        ret =
          process.read.lines(
            "#{binary} --info #{process.quote(infile)} 2>&1"
          )
        def get_meta(l, s) =
          ret = string.extract(pattern="^(\\w+).+:\\s(.+)$", s)
          if
            list.length(ret) > 2
          then
            label = ret[1]
            val = ret[2]
            label = "openmpt:#{string.case(lower=true, label)}"
            ["#{string.quote(label)}=#{string.quote(val)}", ...l]
          else
            l
          end
        end
        meta = list.fold(get_meta, [], ret)

        prefix =
          if
            meta == []
          then
            ""
          else
            "annotate:#{string.concat(separator=',', meta)}:"
          end

        # File is cleaned up as part of the request workflow.
        outfile = file.temp(cleanup=false, "openmpt", ".wav")
        try
          let {status = {code}} =
            process.run(
              "#{binary} --assume-terminal --quiet --force #{options} --output #{
                process.quote(outfile)
              } #{process.quote(infile)}"
            )
          code == 0 ? "#{prefix}#{outfile}" : null
        catch err do
          file.remove(outfile)
          rlog(
            "Error while decoding #{infile} using ffmpeg: #{err}"
          )
          null
        end
      end
  )
end

# Standard function for displaying metadata.
# Shows artist and title, using "Unknown" when a field is empty.
# @param m Metadata packet to be displayed.
# @category String
def string_of_metadata(m) =
  artist = m["artist"]
  title = m["title"]
  artist = if "" == artist then "Unknown" else artist end
  title = if "" == title then "Unknown" else title end
  "#{artist} -- #{title}"
end

# Use X On Screen Display to display metadata info.
# @flag extra
# @param ~color    Color of the text.
# @param ~position Position of the text (top|middle|bottom).
# @param ~font     Font used (xfontsel is your friend...)
# @param ~display  Function used to display a metadata packet.
# @category Source / Track processing
def osd_metadata(
  ~color="green",
  ~position="top",
  ~font="-*-courier-*-r-*-*-*-240-*-*-*-*-*-*",
  ~display=string_of_metadata,
  s
) =
  osd =
    'osd_cat -p #{position} --font #{process.quote(font)}' ^
      ' --color #{color}'

  def feedback(m) =
    ignore(
      process.run(
        "echo #{process.quote(display(m))} | #{osd} &"
      )
    )
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
def notify_metadata(
  ~urgency="low",
  ~icon="stock_smiley-22",
  ~timeout=3.,
  ~display=string_of_metadata,
  ~title="Liquidsoap: new track",
  s
) =
  time = int_of_float(timeout * 1000.)
  send =
    'notify-send -i #{icon} -u #{urgency}' ^
      ' -t #{time} #{process.quote(title)} '

  s.on_metadata(
    fun (m) -> ignore(process.run(send ^ process.quote(display(m))))
  )
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
def input.mplayer(
  ~id="input.mplayer",
  ~restart=true,
  ~restart_on_error=false,
  ~buffer=0.2,
  ~max=10.,
  s
) =
  input.external.wav(
    id=id,
    restart=restart,
    restart_on_error=restart_on_error,
    buffer=buffer,
    max=max,
    "mplayer -really-quiet -af resample=#{audio.samplerate()},channels=#{
      audio.channels()
    } -ao pcm:file=/dev/stdout -vc null -vo null #{process.quote(s)} 2>/dev/null"
  )
end
%endif

# Input from ffmpeg.
# @category Source / Video processing
# @flag extra
# @param ~restart restart on exit.
# @param ~restart_on_error restart on exit with error.
# @param ~buffer Duration of the pre-buffered data.
# @param ~max Maximum duration of the buffered data.
# @param ~binary Path to the `ffmpeg` binary.
# @param inputopts ffmpeg options specifying the input
def input.external.ffmpeg(
  ~id="input.external.ffmpeg",
  ~show_command=false,
  ~restart=true,
  ~restart_on_error=false,
  ~buffer=0.2,
  ~max=10.,
  ~binary="ffmpeg",
  inputopts
) =
  inputopts = (inputopts : string)
  ffmpeg = binary
  cmd =
    "#{ffmpeg} #{inputopts} -f avi -vf \"scale=#{video.frame.width()}:#{
      video.frame.height()
    }:force_original_aspect_ratio=decrease,pad=#{video.frame.width()}:#{
      video.frame.height()
    }:(ow-iw)/2:(oh-ih)/2\",format=yuv420p,fps=fps=#{video.frame.rate()} -c:v \
     rawvideo -r #{video.frame.rate()} -c:a pcm_s16le -ac 2 -ar #{
      audio.samplerate()
    } pipe:1"

  if
    show_command
  then
    log.important(
      label=id,
      "command: #{cmd}"
    )
  end
  (
    input.external.avi(
      id=id,
      restart=restart,
      restart_on_error=restart_on_error,
      buffer=buffer,
      max=max,
      cmd
    )
  :
    source(audio=pcm(stereo), video=canvas)
  )
end

# ffmpeg's test source video (useful for testing and debugging).
# @param ~restart restart on exit.
# @param ~restart_on_error restart on exit with error.
# @param ~buffer Duration of the pre-buffered data.
# @param ~max Maximum duration of the buffered data.
# @category Source / Video processing
# @flag extra
def video.external.testsrc(
  ~id="video.external.testsrc",
  ~restart=true,
  ~restart_on_error=false,
  ~buffer=0.2,
  ~max=10.,
  ~framerate=0
) =
  framerate = if framerate <= 0 then video.frame.rate() else framerate end
  cmd =
    "-f lavfi -i testsrc=size=#{video.frame.width()}x#{
      video.frame.height()
    }:rate=#{framerate} -f lavfi -i \
     sine=frequency=440:beep_factor=2:sample_rate=#{audio.samplerate()}"

  input.external.ffmpeg(
    id=id,
    restart=restart,
    restart_on_error=restart_on_error,
    buffer=buffer,
    max=max,
    show_command=true,
    cmd
  )
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
def output.external.ffmpeg(
  ~id=null,
  ~show_command=false,
  ~flush=false,
  ~fallible=false,
  ~on_start={()},
  ~on_stop={()},
  %argsof(output.external[
    reopen_on_metadata,
    reopen_on_error,
    reopen_when,
    reopen_delay,
    on_reopen]
  ),
  ~start=true,
  outputopts,
  s
) =
  outputopts = (outputopts : string)
  cmd =
    "ffmpeg -f avi -vcodec rawvideo -r #{video.frame.rate()} -acodec pcm_s16le \
     -i pipe:0 #{outputopts}"

  if
    show_command
  then
    log.important(
      label="output.external.ffmpeg",
      "command: #{cmd}"
    )
  end

  output.external(
    id=id,
    flush=flush,
    fallible=fallible,
    on_start=on_start,
    on_stop=on_stop,
    %argsof(output.external[
      reopen_on_metadata,
      reopen_on_error,
      reopen_when,
      reopen_delay,
      on_reopen]
    ),
    start=start,
    %avi,
    cmd,
    s
  )
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
def output.file.hls.ffmpeg(
  ~id=null,
  ~flush=false,
  ~fallible=false,
  ~on_start={()},
  ~on_stop={()},
  %argsof(output.external[
    reopen_on_metadata,
    reopen_on_error,
    reopen_when,
    reopen_delay,
    on_reopen]
  ),
  ~start=true,
  ~playlist="stream.m3u8",
  ~directory,
  s
) =
  width = video.frame.width()
  height = video.frame.height()
  directory = (directory : string)
  cmd =
    "-profile:v baseline -pix_fmt yuv420p -level 3.0 -s #{width}x#{height} \
     -start_number 0 -hls_time 10 -hls_list_size 0 -f hls #{directory}/#{
      playlist
    }"

  output.external.ffmpeg(
    id=id,
    flush=flush,
    fallible=fallible,
    on_start=on_start,
    on_stop=on_stop,
    %argsof(output.external[
      reopen_on_metadata,
      reopen_on_error,
      reopen_when,
      reopen_delay,
      on_reopen]
    ),
    start=start,
    cmd,
    s
  )
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
def output.file.dash.ffmpeg(
  ~id=null,
  ~flush=false,
  ~fallible=false,
  ~on_start={()},
  ~on_stop={()},
  %argsof(output.external[
    reopen_on_metadata,
    reopen_on_error,
    reopen_when,
    reopen_delay,
    on_reopen]
  ),
  ~start=true,
  ~playlist="stream.mpd",
  ~directory,
  s
) =
  width = video.frame.width()
  height = video.frame.height()
  samplerate = audio.samplerate()
  cmd =
    "-map 0 -map 0 -c:a libfdk_aac -c:v libx264 -b:v:0 800k -b:v:1 300k -s:v:1 #{
      width
    }x#{height} -profile:v:1 baseline -profile:v:0 main -bf 1 -keyint_min 120 -g \
     120 -sc_threshold 0 -b_strategy 0 -ar:a:1 #{samplerate} -use_timeline 1 \
     -use_template 1 -window_size 5 -adaptation_sets \"id=0,streams=v \
     id=1,streams=a\" -f dash #{(directory : string)}/#{playlist}"

  output.external.ffmpeg(
    id=id,
    flush=flush,
    fallible=fallible,
    on_start=on_start,
    on_stop=on_stop,
    %argsof(output.external[
      reopen_on_metadata,
      reopen_on_error,
      reopen_when,
      reopen_delay,
      on_reopen]
    ),
    start=start,
    show_command=true,
    cmd,
    s
  )
end
