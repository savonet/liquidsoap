# Enable the external ffmpeg decoder.
# @category Liquidsoap
# @param ~file_extensions File extensions to decode. Should not be empty
# @param ~mimes Mime types to decode. Empty list means any type.
# @param ~binary Path to the `ffmpeg` binary.
def enable_external_ffmpeg_decoder(~binary="ffmpeg", ~mimes, ~file_extensions) =
  decoder.add(
    name="FFMPEG",
    description=
      "Decode files using the ffmpeg decoder binary",
    mimes=mimes,
    file_extensions=file_extensions,
    fun (~rlog, ~maxtime:_, fname) ->
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
  decoder.add(
    name="OPENMPT123",
    description=
      "Decode files using the openmpt123 decoder binary",
    mimes=mimes,
    file_extensions=file_extensions,
    fun (~rlog, ~maxtime:_, infile) ->
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
