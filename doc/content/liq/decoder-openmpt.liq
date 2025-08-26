options = ""

# BEGIN
decoder.add(
  name="OPENMPT123",
  description="Decode files using the openmpt123 decoder binary",
  mimes=[
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
  file_extensions=[
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
  fun (~rlog, ~maxtime:_, infile) ->
    begin
      ret =
        process.read.lines(
          "openmpt123 --info #{process.quote(infile)} 2>&1"
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
            "openmpt123 --assume-terminal --quiet --force #{options} --output #{
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
