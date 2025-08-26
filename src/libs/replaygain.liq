let file.replaygain = ()

# Compute the ReplayGain for a file (in dB).
# @category File
# @param ~id Force the value of the source ID.
# @param ~ratio Decoding ratio. A value of `50` means try to decode the file `50x` faster than real time, if possible.
#               Use this setting to lower CPU peaks when computing replaygain tags.
# @param file_name File name.
# @flag hidden
def file.replaygain.compute(~ratio=50., file_name) =
  _request = request.create(resolve_metadata=false, file_name)
  if
    request.resolve(_request)
  then
    get_gain = ref(fun () -> null)
    def process(s) =
      s = source.replaygain.compute(s)
      get_gain := {s.gain()}
      s
    end

    request.process(ratio=ratio, process=process, _request)

    fn = get_gain()
    fn()
  else
    null
  end
end

# Extract the ReplayGain from the metadata (in dB).
# @category Metadata
# @param _metadata Metadata from which the ReplayGain should be extracted.
def metadata.replaygain(_metadata) =
  if
    list.assoc.mem("r128_track_gain", _metadata)
  then
    try
      float_of_string(_metadata["r128_track_gain"]) / 256.
    catch _ do
      null
    end
  elsif
    list.assoc.mem("replaygain_track_gain", _metadata)
  then
    replaygain_metadata = _metadata["replaygain_track_gain"]
    match = r/([+-]?\d*\.?\d*)/.exec(replaygain_metadata)
    try
      float_of_string(list.assoc(1, match))
    catch _ do
      null
    end
  else
    null
  end
end

# Get the ReplayGain for a file (in dB).
# @category File
# @param ~id Force the value of the source ID.
# @param ~compute Compute ReplayGain if metadata tag is empty.
# @param ~ratio Decoding ratio. A value of `50` means try to decode the file `50x` faster than real time, if possible.
#               Use this setting to lower CPU peaks when computing replaygain tags.
# @param file_name File name.
def replaces file.replaygain(~id=null, ~compute=true, ~ratio=50., file_name) =
  id = string.id.default(default="file.replaygain", id)
  file_name_quoted = string.quote(file_name)

  _metadata = file.metadata(exclude=decoder.metadata.reentrant(), file_name)
  gain = metadata.replaygain(_metadata)

  if
    gain != null
  then
    log.info(
      label=id,
      "Detected track replaygain #{gain} dB for #{file_name_quoted}."
    )
    gain
  elsif
    compute
  then
    log.info(
      label=id,
      "Computing replay gain for #{file_name_quoted}."
    )
    start_time = time()
    gain = file.replaygain.compute(ratio=ratio, file_name)
    elapsed_time = time() - start_time
    if
      gain != null
    then
      log.info(
        label=id,
        "Computed replay gain #{gain} dB for #{file_name_quoted} (time: #{
          elapsed_time
        } s)."
      )
    end
    gain
  else
    null
  end
end

# Enable ReplayGain metadata resolver. This resolver will process any file
# decoded by Liquidsoap and add a `replaygain_track_gain` metadata when this
# value could be computed. For a finer-grained replay gain processing, use the
# `replaygain:` protocol.
# @param ~compute Compute replaygain if metadata tag is empty.
# @param ~ratio Decoding ratio. A value of `50.` means try to decode the file `50x` faster than real time, if possible. Use this setting to lower CPU peaks when computing replaygain tags.
# @category Liquidsoap
def enable_replaygain_metadata(~compute=true, ~ratio=50.) =
  def replaygain_metadata(~metadata:_, file_name) =
    gain = file.replaygain(compute=compute, ratio=ratio, file_name)
    if
      gain != null
    then
      [
        (
          settings.normalize_track_gain_metadata(),
          "#{null.get(gain)} dB"
        )
      ]
    else
      []
    end
  end

  decoder.metadata.add(
    reentrant=true,
    "replaygain_track_gain",
    replaygain_metadata
  )
end
