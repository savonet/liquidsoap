let settings.lufs = ()

let settings.lufs.track_gain_target =
  settings.make(
    description=
      "Target LUFS All available autocue implementations",
    -16.
  )

let settings.lufs.integrated_metadata =
  settings.make(
    description=
      "Metadata used to store integrated LUFS",
    "liq_integrated_lufs"
  )

let settings.lufs.decoding_ratio =
  settings.make(
    description=
      "Decoding ratio used when decoding integrated LUFS from files",
    50.
  )

let file.lufs = ()

# Compute the LUFS of a file (in dB).
# @category File
# @param ~id Force the value of the source ID.
# @param ~ratio Decoding ratio. A value of `50` means try to decode the file `50x` faster than real time, if possible. Use this setting to lower CPU peaks when computing lufs tags. Defaults to `settings.lufs.decoding_ratio` when `null`
# @param file_name File name.
# @flag hidden
def file.lufs.compute(~ratio=null, file_name) =
  ratio = ratio ?? settings.lufs.decoding_ratio()
  _request = request.create(resolve_metadata=false, file_name)
  if
    request.resolve(_request)
  then
    get_lufs = ref(fun () -> null)
    def process(s) =
      s = lufs(s)
      get_lufs := {s.lufs_integrated()}
      s
    end

    request.process(ratio=ratio, process=process, _request)

    fn = get_lufs()
    fn()
  else
    null
  end
end

# Extract the LUFS from the metadata (in dB).
# @category Metadata
# @param _metadata Metadata from which the LUFS should be extracted.
def metadata.lufs(_metadata) =
  k = settings.lufs.integrated_metadata()
  if
    list.assoc.mem(k, _metadata)
  then
    lufs_metadata = _metadata[k]
    match = r/([+-]?\d*\.?\d*)/.exec(lufs_metadata)
    try
      float_of_string(list.assoc(1, match))
    catch _ do
      null
    end
  else
    null
  end
end

# Get the LUFS for a file (in dB).
# @category File
# @param ~id Force the value of the source ID.
# @param ~compute Compute LUFS if metadata tag is empty.
# @param ~ratio Decoding ratio. A value of `50` means try to decode the file `50x` faster than real time, if possible. Use this setting to lower CPU peaks when computing lufs tags. Defaults to `settings.lufs.decoding_ratio` when `null`.
# @param file_name File name.
def replaces file.lufs(~id=null, ~compute=true, ~ratio=null, file_name) =
  id = string.id.default(default="file.lufs", id)
  file_name_quoted = string.quote(file_name)
  ratio = ratio ?? settings.lufs.decoding_ratio()

  _metadata = file.metadata(exclude=decoder.metadata.reentrant(), file_name)
  gain = metadata.lufs(_metadata)

  if
    gain != null
  then
    log.info(
      label=id,
      "Detected track lufs #{gain} dB for #{file_name_quoted}."
    )
    gain
  elsif
    compute
  then
    log.info(
      label=id,
      "Computing integrated LUFS for #{file_name_quoted}."
    )
    start_time = time()
    gain = file.lufs.compute(ratio=ratio, file_name)
    elapsed_time = time() - start_time
    if
      gain != null
    then
      log.info(
        label=id,
        "Computed integrated LUFS of #{gain} dB for #{file_name_quoted} (time: #{
          elapsed_time
        } s)."
      )
    end
    gain
  else
    null
  end
end

# Enable LUFS metadata resolver. This resolver will process any file
# decoded by Liquidsoap and add a `lufs_track_gain` metadata when this
# value could be computed. For a finer-grained replay gain processing, use the
# `lufs:` protocol.
# @param ~compute Compute lufs if metadata tag is empty.
# @param ~ratio Decoding ratio. A value of `50.` means try to decode the file `50x` faster than real time, if possible. Use this setting to lower CPU peaks when computing lufs tags. Defaults to `settings.lufs.decoding_ratio` when `null`
# @category Liquidsoap
def enable_lufs_track_gain_metadata(~compute=true, ~ratio=null) =
  ratio = ratio ?? settings.lufs.decoding_ratio()
  def lufs_metadata(~metadata:_, file_name) =
    gain = file.lufs(compute=compute, ratio=ratio, file_name)
    if
      gain != null
    then
      [
        (
          settings.normalize_track_gain_metadata(),
          "#{settings.lufs.track_gain_target() - null.get(gain)} dB"
        )
      ]
    else
      []
    end
  end

  decoder.metadata.add(reentrant=true, "lufs_track_gain", lufs_metadata)
end
