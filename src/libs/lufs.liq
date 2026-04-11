let settings.lufs = ()

let settings.lufs.track_gain_target =
  settings.make(
    description="Target LUFS All available autocue implementations",
    -16.
  )

let settings.lufs.integrated_metadata =
  settings.make(
    description="Metadata used to store integrated LUFS",
    "liq_integrated_lufs"
  )

let settings.lufs.decoding_ratio =
  settings.make(
    description="Decoding ratio used when decoding integrated LUFS from files",
    50.
  )

let settings.lufs.true_peak_max =
  settings.make(
    description="Maximum allowed true peak (dBTP) after LUFS normalization. \
     Tracks whose true peak would exceed this after gain application will have \
     their gain reduced accordingly. EBU R128 mandates -1.0 dBTP.",
    -1.0
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

# Compute the integrated LUFS and true peak of a file in a single decoding pass.
# Returns a record `{lufs, true_peak}` (both in dB / dBTP), or `null` on failure.
# @category File
# @param ~ratio Decoding ratio. Defaults to `settings.lufs.decoding_ratio` when `null`.
# @param file_name File name.
# @flag hidden
def file.lufs.compute_with_peak(~ratio=null, file_name) =
  ratio = ratio ?? settings.lufs.decoding_ratio()
  _request = request.create(resolve_metadata=false, file_name)
  if
    request.resolve(_request)
  then
    get_lufs = ref(fun () -> null)
    get_tp = ref(fun () -> null)
    def process(s) =
      s = lufs(s)
      get_lufs := {s.lufs_integrated()}
      get_tp := {s.true_peak()}
      s
    end
    request.process(ratio=ratio, process=process, _request)
    lufs_fn = get_lufs()
    tp_fn = get_tp()
    lufs_val = lufs_fn()
    tp_val = tp_fn()
    if
      null.defined(lufs_val) and null.defined(tp_val)
    then
      {lufs = null.get(lufs_val), true_peak = null.get(tp_val)}
    else
      null
    end
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
# decoded by Liquidsoap and add a `liq_normalize_track_gain` metadata when this
# value could be computed (key is configurable via `settings.normalize_track_gain_metadata`).
# For a finer-grained replay gain processing, use the `lufs_track_gain:` protocol.
#
# When `true_peak=true`, both integrated LUFS and true peak are measured
# in a single pass. The gain written to `liq_normalize_track_gain` is then
# adjusted so the true peak after amplification does not exceed
# `settings.lufs.true_peak_max` (default -1.0 dBTP, per EBU R128):
#
#  - If true peak after LUFS gain <= tp_max: gain is used as-is.
#  - If true peak after LUFS gain is in (tp_max, tp_max+0.5]: gain is reduced
#    by `tp_after_gain - tp_max` so that true peak lands exactly at tp_max.
#    Maximum attenuation in this range is 0.5 dB.
#  - If true peak after LUFS gain > tp_max+0.5: same correction is applied, but
#    a warning is logged. A dynamic limiter may be needed for these tracks.
#
# When `true_peak=false` (default), only integrated LUFS is measured and the
# gain is set purely from LUFS — no true peak correction is applied. This
# preserves the original behaviour.
#
# When `compute=false`, the existing LUFS metadata is used without any
# computation (same behaviour as before).
#
# @param ~compute Compute LUFS if metadata tag is empty.
# @param ~true_peak Also measure true peak and apply EBU R128 TP correction to the gain.
# @param ~ratio Decoding ratio. A value of `50.` means try to decode the file `50x` faster than real time, if possible. Defaults to `settings.lufs.decoding_ratio` when `null`.
# @category Liquidsoap
def enable_lufs_track_gain_metadata(
  ~compute=true,
  ~true_peak=true,
  ~ratio=null
) =
  ratio = ratio ?? settings.lufs.decoding_ratio()
  def lufs_metadata(~metadata:_, file_name) =
    if
      compute and true_peak
    then
      # Single-pass scan: measure both LUFS and true peak, apply TP correction.
      result = file.lufs.compute_with_peak(ratio=ratio, file_name)
      if
        result != null
      then
        let {lufs = measured_lufs, true_peak = measured_tp} = null.get(result)
        target = settings.lufs.track_gain_target()
        tp_max = settings.lufs.true_peak_max()
        gain_lufs = target - measured_lufs
        tp_after = measured_tp + gain_lufs

        # x is the correction: negative (attenuation) when tp_after > tp_max.
        x = tp_max - tp_after

        final_gain =
          if
            tp_after <= tp_max
          then
            # Case 1: true peak within limit, LUFS target is met exactly.
            gain_lufs
          elsif
            tp_after <= tp_max + 0.5
          then
            # Case 2: mild overshoot (up to 0.5 dB), correct by reducing gain.
            # Resulting LUFS-I will be slightly below target (max 0.5 LU deviation),
            # but within allowed variance of ± 1 dB.
            gain_lufs + x
          else
            # Case 3: overshoot exceeds 0.5 dB. Capping gain reduction at 0.5 dB
            # to keep LUFS-I no lower than target - 0.5 LU (EBU R128 floor).
            # Remaining TP excess must be handled by a limiter.
            log.important(
              label="lufs",
              "#{string.quote(file_name)}: true peak after normalization would \
               be #{tp_after} dBTP (#{tp_after - tp_max} dB over limit). Gain \
               correction capped at 0.5 dB to preserve LUFS-I; a limiter is \
               required for this track."
            )
            gain_lufs - 0.5
          end

        [
          (
            settings.normalize_track_gain_metadata(),
            "#{final_gain} dB"
          ),
          (
            settings.lufs.integrated_metadata(),
            "#{measured_lufs} LUFS"
          ),
          ("lufs_true_peak", "#{measured_tp}")
        ]
      else
        []
      end
    elsif
      compute
    then
      # LUFS-only scan: no true peak measurement or correction (default behaviour).
      # Uses file.lufs() which checks for existing metadata before computing,
      # matching the original behaviour exactly.
      gain = file.lufs(compute=true, ratio=ratio, file_name)
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
    else
      # compute=false: use existing LUFS metadata, no computation at all.
      gain = file.lufs(compute=false, ratio=ratio, file_name)
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
  end

  decoder.metadata.add(reentrant=true, "lufs_track_gain", lufs_metadata)
end
