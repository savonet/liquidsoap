# Initialize settings for autocue protocol
let settings.autocue = ()

let settings.autocue.lufs_target =
  settings.make(
    description=
      "Loudness target",
    -16.0
  )

let settings.autocue.cue_in_threshold =
  settings.make(
    description=
      "Cue in threshold",
    -34.0
  )

let settings.autocue.cue_out_threshold =
  settings.make(
    description=
      "Cue out threshold",
    -42.0
  )

let settings.autocue.cross_threshold =
  settings.make(
    description=
      "Crossfade start threshold",
    -7.0
  )

let settings.autocue.max_overlap =
  settings.make(
    description=
      "Maximum allowed overlap/crossfade in seconds",
    6.0
  )

let settings.autocue.ratio =
  settings.make(
    description=
      "Maximum real time ratio to control speed of LUFS data analysis",
    50.
  )

let settings.autocue.timeout =
  settings.make(
    description=
      "Maximum allowed processing time (estimated)",
    10.
  )

let settings.autocue.metadata_overrides =
  settings.make(
    description=
      "Existing metadata causing autocue to skip",
    [
      "liq_amplify",
      "liq_cue_in",
      "liq_cue_out",
      "liq_cross_duration",
      "liq_fade_in",
      "liq_fade_out"
    ]
  )

let file.autocue = ()

# Get frames from ffmpeg.filter.ebur128
# @flag hidden
def file.autocue.ebur128(~ratio=50., ~timeout=10., uri) =
  r = request.create(resolve_metadata=false, uri)
  s = request.once(r)

  if
    s.resolve()
  then
%ifdef ffmpeg.filter.ebur128
    duration = null.get(default=0., request.duration(uri))
    estimated_processing_time = duration / ratio

    if
      estimated_processing_time > timeout or duration <= 0.
    then
      log(
        level=2,
        label="autocue",
        "Estimated processing duration is too long, autocue disabled! #{
          duration
        } / #{ratio} = #{estimated_processing_time} (Duration / Ratio = \
         Processing duration; max. allowed: #{timeout})"
      )
      []
    else
      frames = ref([])

      def ebur128(s) =
        def mk_filter(graph) =
          let {audio = a} = source.tracks(s)
          a = ffmpeg.filter.audio.input(graph, a)
          let ([a], _) = ffmpeg.filter.ebur128(metadata=true, graph, a)
          a = ffmpeg.filter.audio.output(graph, a)
          source({audio=a, metadata=track.metadata(a)})
        end

        ffmpeg.filter.create(mk_filter)
      end

      s = ebur128(s)
      s = source.on_metadata(s, fun (m) -> frames := [...frames(), m])
      source.drop(ratio=ratio, s)

      frames()
    end
%else
    log(
      level=2,
      label="autofocus",
      "ffmpeg.filter.ebur128 is not available, autocue disabled!"
    )
    []
%endif
  else
    log(
      level=2,
      label="autocue",
      "Couldn't resolve source for uri: #{uri}"
    )
    []
  end
end

# Compute autocue data
# @flag extra
# @category Source / Audio processing
# @param ~lufs_target Loudness target in LUFS
# @param ~cue_in_threshold Cue in threshold
# @param ~cue_out_threshold Cue out threshold
# @param ~cross_threshold Crossfade start threshold
# @param ~max_overlap Maximum allowed overlap/crossfade in seconds
# @param ~ratio Decoding ratio. A value of `50.` means try to decode the file `50x` faster than real time, if possible. Use this setting to lower CPU peaks when computing autocue data.
def replaces file.autocue(
  ~lufs_target=null(),
  ~cue_in_threshold=null(),
  ~cue_out_threshold=null(),
  ~cross_threshold=null(),
  ~max_overlap=null(),
  ~ratio=null(),
  ~timeout=null(),
  uri
) =
  lufs_target = lufs_target ?? settings.autocue.lufs_target()
  cue_in_threshold = cue_in_threshold ?? settings.autocue.cue_in_threshold()
  cue_out_threshold = cue_out_threshold ?? settings.autocue.cue_out_threshold()
  cross_threshold = cross_threshold ?? settings.autocue.cross_threshold()
  max_overlap = max_overlap ?? settings.autocue.max_overlap()
  ratio = ratio ?? settings.autocue.ratio()
  timeout = timeout ?? settings.autocue.timeout()

  frames = file.autocue.ebur128(ratio=ratio, timeout=timeout, uri)

  if
    frames == []
  then
    log(
      level=2,
      label="autocue",
      "Autocue computation failed!"
    )
    null()
  else
    # Get the 2nd last frame which is the last with loudness data
    frame = list.nth(frames, list.length(frames) - 2)

    # Get the Integrated Loudness from the last frame (overall loudness)
    lufs =
      float_of_string(
        list.assoc(default=string(lufs_target), "lavfi.r128.I", frame)
      )

    # Calc LUFS difference to target for liq_amplify
    lufs_correction = lufs_target - lufs

    # Create dB thresholds relative to LUFS target
    lufs_cue_in_threshold = lufs + cue_in_threshold
    lufs_cue_out_threshold = lufs + cue_out_threshold
    lufs_cross_threshold = lufs + cross_threshold

    log(
      level=4,
      label="autocue",
      "lufs_correction: #{lufs_correction}"
    )
    log(
      level=4,
      label="autocue",
      "adj_cue_in_threshold: #{lufs_cue_in_threshold}"
    )
    log(
      level=4,
      label="autocue",
      "adj_cue_out_threshold: #{lufs_cue_out_threshold}"
    )
    log(
      level=4,
      label="autocue",
      "adj_cross_threshold: #{lufs_cross_threshold}"
    )

    # Set cue/fade defaults
    cue_in = ref(0.)
    cue_out = ref(0.)
    cross_cue = ref(0.)
    fade_in = ref(0.)
    fade_out = ref(0.)
    cross_duration = ref(0.)

    # Extract timestamps for cue points
    # Iterate over loudness data frames and set cue points based on db thresholds
    cue_in_found = ref(false)
    last_ts = ref(0.)
    current_ts = ref(0.)

    def find_cues(frame) =
      # Get current frame loudness level and timestamp
      db_level = list.assoc(default="nan", string("lavfi.r128.M"), frame)
      current_ts :=
        float_of_string(list.assoc(default="0.", "lavfi.liq.pts", frame))

      # Process only valid level values
      if
        db_level != "nan"
      then
        db_level = float_of_string(db_level)

        if
          db_level > lufs_cue_in_threshold and cue_in_found() == false
        then
          # First time exceeding threshold. Set cue in to timestamp of previous frame.
          cue_in := last_ts()
          cue_in_found := true
        end
        if
          db_level > lufs_cue_out_threshold
        then
          # Cue out: Stick with the latest timestamp where level still exceeds threshold
          cue_out := current_ts()
        end
        if
          db_level > lufs_cross_threshold
        then
          # Absolute crossfade cue: Stick with the latest timestamp where level still exceeds threshold
          cross_cue := current_ts()
        end
      end

      # Update last timestamp value with current
      last_ts := current_ts()
    end
    list.iter(find_cues, frames)

    # Get very last frame for precise track duration
    frame = list.last(frames)
    duration =
      float_of_string(list.assoc(default="0.", "lavfi.liq.pts", frame)) +
        float_of_string(list.assoc(default="0.", "lavfi.liq.duration", frame))

    # Finalize cue/cross/fade values now...

    # Calc cross/overlap duration
    if
      cross_cue() + 0.1 < duration
    then
      if
        cue_out() > 0.
      then
        cross_duration := cue_out() - cross_cue()
      else
        cross_duration := duration - cross_cue()
      end
    end

    # Add some margin to cue in
    cue_in := cue_in() - 0.1

    # Avoid hard cuts on cue in
    if
      cue_in() > 0.2
    then
      fade_in := 0.2
      cue_in := cue_in() - 0.2
    end

    # Ignore super short cue in
    if
      cue_in() <= 0.2
    then
      fade_in := 0.
      cue_in := 0.
    end

    # Catch invalid cue out values
    if cue_out() <= cue_in() then cue_out := 0. end

    # Set fade out equal to crossfade/overlap
    if cross_duration() > 0. then fade_out := cross_duration() end

    # Limit overlap duration to maximum
    if
      cross_duration() > max_overlap
    then
      cue_shift = cross_duration() - max_overlap
      cue_out := cue_out() - cue_shift
      cross_duration := max_overlap
      fade_out := max_overlap
    end

    {
      amplify=lufs_correction,
      cue_in=(cue_in() > 0. ? cue_in() : null() ),
      cue_out=(cue_out() > 0. ? cue_out() : null() ),
      cross_duration=cross_duration(),
      fade_in=fade_in(),
      fade_out=fade_out()
    }
  end
end

# Return the file's autocue values as metadata suitable for metadata override.
# @flag extra
# @category Source / Audio processing
# @param ~lufs_target Loudness target in LUFS
# @param ~cue_in_threshold Cue in threshold
# @param ~cue_out_threshold Cue out threshold
# @param ~cross_threshold Crossfade start threshold
# @param ~max_overlap Maximum allowed overlap/crossfade in seconds
# @param ~ratio Decoding ratio. A value of `50.` means try to decode the file `50x` faster than real time, if possible. Use this setting to lower CPU peaks when computing autocue data.
def file.autocue.metadata(
  ~lufs_target=null(),
  ~cue_in_threshold=null(),
  ~cue_out_threshold=null(),
  ~cross_threshold=null(),
  ~max_overlap=null(),
  ~ratio=null(),
  uri
) =
  let autocue =
    file.autocue(
      lufs_target=lufs_target,
      cue_in_threshold=cue_in_threshold,
      cue_out_threshold=cue_out_threshold,
      cross_threshold=cross_threshold,
      max_overlap=max_overlap,
      ratio=ratio,
      uri
    )

  if
    null.defined(autocue)
  then
    let {
      amplify,
      cue_in,
      cue_out,
      cross_duration,
      fade_in,
      fade_out
    } = null.get(autocue)

    [
      ("liq_autocue", "true"),
      (
        "liq_amplify",
        "#{amplify} dB"
      ),
      ...(
        null.defined(cue_in) ? [("liq_cue_in", string(null.get(cue_in)))] : []
      ),
      ...(
        null.defined(cue_out)
        ? [("liq_cue_out", string(null.get(cue_out)))] : []
      ),

      ("liq_cross_duration", string(cross_duration)),
      ("liq_fade_in", string(fade_in)),
      ("liq_fade_out", string(fade_out))
    ]
  else
    log(
      level=2,
      label="autocue.metadata",
      "No autocue data found for file #{uri}"
    )
    []
  end
end

# Enable autocue metadata resolver. This resolver will process any file
# decoded by Liquidsoap and add cue-in/out and crossfade metadata when these
# values can be computed. For a finer-grained processing, use the `autocue:` protocol.
# @category Liquidsoap
def enable_autocue_metadata() =
  def autocue_metadata(~metadata, fname) =
    metadata_overrides = settings.autocue.metadata_overrides()

    if
      list.exists(fun (el) -> list.mem(fst(el), metadata_overrides), metadata)
    then
      log(
        level=2,
        label="autocue.metadata",
        "Override metadata detected for file #{fname}, disabling autocue!"
      )
      []
    else
      file.autocue.metadata(fname)
    end
  end
  decoder.metadata.add("autocue", autocue_metadata)
end

# Define autocue protocol
# @flag hidden
def protocol.autocue(~rlog=_, ~maxtime=_, arg) =
  cue_metadata = file.autocue.metadata(arg)

  if
    cue_metadata != []
  then
    cue_metadata =
      list.map(fun (el) -> "#{fst(el)}=#{string.quote(snd(el))}", cue_metadata)
    cue_metadata = string.concat(separator=",", cue_metadata)
    ["annotate:#{cue_metadata}:#{arg}"]
  else
    log(
      level=2,
      label="autocue.protocol",
      "No autofocus data found for URI #{arg}!"
    )
    [arg]
  end
end
protocol.add(
  "autocue",
  protocol.autocue,
  doc=
    "Adding automatically computed cues/crossfade metadata",
  syntax="autocue:uri"
)
