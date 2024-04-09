# Initialize settings for autocue
let settings.autocue = {internal=()}

let settings.autocue.implementations =
  settings.make(
    description=
      "All available autocue implementations",
    []
  )

let settings.autocue.target_cross_duration =
  settings.make(
    description=
      "How much data should be buffer from each track when computing crossfade \
       transitions. This will be shorter if one of the track is shorter than \
       that.",
    3.
  )

let settings.autocue.preferred =
  settings.make(
    description=
      "Preferred autocue",
    "internal"
  )

let settings.autocue.metadata_override =
  settings.make(
    description=
      "Disable processing when one of these metadata is found",
    [
      "liq_cue_in",
      "liq_cue_out",
      "liq_fade_in",
      "liq_fade_in_delay",
      "liq_fade_out",
      "liq_fade_out_delay"
    ]
  )

let settings.autocue.amplify_behavior =
  settings.make(
    description=
      "How to proceed with amplify data when `\"liq_amplify\"` is set. One of: \
       `\"keep\"` (prefer user-provided value), `\"override\"` (always override \
       with computed value), `\"ignore\"` (ignore/suppress loudness \
       adjustment).",
    "keep"
  )

let settings.autocue.internal.lufs_target =
  settings.make(
    description=
      "Loudness target",
    -16.0
  )

let settings.autocue.internal.cue_in_threshold =
  settings.make(
    description=
      "Cue in threshold",
    -34.0
  )

let settings.autocue.internal.cue_out_threshold =
  settings.make(
    description=
      "Cue out threshold",
    -42.0
  )

let settings.autocue.internal.cross_threshold =
  settings.make(
    description=
      "Crossfade start threshold",
    -7.0
  )

let settings.autocue.internal.max_overlap =
  settings.make(
    description=
      "Maximum allowed overlap/crossfade in seconds",
    6.0
  )

let settings.autocue.internal.sustained_endings_enabled =
  settings.make(
    description=
      "Try to optimize crossfade point on sustained endings",
    true
  )

let settings.autocue.internal.sustained_endings_dropoff =
  settings.make(
    description=
      "Max. loudness drop off immediately after crossfade point to consider it \
      as relevant ending [percentage]",
    15.0
  )

let settings.autocue.internal.sustained_endings_slope =
  settings.make(
    description=
      "Max. loudness difference between crossfade point and cue out to consider \
      it as relevant ending [percentage]",
    20.0
  )

let settings.autocue.internal.ratio =
  settings.make(
    description=
      "Maximum real time ratio to control speed of LUFS data analysis",
    50.
  )

let settings.autocue.internal.timeout =
  settings.make(
    description=
      "Maximum allowed processing time (estimated)",
    10.
  )

let autocue = {internal=()}

def autocue.register(~name, fn) =
  current_implementations = settings.autocue.implementations()
  if
    list.assoc.mem(name, current_implementations)
  then
    error.raise(
      error.invalid,
      "Autocue implementation #{name} already exists!"
    )
  end
  settings.autocue.implementations := [(name, fn), ...current_implementations]
end

# Get frames from ffmpeg.filter.ebur128
# @flag hidden
def autocue.internal.ebur128(~ratio=50., ~timeout=10., uri) =
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
        label="autocue.internal",
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
      label="autocue.internal",
      "ffmpeg.filter.ebur128 is not available, autocue disabled!"
    )
    []
%endif
  else
    log(
      level=2,
      label="autocue.internal",
      "Couldn't resolve source for uri: #{uri}"
    )
    []
  end
end

# Compute autocue data
# @flag hidden
def autocue.internal.implementation(uri) =
  lufs_target = settings.autocue.internal.lufs_target()
  cue_in_threshold = settings.autocue.internal.cue_in_threshold()
  cue_out_threshold = settings.autocue.internal.cue_out_threshold()
  cross_threshold = settings.autocue.internal.cross_threshold()
  max_overlap = settings.autocue.internal.max_overlap()
  sustained_endings_enabled = settings.autocue.internal.sustained_endings_enabled()
  sustained_endings_dropoff = settings.autocue.internal.sustained_endings_dropoff()
  sustained_endings_slope = settings.autocue.internal.sustained_endings_slope()
  ratio = settings.autocue.internal.ratio()
  timeout = settings.autocue.internal.timeout()

  log(
    level=4,
    label="autocue.internal",
    "Starting to process #{path.basename(uri)}"
  )

  frames = autocue.internal.ebur128(ratio=ratio, timeout=timeout, uri)

  if
    list.length(frames) < 2
  then
    log(
      level=2,
      label="autocue.internal",
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
      label="autocue.internal",
      "Processing results for #{path.basename(uri)}"
    )

    log(
      level=4,
      label="autocue.internal",
      "lufs_correction: #{lufs_correction}"
    )
    log(
      level=4,
      label="autocue.internal",
      "lufs_cue_in_threshold: #{lufs_cue_in_threshold}"
    )
    log(
      level=4,
      label="autocue.internal",
      "lufs_cue_out_threshold: #{lufs_cue_out_threshold}"
    )
    log(
      level=4,
      label="autocue.internal",
      "lufs_cross_threshold: #{lufs_cross_threshold}"
    )

    # Set cue/fade defaults
    cue_in = ref(0.)
    cue_out = ref(0.)
    cross_cue = ref(0.)
    fade_in = ref(0.)
    fade_out = ref(0.)

    # Extract timestamps for cue points
    # Iterate over loudness data frames and set cue points based on db thresholds
    last_ts = ref(0.)
    current_ts = ref(0.)
    cue_found = ref(false)
    cross_start_idx = ref(0.)
    cross_stop_idx = ref(0.)
    cross_mid_idx = ref(0.)
    cross_frame_length = ref(0.)
    ending_fst_db = ref(0.)
    ending_snd_db = ref(0.)
    reset_iter_values = ref(true)

    frames_rev = list.rev(frames)
    total_frames_length = float_of_int(list.length(frames))
    frame_idx = ref(total_frames_length - 1.)
    lufs_cross_threshold_sustained = ref(lufs_cross_threshold)
    lufs_cue_out_threshold_sustained = ref(lufs_cue_out_threshold)

    err = error.register("assoc")
    def find_cues(
      frame,
      ~reverse_order=false,
      ~sustained_ending_check=false,
      ~sustained_ending_recalc=false
    ) =
      if
        reset_iter_values()
      then
        last_ts := 0.
        current_ts := 0.
        cue_found := false
      end

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
          not sustained_ending_check and
          not sustained_ending_recalc
        then
          # Run regular cue point calc
          reset_iter_values := false
          if
            not reverse_order
          then
            # Search for cue in
            if
              db_level > lufs_cue_in_threshold
            then
              # First time exceeding threshold
              cue_in := last_ts()
              # Break
              error.raise(err,"break list.iter")
            end
          else
            # Search for cue out and crossfade point starting from the end (reversed)
            if
              db_level > lufs_cue_out_threshold and
              not cue_found()
            then
              # Cue out
              cue_out := last_ts()
              cross_stop_idx := frame_idx()
              cue_found := true
            elsif
              db_level > lufs_cross_threshold
            then
              # Absolute crossfade cue
              cross_cue := last_ts()
              cross_start_idx := frame_idx()
              # Break
              error.raise(err,"break list.iter")
            end
            frame_idx := frame_idx() - 1.
          end
        elsif
          sustained_ending_check
        then
          # Check regular crossfade data for sustained ending
          if
            reset_iter_values()
          then
            frame_idx := total_frames_length - 1.
            cross_start_idx := cross_start_idx() + 5.
            cross_stop_idx := cross_stop_idx() - 5.
            cross_frame_length := cross_stop_idx() - cross_start_idx()
            cross_mid_idx := cross_stop_idx() - (cross_frame_length() / 2.)
          end
          reset_iter_values := false

          if
            frame_idx() < cross_start_idx() or
            cross_frame_length() < 5.
          then
            error.raise(err,"break list.iter")
          end

          if
            frame_idx() < cross_stop_idx() and
            frame_idx() > cross_mid_idx()
          then
            if
              ending_snd_db() < 0.
            then
              ending_snd_db := (ending_snd_db() + db_level) / 2.
            else
              ending_snd_db := db_level
            end
          end

          if
            frame_idx() > cross_start_idx() and
            frame_idx() < cross_mid_idx()
          then
            if
              ending_fst_db() < 0.
            then
              ending_fst_db := (ending_fst_db() + db_level) / 2.
            else
              ending_fst_db := db_level
            end
          end
          frame_idx := frame_idx() - 1.
        elsif
          sustained_ending_recalc
        then
          # Recalculate crossfade on sustained ending
          if
            reset_iter_values()
          then
            cue_out := 0.
            cross_cue := 0.
          end
          reset_iter_values := false
          if
            db_level > lufs_cue_out_threshold_sustained() and
            not cue_found()
          then
            # Cue out
            cue_out := last_ts()
            cue_found := true
          end
          if
            db_level > lufs_cross_threshold_sustained()
          then
            # Absolute crossfade cue
            cross_cue := last_ts()
            error.raise(err,"break list.iter")
          end
        end

        # Update last timestamp value with current
        last_ts := current_ts()
      end
    end

    # Search for cue_in first
    reset_iter_values := true
    def cue_iter_fwd(frame) =
      find_cues(frame)
    end
    try
      list.iter(cue_iter_fwd, frames)
    catch _ do
      log(
        level=4,
        label="autocue.internal",
        "cue_iter_fwd completed."
      )
    end

    # Reverse frames and search in reverse order for cross_cue and cue_out
    reset_iter_values := true
    def cue_iter_rev(frame) =
      find_cues(frame,reverse_order=true)
    end
    try
      list.iter(cue_iter_rev, frames_rev)
    catch _ do
      log(
        level=4,
        label="autocue.internal",
        "cue_iter_rev completed."
      )
    end

    if
      sustained_endings_enabled
    then
      # Check for sustained ending
      reset_iter_values := true
      def sustained_ending_check_iter(frame) =
        find_cues(frame,sustained_ending_check=true)
      end
      try
        list.iter(sustained_ending_check_iter, frames_rev)
      catch _ do
        log(
          level=4,
          label="autocue.internal.sustained_ending",
          "sustained_ending_check_iter completed."
        )
      end

      log(
        level=4,
        label="autocue.internal.sustained_ending",
        "Analysis frame length: #{cross_frame_length()}"
      )
      log(
        level=4,
        label="autocue.internal.sustained_ending",
        "Avg. ending dB level: 1st half => #{ending_fst_db()} / 2nd half => #{ending_snd_db()}"
      )

      # Check whether data indicate a sustained ending
      if
        ending_fst_db() < 0.
      then
        slope = ref(0.)

        if
          ending_snd_db() < 0.
        then
          slope := ending_fst_db() / ending_snd_db()
        end

        log(
          level=4,
          label="autocue.internal.sustained_ending",
          "Slope: #{slope()} (#{
            (1. - slope()) * 100.
          }%)"
        )

        detect_slope = slope() > 1. - sustained_endings_slope / 100.
        detect_dropoff = ending_fst_db() > lufs_cross_threshold * sustained_endings_dropoff / 100. + 1.
        if
          detect_slope or
          detect_dropoff
        then
          log(
            level=3,
            label="autocue.internal.sustained_ending",
            "Sustained ending detected (drop off: #{detect_dropoff} / slope: #{detect_slope})"
          )

          if
            detect_slope
          then
            lufs_cross_threshold_sustained := ending_snd_db() - 0.5
          else
            lufs_cross_threshold_sustained := ending_fst_db() - 0.5
          end
          lufs_cue_out_threshold_sustained = ref(
            lufs_cue_out_threshold + (lufs_cross_threshold_sustained() - lufs_cross_threshold)
          )

          log(
            level=4,
            label="autocue.internal.sustained_ending",
            "Changed crossfade threshold: #{lufs_cross_threshold} => #{lufs_cross_threshold_sustained()}"
          )
          log(
            level=4,
            label="autocue.internal.sustained_ending",
            "Changed cue out threshold: #{lufs_cue_out_threshold} => #{lufs_cue_out_threshold_sustained()}"
          )

          cross_cue_init = cross_cue()
          cue_out_init = cue_out()

          reset_iter_values := true
          def sustained_ending_recalc_iter(frame) =
            find_cues(frame,sustained_ending_recalc=true)
          end
          try
            list.iter(sustained_ending_recalc_iter, frames_rev)
          catch _ do
            log(
              level=4,
              label="autocue.internal",
              "sustained_ending_recalc_iter completed."
            )
          end

          log(
            level=4,
            label="autocue.internal.sustained_ending",
            "Changed crossfade point: #{cross_cue_init} => #{cross_cue()}"
          )
          log(
            level=4,
            label="autocue.internal.sustained_ending",
            "Changed cue out point:  #{cue_out_init} => #{cue_out()}"
          )
        else
          log(
            level=3,
            label="autocue.internal.sustained_ending",
            "No sustained ending detected."
          )
        end
      else
        log(
          level=3,
          label="autocue.internal.sustained_ending",
          "No sustained ending detected."
        )
      end
    end

    # Get very last frame for precise track duration
    frame = list.last(frames)
    duration =
      float_of_string(list.assoc(default="0.", "lavfi.liq.pts", frame)) +
        float_of_string(list.assoc(default="0.", "lavfi.liq.duration", frame))

    # Finalize cue/cross/fade values now...
    if cue_out() == 0. then cue_out := duration end

    # Calc cross/overlap duration
    if
      cross_cue() + 0.1 < cue_out()
    then
      fade_out := cue_out() - cross_cue()
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

    # Limit overlap duration to maximum
    if max_overlap < fade_in() then fade_in := max_overlap end

    if
      max_overlap < fade_out()
    then
      cue_shift = fade_out() - max_overlap
      cue_out := cue_out() - cue_shift
      fade_out := max_overlap
      fade_out := max_overlap
    end

    (
      {
        amplify=lin_of_dB(lufs_correction),
        cue_in=cue_in(),
        cue_out=cue_out(),
        fade_in=fade_in(),
        fade_out=fade_out(),
        extra_metadata=[("liq_autocue", "internal")]
      }
    :
      {
        amplify?: float,
        cue_in: float,
        cue_out: float,
        fade_in: float,
        fade_out: float,
        extra_metadata?: [(string*string)]
      }
    )
  end
end

autocue.register(name="internal", autocue.internal.implementation)

let file.autocue = ()

# Return the file's autocue values as metadata suitable for metadata override.
# @category Source / Audio processing
def file.autocue.metadata(uri) =
  preferred_implementation = settings.autocue.preferred()
  implementations = settings.autocue.implementations()
  implementation =
    if
      list.assoc.mem(preferred_implementation, implementations)
    then
      log(
        level=4,
        label="autocue",
        "Using preferred #{preferred_implementation} autocue implementation."
      )
      list.assoc(preferred_implementation, implementations)
    elsif
      list.length(implementations) > 0
    then
      let [(name, implementation)] = implementations
      log(
        level=4,
        label="autocue",
        "Using first available #{name} autocue implementation."
      )
      implementation
    else
      error.raise(
        error.not_found,
        "No autocue implementation found!"
      )
    end

  autocue = implementation(uri)

  if
    null.defined(autocue)
  then
    autocue = null.get(autocue)
    let {cue_in, cue_out, fade_in, fade_out} = autocue
    extra_metadata = autocue.extra_metadata ?? []
    amplify = autocue?.amplify
    target_cross_duration = settings.autocue.target_cross_duration()

    cross_duration =
      if
        cue_out - cue_in < target_cross_duration * 2.
      or
        target_cross_duration < fade_in
      or
        target_cross_duration < fade_out
      then
        max(fade_in, fade_out)
      else
        target_cross_duration
      end

    # Make sure cross_duration is never 0.
    cross_duration = max(cross_duration, 0.1)

    fade_out_delay =
      if fade_out < cross_duration then cross_duration - fade_out else 0. end

    [
      ...(null.defined(amplify) ? [("liq_amplify", string(amplify))] : [] ),
      ("liq_cue_in", string(cue_in)),
      ("liq_cue_out", string(cue_out)),
      ("liq_cross_duration", string(cross_duration)),
      ("liq_fade_in", string(fade_in)),
      ("liq_fade_in_delay", "0."),
      ("liq_fade_out", string(fade_out)),
      ("liq_fade_out_delay", string(fade_out_delay)),
      ...extra_metadata
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
# values can be computed. This function sets `settings.request.prefetch` to `2`
# to account for the latency introduced by the `autocue` computation when resolving
# reausts and sets `settings.crossfade.assume_autocue` to `true` as well. For a
# finer-grained processing, use the `autocue:` protocol.
# @category Liquidsoap
def enable_autocue_metadata() =
  if settings.request.prefetch() == 1 then settings.request.prefetch := 2 end
  settings.crossfade.assume_autocue := true

  def autocue_metadata(~metadata, fname) =
    metadata_overrides = settings.autocue.metadata_override()

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
      autocue_metadata = file.autocue.metadata(fname)

      autocue_metadata =
        if
          settings.autocue.amplify_behavior() == "ignore"
        then
          [...list.assoc.remove("liq_amplify", autocue_metadata)]
        else
          if
            list.assoc.mem("liq_amplify", metadata)
          then
            if
              settings.autocue.amplify_behavior() == "keep"
            then
              log(
                level=3,
                label="autocue.metadata",
                "Keeping user-provided `\"liq_amplify\"` value."
              )
              [
                ...list.assoc.remove("liq_amplify", autocue_metadata),
                ("liq_amplify", metadata["liq_amplify"])
              ]
            elsif
              settings.autocue.amplify_behavior() == "override"
            then
              log(
                level=3,
                label="autocue.metadata",
                "Overriding user-provided `\"liq_amplify\"` value."
              )
              autocue_metadata
            else
              log(
                level=2,
                label="autocue.metadata",
                "Invalid value for `settings.autocue.amplify_behavior`: #{
                  settings.autocue.amplify_behavior()
                }"
              )
              autocue_metadata
            end
          else
            autocue_metadata
          end
        end
      log(level=4, label="autocue.metadata", "#{autocue_metadata}")
      autocue_metadata
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
      "No autocue data found for URI #{arg}!"
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
