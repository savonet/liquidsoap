# Initialize settings for autocue
let settings.autocue = {internal=()}

let settings.autocue.implementations =
  settings.make(
    description=
      "All available autocue implementations",
    []
  )

let settings.autocue.metadata = ()

let settings.autocue.metadata.priority =
  settings.make(
    description=
      "Priority for the autocue metadata resolver. Default value allows it to \
       override both file and request metadata.",
    10
  )

let settings.autocue.preferred =
  settings.make(
    description=
      "Preferred autocue",
    "internal"
  )

let settings.autocue.amplify_behavior =
  settings.make(
    description=
      "How to proceed with loudness adjustment. Set to `\"override\"` to always prefer
      the value provided by the `autocue` provider. Set to `\"ignore\"` to ignore all
      loudness correction provided via the `autocue` provider. Set to
      `\"keep\"` to always prefer user-provided values (via request annotation or file tags)
      over values provided by the `autocue` provider.",
    "override"
  )

let settings.autocue.amplify_aliases =
  settings.make(
    description=
      "List of metadata to treat as amplify aliases when applying the \
       `amplify_behavior` policy.",
    ["replaygain_track_gain"]
  )

let settings.autocue.internal.metadata_override =
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

let settings.autocue.internal.queues_count =
  settings.make(
    description=
      "Number of dedicated queues for resolving autocue data using the internal \
       implementation",
    1
  )

def settings.autocue.internal.queues_count.set(c) =
  settings.scheduler.queues :=
    [
      ...list.assoc.remove("autocue", settings.scheduler.queues()),
      ("autocue", c)
    ]
  settings.autocue.internal.queues_count.set(c)
end

settings.autocue.internal.queues_count.set(1)

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

let settings.autocue.internal.sustained_endings_min_duration =
  settings.make(
    description=
      "Minimum duration to consider it the ending as sustained [seconds]",
    1.0
  )

let settings.autocue.internal.sustained_endings_threshold_limit =
  settings.make(
    description=
      "Max reduction of dB thresholds compared to initial value [multiplying \
       factor]",
    2.0
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

# Register an `autocue` implementation.
# @category Source / Audio processing
# @param ~name Name of the implementation
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
def autocue.internal.ebur128(~duration, ~ratio=50., ~timeout=10., filename) =
  ignore(ratio)
  ignore(timeout)
  ignore(filename)
  ignore(duration)
%ifdef ffmpeg.filter.ebur128
  estimated_processing_time = duration / ratio

  if
    estimated_processing_time > timeout or duration <= 0.
  then
    log(
      level=2,
      label="autocue.internal",
      "Estimated processing duration is too long, autocue disabled! #{
        duration
      } / #{ratio} = #{estimated_processing_time} (Duration / Ratio = Processing \
       duration; max. allowed: #{timeout})"
    )
    []
  else
    s =
      request.once(
        thread_queue="autocue", request.create(resolve_metadata=false, filename)
      )
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

    list.sort(
      fun (m, m') ->
        begin
          pos =
            float_of_string(list.assoc(default="0.", "lavfi.liq.pts_time", m))
          pos' =
            float_of_string(list.assoc(default="0.", "lavfi.liq.pts_time", m'))
          if pos < pos' then -1 elsif pos < pos' then 1 else 0 end
        end,
      frames()
    )
  end
%else
  ignore(ratio)
  ignore(timeout)
  ignore(filename)
  log(
    level=2,
    label="autocue.internal",
    "ffmpeg.filter.ebur128 is not available, autocue disabled!"
  )
  []
%endif
end

# Compute autocue data
# @flag hidden
def autocue.internal.implementation(
  ~request_metadata,
  ~file_metadata,
  filename
) =
  lufs_target = settings.autocue.internal.lufs_target()
  cue_in_threshold = settings.autocue.internal.cue_in_threshold()
  cue_out_threshold = settings.autocue.internal.cue_out_threshold()
  cross_threshold = settings.autocue.internal.cross_threshold()
  max_overlap = settings.autocue.internal.max_overlap()
  sustained_endings_enabled =
    settings.autocue.internal.sustained_endings_enabled()
  sustained_endings_dropoff =
    settings.autocue.internal.sustained_endings_dropoff()
  sustained_endings_slope = settings.autocue.internal.sustained_endings_slope()
  sustained_endings_min_duration =
    settings.autocue.internal.sustained_endings_min_duration()
  sustained_endings_threshold_limit =
    settings.autocue.internal.sustained_endings_threshold_limit()
  ratio = settings.autocue.internal.ratio()
  timeout = settings.autocue.internal.timeout()

  metadata_overrides = settings.autocue.internal.metadata_override()

  metadata = [...request_metadata, ...file_metadata]

  if
    list.exists(fun (el) -> list.mem(fst(el), metadata_overrides), metadata)
  then
    log(
      level=2,
      label="autocue.internal.metadata",
      "Override metadata detected for #{filename}, disabling autocue!"
    )
    null()
  else
    log(
      level=4,
      label="autocue.internal",
      "Starting to process #{filename}"
    )

%ifdef request.duration.ffmpeg
    duration = request.duration.ffmpeg(resolve_metadata=false, filename)
%else
    duration = null()
%endif

    if
      duration == null()
    then
      log(
        level=2,
        label="autocue.internal",
        "Could not get request duration, internal autocue disabled!"
      )
      null()
    else
      duration = null.get(duration)

      frames =
        autocue.internal.ebur128(
          duration=duration, ratio=ratio, timeout=timeout, filename
        )

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
          "Processing results for #{filename}"
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
            float_of_string(
              list.assoc(default="0.", "lavfi.liq.pts_time", frame)
            )

          # Process only valid level values
          if
            db_level != "nan"
          then
            db_level = float_of_string(db_level)

            if
              not sustained_ending_check and not sustained_ending_recalc
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
                  error.raise(
                    err,
                    "break list.iter"
                  )
                end
              else
                # Search for cue out and crossfade point starting from the end (reversed)
                if
                  db_level > lufs_cue_out_threshold and not cue_found()
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
                  error.raise(
                    err,
                    "break list.iter"
                  )
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
                frame_idx() < cross_start_idx()
              or
                cross_frame_length() < sustained_endings_min_duration * 10.
              then
                error.raise(
                  err,
                  "break list.iter"
                )
              end

              if
                frame_idx() < cross_stop_idx() and frame_idx() > cross_mid_idx()
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
                frame_idx() > cross_start_idx()
              and
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
                db_level > lufs_cue_out_threshold_sustained()
              and
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
                cross_cue := current_ts()
                error.raise(
                  err,
                  "break list.iter"
                )
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
          find_cues(frame, reverse_order=true)
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
            find_cues(frame, sustained_ending_check=true)
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
            "Avg. ending loudness: #{ending_fst_db()} => #{ending_snd_db()}"
          )

          # Check whether data indicate a sustained ending
          if
            ending_fst_db() < 0.
          then
            slope = ref(0.)
            dropoff = lufs_cross_threshold / ending_fst_db()

            if
              ending_snd_db() < 0.
            then
              slope := ending_fst_db() / ending_snd_db()
            end

            log(
              level=4,
              label="autocue.internal.sustained_ending",
              "Drop off: #{(1. - dropoff) * 100.}%"
            )
            log(
              level=4,
              label="autocue.internal.sustained_ending",
              "Slope: #{(1. - slope()) * 100.}%"
            )

            detect_slope = slope() > 1. - sustained_endings_slope / 100.
            detect_dropoff =
              ending_fst_db() >
                lufs_cross_threshold * (sustained_endings_dropoff / 100. + 1.)
            if
              detect_slope or detect_dropoff
            then
              log(
                level=3,
                label="autocue.internal.sustained_ending",
                "Sustained ending detected (drop off: #{detect_dropoff} / slope: \
                 #{detect_slope})"
              )

              if
                detect_slope
              then
                lufs_cross_threshold_sustained :=
                  max(
                    lufs_cross_threshold * sustained_endings_threshold_limit,
                    ending_snd_db() - 0.5
                  )
              else
                lufs_cross_threshold_sustained :=
                  max(
                    lufs_cross_threshold * sustained_endings_threshold_limit,
                    ending_fst_db() - 0.5
                  )
              end
              lufs_cue_out_threshold_sustained =
                ref(
                  max(
                    lufs_cue_out_threshold * sustained_endings_threshold_limit,
                    lufs_cue_out_threshold +
                      (lufs_cross_threshold_sustained() - lufs_cross_threshold)
                  )
                )

              log(
                level=4,
                label="autocue.internal.sustained_ending",
                "Changed crossfade threshold: #{lufs_cross_threshold} => #{
                  lufs_cross_threshold_sustained()
                }"
              )
              log(
                level=4,
                label="autocue.internal.sustained_ending",
                "Changed cue out threshold: #{lufs_cue_out_threshold} => #{
                  lufs_cue_out_threshold_sustained()
                }"
              )

              cross_cue_init = cross_cue()
              cue_out_init = cue_out()

              reset_iter_values := true
              def sustained_ending_recalc_iter(frame) =
                find_cues(frame, sustained_ending_recalc=true)
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
            amplify=
              "#{lufs_correction} dB",
            cue_in=cue_in(),
            cue_out=cue_out(),
            fade_in=fade_in(),
            fade_out=fade_out()
          }
        :
          {
            amplify?: string,
            cue_in: float,
            cue_out: float,
            fade_in: float,
            fade_in_type?: string,
            fade_in_curve?: float,
            fade_out: float,
            fade_out_type?: string,
            fade_out_curve?: float,
            start_next?: float,
            extra_metadata?: [(string*string)]
          }
        )
      end
    end
  end
end

autocue.register(name="internal", autocue.internal.implementation)

let file.autocue = ()

# Return the file's autocue values as metadata suitable for metadata override.
# @category Source / Audio processing
def file.autocue.metadata(~metadata, uri) =
  preferred_implementation = settings.autocue.preferred()
  implementations = settings.autocue.implementations()
  let (implementation_name, implementation) =
    if
      list.assoc.mem(preferred_implementation, implementations)
    then
      log(
        level=4,
        label="autocue",
        "Using preferred #{preferred_implementation} autocue implementation."
      )
      (
        preferred_implementation,
        list.assoc(preferred_implementation, implementations)
      )
    elsif
      list.length(implementations) > 0
    then
      let [(name, implementation)] = implementations
      log(
        level=4,
        label="autocue",
        "Using first available #{name} autocue implementation."
      )
      (name, implementation)
    else
      error.raise(
        error.not_found,
        "No autocue implementation found!"
      )
    end

  r = request.create(excluded_metadata_resolvers=["autocue"], uri)

  if
    not request.resolve(r)
  then
    request.destroy(r)
    log(
      level=2,
      label="autocue",
      "Couldn't resolve uri: #{uri}"
    )
    []
  else
    autocue =
      try
        autocue =
          implementation(
            request_metadata=metadata,
            file_metadata=request.metadata(r),
            request.filename(r)
          )
        request.destroy(r)
        autocue
      catch err do
        request.destroy(r)
        log(
          level=2,
          label="autocue",
          "Error while processing autocue: #{err}"
        )
        error.raise(err)
      end

    if
      null.defined(autocue)
    then
      autocue = null.get(autocue)
      let {cue_in, cue_out, fade_in, fade_out} = autocue

      extra_metadata = autocue.extra_metadata ?? []
      amplify = autocue?.amplify

      fade_in_type = autocue?.fade_in_type
      fade_in_curve = autocue?.fade_in_curve
      fade_out_type = autocue?.fade_out_type
      fade_out_curve = autocue?.fade_out_curve

      fade_out_start = cue_out - fade_out
      let (fade_out, fade_out_start) =
        if
          fade_out_start < 0.
        then
          log(
            level=2,
            label="autocue",
            "Invalid cue_out/fade_out values: #{cue_out}/#{fade_out}"
          )
          (0., cue_out)
        else
          (fade_out, fade_out_start)
        end

      start_next = autocue.start_next ?? fade_out_start

      start_next =
        if
          start_next < cue_in or cue_out < start_next
        then
          log(
            level=2,
            label="autocue",
            "Invalid start_next: #{start_next}"
          )
          fade_out_start
        else
          start_next
        end

      fade_out_start_next = start_next - fade_out_start

      let fade_out_delay =
        if
          start_next < fade_out_start
        then
          fade_out_start - start_next
        else
          0.
        end

      total_fade_out = fade_out + fade_out_delay

      max_start_duration = cue_out - cue_in - total_fade_out

      opt_arg = fun (lbl, v) -> null.defined(v) ? [(lbl, string(v))] : []

      [
        ("liq_autocue", implementation_name),
        ...opt_arg("liq_amplify", amplify),

        ("liq_cue_in", string(cue_in)),
        ("liq_cue_out", string(cue_out)),
        ("liq_cross_start_duration", string(fade_in)),
        ("liq_cross_max_start_duration", string(max_start_duration)),
        ("liq_cross_end_duration", string(total_fade_out)),
        ("liq_fade_in", string(fade_in)),
        ...opt_arg("liq_fade_in_type", fade_in_type),
        ...opt_arg("liq_fade_in_curve", fade_in_curve),

        ("liq_fade_out", string(fade_out)),
        ("liq_fade_out_start_next", string(fade_out_start_next)),
        ("liq_fade_out_delay", string(fade_out_delay)),
        ...opt_arg("liq_fade_out_type", fade_out_type),
        ...opt_arg("liq_fade_out_curve", fade_out_curve),
        ...extra_metadata
      ]
    else
      log(
        level=2,
        label="autocue.metadata",
        "No autocue data returned for file #{uri}"
      )
      []
    end
  end
end

# Enable autocue metadata resolver. This resolver will process any file
# decoded by Liquidsoap and add cue-in/out and crossfade metadata when these
# values can be computed. This function sets `settings.request.prefetch` to `2`
# to account for the latency introduced by the `autocue` computation when resolving
# reausts. For a finer-grained processing, use the `autocue:` protocol.
# @category Liquidsoap
def enable_autocue_metadata() =
  if settings.request.prefetch() == 1 then settings.request.prefetch := 2 end

  def autocue_metadata(~metadata, fname) =
    autocue_metadata = file.autocue.metadata(metadata=metadata, fname)

    all_amplify = [...settings.autocue.amplify_aliases(), "liq_amplify"]
    user_supplied_amplify =
      list.filter_map(
        fun (el) ->
          if list.mem(fst(el), all_amplify) then snd(el) else null() end,
        metadata
      )
    user_supplied_amplify_labels =
      string.concat(
        separator=
          ", ",
        user_supplied_amplify
      )

    autocue_metadata =
      if
        settings.autocue.amplify_behavior() == "ignore"
      then
        [...list.assoc.remove("liq_amplify", autocue_metadata)]
      else
        if
          user_supplied_amplify != []
        then
          if
            settings.autocue.amplify_behavior() == "keep"
          then
            log(
              level=3,
              label="autocue.metadata",
              "User-supplied amplify metadata detected: #{
                user_supplied_amplify_labels
              }, keeping user-provided data."
            )
            list.assoc.remove("liq_amplify", autocue_metadata)
          elsif
            settings.autocue.amplify_behavior() == "override"
          then
            log(
              level=3,
              label="autocue.metadata",
              "User-supplied amplify metadata detected: #{
                user_supplied_amplify_labels
              }, overriding with autocue data."
            )
            [
              ...autocue_metadata,
              # This replaces all user-provided tags with the value returned by
              # the autocue implementation.
              ...list.map(
                fun (lbl) -> (lbl, autocue_metadata["liq_amplify"]),
                user_supplied_amplify
              )
            ]
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
  decoder.metadata.add(
    priority=settings.autocue.metadata.priority, "autocue", autocue_metadata
  )
end

# Define autocue protocol
# @flag hidden
def protocol.autocue(~rlog=_, ~maxtime=_, arg) =
  cue_metadata = file.autocue.metadata(metadata=[], arg)

  if
    cue_metadata != []
  then
    cue_metadata =
      list.map(fun (el) -> "#{fst(el)}=#{string.quote(snd(el))}", cue_metadata)
    cue_metadata = string.concat(separator=",", cue_metadata)
    "annotate:#{cue_metadata}:#{arg}"
  else
    log(
      level=2,
      label="autocue.protocol",
      "No autocue data returned for URI #{arg}!"
    )
    arg
  end
end
protocol.add(
  "autocue",
  protocol.autocue,
  doc=
    "Adding automatically computed cues/crossfade metadata",
  syntax="autocue:uri"
)
