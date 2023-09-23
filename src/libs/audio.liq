audio = ()

let audio.encode = ()

# Encode audio track to pcm_s16
# @category Source / Audio processing
def audio.encode.pcm_s16(~id=null("audio.encode.pcm_s16"), s) =
  let {audio, ...tracks} = source.tracks(s)
  source(id=id, tracks.{audio = track.encode.audio.pcm_s16(audio)})
end

# Encode audio track to pcm_f32
# @category Source / Audio processing
def audio.encode.pcm_f32(~id=null("audio.encode.pcm_f32"), s) =
  let {audio, ...tracks} = source.tracks(s)
  source(id=id, tracks.{audio = track.encode.audio.pcm_f32(audio)})
end

let audio.decode = ()

# Decode audio track to pcm_s16
# @category Source / Audio processing
def audio.decode.pcm_s16(~id=null("audio.decode.pcm_s16"), s) =
  let {audio, ...tracks} = source.tracks(s)
  source(id=id, tracks.{audio = track.decode.audio.pcm_s16(audio)})
end

# Decode audio track to pcm_f32
# @category Source / Audio processing
def audio.decode.pcm_f32(~id=null("audio.decode.pcm_f32"), s) =
  let {audio, ...tracks} = source.tracks(s)
  source(id=id, tracks.{audio = track.decode.audio.pcm_f32(audio)})
end

# Samplerate for audio.
# @category Settings
def audio.samplerate =
  settings.frame.audio.samplerate
end

# Channels for audio.
# @category Settings
def audio.channels =
  settings.frame.audio.channels
end

# Multiply the amplitude of the signal.
# @category Source / Audio processing
# @param f Multiplicative factor.
# @argsof track.audio.amplify
def amplify(~id=null("amplify"), %argsof(track.audio.amplify[!id]), f, s) =
  tracks = source.tracks(s)
  source(
    id=id,
    tracks.{
      audio=track.audio.amplify(
        id=null("track.amplify"),
        %argsof(track.audio.amplify[!id]),
        f,
        tracks.audio
      )
    }
  )
end

# Clip samples, i.e. ensure that all values are between
# `-1` and `1`: values lower than `-1` become `-1` and
# values higher than `1` become `1`. `nan` values become `0`.
# @category Source / Audio processing
# @argsof track.audio.clip
def clip(~id=null("clip"), %argsof(track.audio.clip[!id]), s) =
  tracks = source.tracks(s)
  source(id=id, tracks.{audio=track.audio.clip(%argsof(track.audio.clip), tracks.audio)})
end

lufs_builtin = lufs

# Normalization the volume of a stream (this is also called _automatic gain
# control_). Dynamic normalization of the signal is sometimes the only option
# (for instance, for live sources), and can make a listening experience much
# nicer. However, its dynamic aspect implies some limitations which can go as
# far as creating saturation in some extreme cases. If possible, consider using
# some track-based normalization techniques such as those based on
# ReplayGain. The implementation of Liquidsoap < 2.0 was renamed to
# `normalize.old`.
# @category Source / Audio processing
# @param ~id Force the value of the source ID.
# @param ~gain_max Maximal gain value (dB).
# @param ~gain_min Minimal gain value (dB).
# @param ~down Characteristic time to go down.
# @param ~up Characteristic time to go up.
# @param ~lookahead How much time to look ahead of the signal (second). Setting a positive value delays the output by the corresponding amount of time.
# @param ~lufs Use LUFS instead of RMS to compute intensity.
# @param ~target Desired RMS (dB).
# @param ~threshold Minimal RMS for activaing gain control (dB).
# @param ~window Duration of the window used to compute the current RMS power (second).
# @param ~debug How often to print debug messages, in seconds, useful to finetune the parameters. You should set `set("log.level", 5)` to see them.
# @param s Source to normalize.
# @method gain Current amplification coefficient (in linear scale).
# @method target_gain Current target amplification coefficient (in linear scale).
# @method rms Current rms (in linear scale).
def replaces normalize(~id=null(), ~target=getter(-13.), ~up=getter(10.), ~down=getter(.1), ~gain_min=-12., ~gain_max=12., ~lufs=false, ~lookahead=getter(0.), ~window=getter(.5), ~threshold=getter(-40.), ~track_sensitive=true, ~debug=null(), s)
  let (s, rms) =
    if lufs then
      s = lufs_builtin(id=id, window=window, s)
      (s, {lin_of_dB(s.lufs())})
    else
      s = rms.smooth(id=id, duration=window, s)
      (s, s.rms)
    end
  v = ref(1.)
  frame = frame.duration()
  gain_min = lin_of_dB(gain_min)
  gain_max = lin_of_dB(gain_max)
  def update()
    target    = lin_of_dB(getter.get(target))
    threshold = lin_of_dB(getter.get(threshold))
    rms       = rms()
    if rms >= threshold then
      if v() * rms <= target then
        up = 1. - exp (0. - frame / getter.get(up))
        v := v() + up * ((target / rms) - v())
      else
        down = 1. - exp (0. - frame / getter.get(down))
        v := v() + down * ((target / rms) - v())
      end
      v := max(gain_min, min(gain_max, v()))
    end
  end
  def target_gain () = lin_of_dB(getter.get(target)) / rms() end
  s =
    if null.defined(debug) then
      source.run(s, every=null.get(debug), {log.debug("rms: #{rms()} / #{lin_of_dB(getter.get(target))}\tgain: #{v()} / #{target_gain()}")})
    else
      s
    end
  s = source.on_frame(s, update)
  s = if track_sensitive then source.on_track(s, fun (_) -> v := 1.) else s end
  amplify(id=id, {v()}, delay_line(lookahead, s)).{ rms = rms, gain = {v()}, target_gain = target_gain }
end

# Swap two channels of a stereo source.
# @category Source / Conversion
def swap(id=null("swap"), s)
  tracks = source.tracks(s)
  source(id=id, tracks.{audio = track.audio.swap(tracks.audio)})
end

# Produce mono audio by taking the mean of all audio channels.
# @category Source / Conversion
# @argsof track.audio.mean
def mean(~id=null("mean"), %argsof(track.audio.mean[!id]), s)
  tracks = source.tracks(s)
  source(id=id, tracks.{audio = track.audio.mean(%argsof(track.audio.mean), tracks.audio)})
end

# Convert any pcm audio source into a stereo source.
# @category Source / Conversion
def stereo(~id=null("stereo"), s) =
  tracks = source.tracks(s)
  source(id=id, tracks.{audio=track.audio.stereo(tracks.audio)})
end

# Extract the left channel of a stereo track
# @category Source / Conversion
# @param t Track to extract from
def track.audio.stereo.left(~id=null("track.audio.stereo.left"), t)
  track.audio.mean(id=id, track.audio.stereo.pan(pan=-1., t))
end

# Extract the left channel of a stereo source
# @category Source / Conversion
# @param s Source to extract from
def stereo.left(~id=null("stereo.left"), s)
  tracks = source.tracks(s)
  source(id=id, tracks.{audio=track.audio.stereo.left(tracks.audio)})
end

# Extract the right channel of a stereo track
# @category Source / Conversion
# @param s Track to extract from
def track.audio.stereo.right(~id=null("track.audio.stereo.right"), t)
  track.audio.mean(id=id, track.audio.stereo.pan(pan=1., t))
end

# Extract the right channel of a stereo source
# @category Source / Conversion
# @param s Source to extract from
def stereo.right(~id=null("stereo.right"), s)
  tracks = source.tracks(s)
  source(id=id, tracks.{audio=track.audio.stereo.right(tracks.audio)})
end

# Spacializer which allows controlling the width of the signal.
# @category Source / Audio processing
# @param w Width of the signal (-1: mono, 0.: original, 1.: wide stereo).
def stereo.width(~id=null("stereo.width"), w=getter(0.), (s:source)) =
   tracks = source.tracks(s)
   source(id=id, tracks.{audio=track.audio.stereo.width(w, tracks.audio)})
end

# Pan a stereo sound.
# @category Source / Audio processing
# @argsof track.audio.stereo.pan
def stereo.pan(~id=null("stereo.pan"), %argsof(track.audio.stereo.pan[!id]), (s:source)) =
   tracks = source.tracks(s)
   source(id=id, tracks.{audio=track.audio.stereo.pan(%argsof(track.audio.stereo.pan), tracks.audio)})
end

# Slow down or accelerate an audio stream by stretching (sounds lower) or squeezing it (sounds higher).
# @category Source / Audio processing
# @argsof track.audio.stretch
def stretch(~id=null("stretch"), %argsof(track.audio.stretch[!id]), (s:source)) =
  tracks = source.tracks(s)
  source.audio(id=id, track.audio.stretch(%argsof(track.audio.stretch), tracks.audio))
end

let stereo.ms = ()

# Decode mid+side stereo (M/S) to left+right stereo.
# @category Source / Audio processing
# @argsof track.audio.stereo.ms.decode
def stereo.ms.decode(~id=null("stereo.ms.decode"), %argsof(track.audio.stereo.ms.decode[!id]), (s:source)) =
  tracks = source.tracks(s)
  source(id=id, tracks.{audio=track.audio.stereo.ms.decode(%argsof(track.audio.stereo.ms.decode), tracks.audio)})
end

# Encode left+right stereo to mid+side stereo (M/S).
# @category Source / Audio processing
# @argsof track.audio.stereo.ms.encode
def stereo.ms.encode(~id=null("stereo.ms.encode"), %argsof(track.audio.stereo.ms.encode[!id]), (s:source)) =
  tracks = source.tracks(s)
  source(id=id, tracks.{audio=track.audio.stereo.ms.encode(%argsof(track.audio.stereo.ms.encode), tracks.audio)})
end

%ifdef track.audio.stereotool
# Process an audio source using stereotool
# @argsof track.audio.stereotool
# @category Source / Audio processing
def stereotool(~id=null("stereotool"), %argsof(track.audio.stereotool[!id]), s) =
  let {audio, metadata, track_marks, ...} = source.tracks(s)
  s = track.audio.stereotool(%argsof(track.audio.stereotool[!id]), audio)
  let replaces s = source(id=id, {
    audio       = (s:pcm),
    metadata    = metadata,
    track_marks = track_marks
  })
  s
end
%endif
