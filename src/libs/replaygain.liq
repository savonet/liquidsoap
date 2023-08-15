# Amplify source tracks according to replaygain metadata. This operator does not
# compute itself the replaygain: you can use either `enable_replaygain_metadata`
# or the `replaygain:` protocol for this.
# @category Source / Audio processing
# @param ~id Force the value of the source ID.
# @param ~ebu_r128 Also amplify according to EBU R128 tags.
# @param s Source to be amplified.
def replaygain(~id=null(), ~ebu_r128=true, s)
  # Normalize opus gain
  def f(m)
    def f(m)
      let (k, v) = m
      if k == "r128_track_gain" then
        v = int_of_string(v)
        v = v + 5 # normalize to -18 dB as usual replaygain instead of -23 dB
        v = lin_of_dB(float(v) / 256.)
        ("replaygain_track_gain", string.float(v))
      else
        (k, v)
      end
    end
    if ebu_r128 then list.map(f, m) else m end
  end
  s = metadata.map(f, s)
  amplify(id=id, override="replaygain_track_gain", 1., s)
end

# Compute the ReplayGain for a file (in dB).
# @category File
# @param ~ratio Decoding ratio. A value of `50` means try to decode the file `50x` faster than real time, if possible. Use this setting to lower CPU peaks when computing replaygain tags.
# @param fname File name.
def file.replaygain(~id=null(), ~ratio=50., fname)
  id = string.id.default(default="file.replaygain", id)
  try
    meta = file.metadata(exclude=["replaygain_track_gain"], fname)
    replaygain_meta = meta["replaygain_track_gain"]
    match = r/([\+\-\d\.]+)\s*dB/i.exec(replaygain_meta)
    gain = float_of_string(list.assoc(1, match))
    log.important(label=id, "Detected track replaygain #{gain} dB for #{string.quote(fname)}")
    gain
  catch _ do
    r = request.create(resolve_metadata=false, fname)
    if request.resolve(r) then
      log.important(label=id, "Computing replay gain for #{string.quote(fname)}")
      t = time()
      s = source.replaygain.compute(request.once(r))
      source.drop(ratio=ratio, s)
      gain = s.gain()
      log.important(label=id, "Computed replay gain #{gain} dB for #{string.quote(fname)} (time: #{time() - t} s).")
      gain
    else
      null()
    end
  end
end

# Enable ReplayGain metadata resolver. This resolver will process any file
# decoded by Liquidsoap and add a `replaygain_track_gain` metadata when this
# value could be computed. For a finer-grained replay gain processing, use the
# `replaygain:` protocol.
# @param ~ratio Decoding ratio. A value of `50.` means try to decode the file `50x` faster than real time, if possible. Use this setting to lower CPU peaks when computing replaygain tags.
# @category Liquidsoap
def enable_replaygain_metadata(~ratio=50.)
  def replaygain_metadata(~metadata=_, fname)
    meta = file.metadata(exclude=["replaygain_track_gain"], fname)
    replaygain_meta = meta["replaygain_track_gain"]
    if replaygain_meta != "" then
      log.important(label="decoder.replaygain.metadata", "Detected replaygain metadata #{replaygain_meta} for #{string.quote(fname)}")
      [("replaygain_track_gain", replaygain_meta)]
    else
      gain = file.replaygain(ratio=ratio, fname)
      if null.defined(gain) then
        [("replaygain_track_gain","#{null.get(gain)} dB")]
      else
        []
      end
    end
  end
  decoder.metadata.add("replaygain_track_gain", replaygain_metadata)
end
