# Amplify source tracks according to replaygain metadata. This operator does not
# compute itself the replaygain: you can use either `enable_replaygain_metadata`
# or the `replaygain:` protocol for this.
# @category Source / Audio processing
# @param ~id Force the value of the source ID.
# @param ~ebu_r128 Also amplify according to EBU R128 tags.
# @param s Source to be amplified.
def replaygain(~id=null(), ~ebu_r128=true, s)
  s = insert_metadata(s)
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
# @param fname File name.
def file.replaygain(~id=null(), fname)
  id = string.id.default(default="file.replaygain", id)
  r = request.create(fname)
  if request.resolve(r) then
    log.info(label=id, "Computing replay gain for #{fname}.")
    t = time()
    s = source.replaygain.compute(request.once(r))
    source.drop(s)
    gain = s.gain()
    log.info(label=id, "Computed replay gain for #{fname}: #{gain} dB (in #{time() - t} s).")
    gain
  else
    null()
  end
end

# Enable ReplayGain metadata resolver. This resolver will process any file
# decoded by Liquidsoap and add a `replaygain_track_gain` metadata when this
# value could be computed. For a finer-grained replay gain processing, use the
# `replaygain:` protocol.
# @category Liquidsoap
def enable_replaygain_metadata()
  def replaygain_metadata(fname)
    gain = file.replaygain(fname)
    if null.defined(gain) then
      [("replaygain_track_gain","#{null.get(gain)} dB")]
    else
      []
    end
  end
  add_metadata_resolver("replaygain_track_gain", replaygain_metadata)
end
