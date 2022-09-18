# Amplify source tracks according to replaygain metadata. This operator does not
# compute itself the replaygain: you can use either `enable_replaygain_metadata`
# or the `replaygain:` protocol for this.
# @category Source / Sound Processing
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
        ("replaygain_track_gain", string_of_float(v))
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
def file.replaygain(fname)
  s = source.replaygain.compute(once(single(fname)))
  source.drop(s)
  s.gain()
end
