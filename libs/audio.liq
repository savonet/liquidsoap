# Samplerate for audio.
# @category Liquidsoap
def audio.samplerate() =
  get(default=-1,"frame.audio.samplerate")
end

# Channels for audio.
# @category Liquidsoap
def audio.channels() =
  get(default=-1,"frame.audio.channels")
end
