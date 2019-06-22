# Samplerate for audio.
# @category Liquidsoap
def audio.samplerate() =
  get(default=-1,"frame.audio.samplerate")
end
