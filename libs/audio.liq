# Samplerate for audio.
# @category Liquidsoap
def audio.samplerate =
  setting.getter.int("frame.audio.samplerate")
end

# Channels for audio.
# @category Liquidsoap
def audio.channels =
  setting.getter.int("frame.audio.channels")
end
