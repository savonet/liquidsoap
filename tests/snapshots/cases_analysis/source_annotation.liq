# What a track holds is checked here, and shown.
def f(s) =
  ignore((s : source(audio=pcm(mono))))
  ignore((s : source(audio=pcm(stereo))))
end
ignore(f)
