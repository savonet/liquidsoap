%ifndef input.alsa
let input.alsa = blank
%endif

%ifndef output.alsa
let output.alsa = output.dummy
%endif

# BEGIN
# Set correct frame size:
# This makes it possible to set any audio frame size.
# Make sure that you do NOT use video in this case!
video.frame.rate := 0

# Now set the audio frame size exactly as required:
settings.frame.audio.size := 2048

input = input.alsa()
output.alsa(input)

# END
