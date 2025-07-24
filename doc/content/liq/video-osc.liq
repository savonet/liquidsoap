# Set the OSC port to match TouchOSC's default port
settings.osc.port := 8000

# Input from the webcam with sound
cam = input.v4l2()
mic = input.pulseaudio()
s = source.mux.audio(audio=mic, cam)
s = mksafe(s)

# We get the angle from fader 3
angle = osc.float("/1/fader3", 0.)

# we rescale the position of fader 3 so that it corresponds to a 2π rotation
angle = fun () -> angle() * 2. * 3.1416

# ...and we rotate the video according to the angle
s = video.rotate(angle=angle, s)

# Change brightness according to fader 1
s = video.frei0r.brightness(brightness=osc.float("/1/fader1", 0.5), s)

# Change contrast according to fader 2
s = video.frei0r.contrast0r(contrast=osc.float("/1/fader2", 0.5), s)

# We have to buffer here otherwise we get clocks problems
s = buffer(s)

# Output sound and video
output.pulseaudio(fallible=true, s)
output.sdl(fallible=true, s)
