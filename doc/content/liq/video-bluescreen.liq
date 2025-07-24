# The video of the bunny
s = single("big_buck_bunny_720p_stereo.ogg")

# Input from the webcam
cam = input.v4l2()

# Flip the video around a vertical axis so that it is easier to position
# yourself
cam = video.frei0r.flippo(x_axis=true, cam)

# Make the white background transparent I had to tweak the precision parameter
# so that I will be seen but not the wall
cam = video.alpha.of_color(color=0xffffff, precision=0.64, cam)

# Superpose the two videos
s = add([s, cam])

# Output to SDL
output.sdl(fallible=true, s)
