img = single("weather.jpg")
cam = input.v4l2()
cam = video.alpha.of_color(color=0x0000ff, precision=0.2, cam)
s = add([img, cam])

# END
output.dummy(fallible=true, s)
