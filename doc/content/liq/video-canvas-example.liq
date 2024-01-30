# Change to video.canvas.virtual_10k.actual_720p etc.. to render
# in different sizes without changing the values below!
let {px, rem, vh, vw, width, height} = video.canvas.virtual_10k.actual_1080p

video.frame.width := width
video.frame.height := height

background =
  video.add_image(
    x=0.3 @ vw,
    y=0.01 @ vh,
    width=1562 @ px,
    height=1562 @ px,
    file="/path/to/cover.jpg",
    background
  )

background =
  video.add_text(
    color=0xFCB900,
    font=myfont,
    speed=0,
    x=234 @ px,
    y=4437 @ px,
    size=1.5 @ rem,
    {nowplaying()},
    background
  )
