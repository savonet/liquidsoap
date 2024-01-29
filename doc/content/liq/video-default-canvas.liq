# Standard video canvas based off a `10k` virtual canvas.
# @category Source / Video processing
def video.canvas.virtual_10k =
  def make(height, width) =
    video.canvas.make(
      virtual_height=10000,
      actual_size={width=width, height=height},
      font_size=160
    )
  end

  {
    actual_360p=make(640, 360),
    actual_480p=make(640, 480),
    actual_720p=make(1280, 720),
    actual_1080p=make(1920, 1080),
    actual_1440p=make(2560, 1440),
    actual_4k=make(3840, 2160),
    actual_8k=make(7680, 4320)
  }
end
