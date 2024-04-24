def hflip(video_track) =
  def mkfilter(graph) =
    video_track = ffmpeg.filter.video.input(graph, video_track)
    video_track = ffmpeg.filter.hflip(graph, video_track)
    ffmpeg.filter.video.output(graph, video_track)
  end

  ffmpeg.filter.create(mkfilter)
end
