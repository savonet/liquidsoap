s1 = input.rtmp(listen=false, "rtmp://....")
s1 = ffmpeg.filter.bitstream.h264_mp4toannexb(s1)

s2 = playlist("/path/to/playlist")
s2 = ffmpeg.filter.bitstream.h264_mp4toannexb(s2)

s = fallback(track_sensitive=false, [s1, s2])

mpegts =
  %ffmpeg(format = "mpegts", fflags = "-autobsf", %audio.copy, %video.copy)

streams = [("mpegts", mpegts)]

output_dir = "/tmp/hls"

output.file.hls(
  playlist="live.m3u8",
  fallible=true,
  segment_duration=5.,
  output_dir,
  streams,
  s
)
