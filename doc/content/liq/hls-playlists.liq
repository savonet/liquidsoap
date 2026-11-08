radio = mksafe(playlist("~/music"))

# BEGIN
output.file.hls(
  "/var/www/hls",
  playlist="live.m3u8",
  segment_duration=4.,
  segments=8,
  segments_overhead=10,
  prefix="https://cdn.example.org/live",
  extra_tags=["#EXT-X-INDEPENDENT-SEGMENTS"],
  [
    (
      "aac",
      %fdkaac(bitrate = 128).{extra_tags = ["#EXT-X-START:TIME-OFFSET=-12"]}
    )
  ],
  radio
)
# END
