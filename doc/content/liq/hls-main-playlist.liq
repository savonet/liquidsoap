radio = mksafe(playlist("~/music"))

# BEGIN
def main_playlist(~extra_tags, ~prefix, ~version, streams) =
  # Highest bandwidth first.
  streams = list.sort(fun (a, b) -> b.bandwidth - a.bandwidth, streams)
  hls.playlist.main(
    extra_tags=extra_tags,
    prefix=prefix,
    version=version,
    streams
  )
end

output.file.hls(
  "/var/www/hls",
  main_playlist_writer=main_playlist,
  [("lofi", %fdkaac(bitrate = 48)), ("hifi", %fdkaac(bitrate = 192))],
  radio
)
# END
