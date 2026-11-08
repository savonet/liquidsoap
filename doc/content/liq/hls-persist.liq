radio = mksafe(playlist("~/music"))

# BEGIN
def on_error(err) =
  log.important(
    "HLS output failed: #{err}"
  )
  5.
end

output.file.hls(
  "/var/www/hls",
  persist_at="state.json",
  strict_persist=true,
  reopen_on_error=on_error,
  [("aac", %fdkaac(bitrate = 128))],
  radio
)
# END
