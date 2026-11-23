%include "../test.liq"

# A container header the muxer refuses fails the output. It used to count as
# a fault of each item, reset by every frame that produced no packet, so the
# output streamed nothing forever with one log line: here, plain MP4 to a
# pipe, which MP4 cannot write without seeking.

def on_error(_) =
  print(
    "error raised"
  )
  test.pass()
  null
end

output.external(
  reopen_on_error=on_error,
  %ffmpeg(format = "mp4", %audio(codec = "aac")),
  "cat > /dev/null",
  sine()
)
