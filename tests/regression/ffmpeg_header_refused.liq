%include "../test.liq"

# A container header the muxer refuses fails the output: here, an MP4 with
# movflags that do not fragment it, to a pipe, which it cannot write without
# seeking.

def on_error(_) =
  print(
    "error raised"
  )
  test.pass()
  null
end

output.external(
  reopen_on_error=on_error,
  %ffmpeg(
    format = "mp4",
    movflags = "+default_base_moof",
    %audio(codec = "aac")
  ),
  "cat > /dev/null",
  sine()
)
