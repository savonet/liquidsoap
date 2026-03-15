# Test that video dimensions are auto-detected from the first decoded file

s = once(single("nola_test_size.mp4"))

output_file = file.temp("liq", ".mp4", cleanup=true)

def on_close(_) =
  # Check the output file dimensions using ffprobe
  result =
    process.read(
      "ffprobe -v error -select_streams v:0 -show_entries stream=width,height \
       -of csv=s=x:p=0 #{process.quote(output_file)}"
    )
  result = string.trim(result)
  print(
    "Detected output dimensions: #{result}"
  )

  if result == "504x504" then test.pass() else test.fail() end
end

clock.assign_new(sync='none', [s])
output.file(
  fallible=true,
  on_close=on_close,
  %ffmpeg(format = "mp4", %video(codec = "libx264"), %audio(codec = "aac")),
  output_file,
  s
)
