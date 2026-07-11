# Regression test for https://github.com/savonet/liquidsoap/issues/5259
# A transient HLS filesystem error must not terminate the process.

tmp_dir = file.temp_dir("GH5259")
on_cleanup({file.rmdir(tmp_dir)})

hls_dir = path.concat(tmp_dir, "hls")
saved_dir = path.concat(tmp_dir, "hls.saved")
made_unavailable = ref(false)
error_seen = ref(false)

def on_file_change({state, path = fname}) =
  if
    state == "created"
    and string.contains(prefix="stream_", path.basename(fname))
  then
    if not made_unavailable() then
      made_unavailable := true
      file.move(atomic=true, hls_dir, saved_dir)
      file.write(data="not a directory", hls_dir)
    elsif error_seen() then
      test.pass()
    end
  end
end

def on_error(_) =
  error_seen := true
  file.remove(hls_dir)
  file.move(atomic=true, saved_dir, hls_dir)
  0.1
end

output.dummy(id="primary", mksafe(buffer(sine())))

o =
  output.file.hls(
    fallible=true,
    reopen_on_error=on_error,
    segment_duration=0.5,
    hls_dir,
    [("stream", %ffmpeg(format = "mpegts", %audio(codec = "aac")))],
    sine()
  )

o.on_file_change(synchronous=true, on_file_change)

thread.run(delay=20., {test.fail("HLS output did not recover")})
