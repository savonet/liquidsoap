# Regression test for https://github.com/savonet/liquidsoap/issues/5259
# A transient HLS filesystem error must not terminate the process.

tmp_dir = file.temp_dir("GH5259")
on_cleanup({file.rmdir(tmp_dir)})

hls_dir = path.concat(tmp_dir, "hls")
saved_dir = path.concat(tmp_dir, "hls.saved")
made_unavailable = ref(false)

# A segment can only be created again after `on_error` has restored `hls_dir`,
# so a second "created" event proves the output recovered from the error.
def on_file_change({state, path = fname}) =
  if
    state == "created"
    and string.contains(prefix="stream_", path.basename(fname))
  then
    if not made_unavailable() then
      made_unavailable := true
      file.move(atomic=true, hls_dir, saved_dir)
      file.write(data="not a directory", hls_dir)
    else
      test.pass()
    end
  end
end

def on_error(_) =
  file.remove(hls_dir)
  file.move(atomic=true, saved_dir, hls_dir)
  0.1
end

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
