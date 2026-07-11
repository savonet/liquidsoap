# Regression test for https://github.com/savonet/liquidsoap/issues/5261
# HLS output and temporary directories must be writable during initialization.

tmp_dir = file.temp_dir("GH5261")
on_cleanup({file.rmdir(tmp_dir)})

def make_unwritable(name) =
  dir = path.concat(tmp_dir, name)
  file.mkdir(dir)
  if not process.test("chmod 0555 #{process.quote(dir)}") then
    test.fail("Could not make test directory unwritable")
  end
  on_cleanup({ignore(process.run("chmod 0755 #{process.quote(dir)}"))})
  dir
end

def expect_invalid(~temp_dir="null", ~message, output_dir) =
  script = file.temp("GH5261", ".liq")
  on_cleanup({file.remove(script)})
  file.write(
    data="output.file.hls(start=false,temp_dir=#{temp_dir},#{
      string.quote(output_dir)
    },[(\"stream\",%ffmpeg(format=\"mpegts\",%audio(codec=\"aac\")))],sine())",
    script
  )
  result =
    process.run(
      "#{process.quote(liquidsoap.executable)} -q --no-stdlib #{
        process.quote(script)
      }"
    )
  child_output = result.stdout ^ result.stderr
  if
    result.status != "exit"
    or result.status.code == 0
    or not string.contains(substring=message, child_output)
  then
    test.fail("HLS did not reject an unwritable directory: #{child_output}")
  end
end

expect_invalid(
  message="Could not write to output directory",
  make_unwritable("output")
)
expect_invalid(
  temp_dir=string.quote(make_unwritable("temporary")),
  message="Could not write to temporary directory",
  path.concat(tmp_dir, "writable")
)
test.pass()
