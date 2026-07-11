# Regression test for https://github.com/savonet/liquidsoap/issues/5261
# HLS output, temporary and persistence directories must be writable during
# initialization.

tmp_dir = file.temp_dir(cleanup=true, "GH5261")

def make_unwritable(name) =
  dir = path.concat(tmp_dir, name)
  file.mkdir(dir)
  if
    not process.test(
      "chmod 0555 #{process.quote(dir)}"
    )
  then
    test.fail(
      "Could not make test directory unwritable"
    )
  end
  on_cleanup(
    {
      ignore(
        process.run(
          "chmod 0755 #{process.quote(dir)}"
        )
      )
    }
  )
  dir
end

def expect_invalid(~args="", ~message, output_dir) =
  code =
    "output.file.hls(start=false,#{args}#{
      string.quote(output_dir)
    },[(\"stream\",%ffmpeg(format=\"mpegts\",%audio(codec=\"aac\")))],sine())"
  result =
    process.run(
      timeout=30.,
      "#{process.quote(liquidsoap.executable)} -q --no-stdlib #{
        process.quote(code)
      }"
    )
  child_output = result.stdout ^ result.stderr
  if
    result.status != "exit"
    or result.status.code == 0
    or not string.contains(substring=message, child_output)
  then
    test.fail(
      "HLS did not reject an unwritable directory: #{child_output}"
    )
  end
end

writable_dir = path.concat(tmp_dir, "writable")

expect_invalid(
  message="Could not write to output directory",
  make_unwritable("output")
)

temporary_dir = make_unwritable("temporary")
expect_invalid(
  args="temp_dir=#{string.quote(temporary_dir)},",
  message="Could not write to temporary directory",
  writable_dir
)

persist_file = path.concat(make_unwritable("persist"), "state.json")
expect_invalid(
  args="persist_at=#{string.quote(persist_file)},",
  message="Could not write to persistence directory",
  writable_dir
)

test.pass()
