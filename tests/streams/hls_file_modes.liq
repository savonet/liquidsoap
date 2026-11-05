%include "../test.liq"

# files.md §1, §3, known-complexity.md KC14: under umask 022 and the default
# `perm` and `dir_perm`, files are 0644 and directories 0755.
file.umask.set(0o022)
dir = file.temp_dir("hls_file_modes")
on_cleanup({file.rmdir(dir)})
out = "#{dir}/x/y"

s = sine()
clock.assign_new(sync="none", [s])

def fail_on_error(err) =
  test.fail(
    "error while streaming: #{err}"
  )
  null
end

def mode(file_path) =
  string.trim(
    process.read(
      "stat -c %a #{process.quote(file_path)}"
    )
  )
end

checks = ref(0)

def expect_mode(file_path, expected) =
  m = mode(file_path)
  if
    m != expected
  then
    test.fail(
      "#{file_path} has mode #{m}, not #{expected}"
    )
  end
  checks := checks() + 1
end

def check(event) =
  if
    not test.done() and event.state != "deleted"
  then
    expect_mode(event.path, "644")
    if
      event.path == "#{out}/sub/dir/a_3.mp3"
    then
      list.iter(
        fun (d) -> expect_mode(d, "755"),
        ["#{dir}/x", out, "#{out}/sub", "#{out}/sub/dir"]
      )
      if
        checks() < 8
      then
        test.fail(
          "only #{checks()} modes checked"
        )
      end
      print(
        "#{checks()} modes checked"
      )
      test.pass()
    end
  end
end

o =
  output.file.hls(
    reopen_on_error=fail_on_error,
    segment_duration=1.,
    segment_name=fun (segment) ->
      "sub/dir/#{segment.stream_name}_#{segment.position}.#{segment.extname}",
    out,
    [("a", %mp3)],
    s
  )

o.on_file_change(synchronous=true, check)
