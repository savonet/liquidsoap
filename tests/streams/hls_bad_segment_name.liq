%include "../test.liq"

# files.md §2, operator.md §6: a segment name outside the directory, reserved
# or already tracked makes the output fail without calling
# `reopen_on_error`; each case runs in its own process since the failure
# ends it.
dir = file.temp_dir("hls_bad_segment_name")
on_cleanup({file.rmdir(dir)})
out = "#{dir}/out"
stdlib = "../../src/libs/stdlib.liq"
liquidsoap_command =
  process.quote(liquidsoap.executable) ^
    (
      file.exists(stdlib)
        ? " --stdlib #{stdlib}"
        : ""
    )

# Each case: name, the `segment_name` result, and a file it must not write.
cases =
  [
    (
      "parent directory",
      "\"../x.mp3\"",
      "#{dir}/x.mp3"
    ),
    (
      "absolute path",
      string.quote("#{dir}/abs.mp3"),
      "#{dir}/abs.mp3"
    ),
    (
      "media playlist name",
      "\"a.m3u8\"",
      ""
    ),
    (
      "tracked file",
      "\"same.mp3\"",
      ""
    )
  ]

def run_case(c) =
  let (name, segment_name, outside) = c
  script = "#{dir}/case_#{random.int()}.liq"
  file.write(
    data=string.concat(
      separator="\n",
      [
        "s = sine()",
        "clock.assign_new(sync=\"none\", [s])",
        "o = output.file.hls(",
        "  segment_duration=1.,",
        "  reopen_on_error=fun (_) -> begin print(\"REOPEN CALLED\") 0.1 end,",
        "  segment_name=fun (_) -> #{segment_name},",
        "  #{string.quote(out)}, [(\"a\", %mp3)], s)",
        "o.on_start(synchronous=true, fun () -> print(\"OUTPUT STARTED\"))",
        ""
      ]
    ),
    script
  )

  result =
    process.run(
      timeout=180.,
      "#{liquidsoap_command} #{process.quote(script)}"
    )

  log_text = result.stdout ^ result.stderr
  if
    result.status != "exit" or result.status.code == 0
  then
    test.fail(
      "#{name}: the output did not fail (#{result.status} #{
        result.status.code
      })"
    )
  end
  if
    not (
      string.index(
        substring="OUTPUT STARTED",
        log_text
      ) >=
        0
    )
  then
    test.fail(
      "#{name}: the output never started"
    )
  end
  if
    (
      string.index(
        substring="REOPEN CALLED",
        log_text
      ) >=
        0
    )
  then
    test.fail(
      "#{name}: reopen_on_error was called"
    )
  end
  if
    outside != "" and file.exists(outside)
  then
    test.fail(
      "#{name}: #{outside} was written"
    )
  end
  print(
    "failed without reopen: #{name}"
  )
end

def run_all() =
  list.iter(run_case, cases)
  test.pass()
end

thread.run(run_all)
