%include "../test.liq"

# KC14: an existing output directory or temp_dir without write permission
# stops the script before streaming, naming the argument; each case runs in
# its own process since the error ends the script.
dir = file.temp_dir("hls_unwritable_dir")
on_cleanup(
  {
    ignore(
      process.run(
        "chmod -R u+w #{process.quote(dir)}"
      )
    )
    file.rmdir(dir)
  }
)
locked = "#{dir}/locked"
file.mkdir(perms=0o500, locked)
out = string.quote("#{dir}/out")
is_root =
  string.trim(
    process.read(
      "id -u"
    )
  ) ==
    "0"
stdlib = "../../src/libs/stdlib.liq"
liquidsoap_command =
  process.quote(liquidsoap.executable) ^
    (
      file.exists(stdlib)
        ? " --stdlib #{stdlib}"
        : ""
    )

# Each case: name and the argument lines, the first naming the faulty one.
cases =
  [
    (
      "output directory",
      [
        string.quote(locked) ^ ",",
        "[(\"a\", %mp3)],"
      ]
    ),
    (
      "temp_dir",
      [
        "temp_dir=#{string.quote(locked)},",
        "#{out},",
        "[(\"a\", %mp3)],"
      ]
    )
  ]

checked = ref(0)

def run_case(c) =
  let (name, lines) = c
  script = "#{dir}/case_#{random.int()}.liq"
  faulty = list.hd(lines)
  file.write(
    data=string.concat(
      separator="\n",
      [
        "s = sine()",
        "output.file.hls(",
        ...lines,
        "s)",
        "thread.run(delay=3., shutdown)",
        ""
      ]
    ),
    script
  )
  result =
    process.run(
      timeout=180.,
      "#{liquidsoap_command} #{process.quote(script)} 2>&1"
    )
  log_text = result.stdout ^ result.stderr
  if
    result.status != "exit" or result.status.code == 0
  then
    test.fail(
      "#{name}: not rejected (#{result.status} #{result.status.code}):\n#{
        log_text
      }"
    )
  end
  if
    string.index(
      substring="Starting top-level clock",
      log_text
    ) >=
      0
  then
    test.fail(
      "#{name}: streaming started before the error:\n#{log_text}"
    )
  end
  if
    string.index(
      substring="Error 7: Invalid value",
      log_text
    ) <
      0
    or string.index(substring="\n#{faulty}\n", log_text) < 0
    or string.index(
      substring="not writable",
      log_text
    ) <
      0
  then
    test.fail(
      "#{name}: not an invalid value at #{faulty} saying not writable:\n#{
        log_text
      }"
    )
  end
  ref.incr(checked)
  print(
    "rejected: #{name}"
  )
end

def run_all() =
  if
    is_root
  then
    print(
      "skipped: root writes into read-only directories"
    )
  else
    list.iter(run_case, cases)
    if
      checked() != 2
    then
      test.fail(
        "#{checked()} cases checked"
      )
    end
  end
  test.pass()
end

thread.run(run_all)
