%include "../test.liq"

# Each invalid argument stops the script with an invalid-value error whose
# position is that argument's value. In dedicated mode the burst is not
# checked against the buffer.
def f() =
  dir = file.temp_dir("harbor", "construction")

  # Runs `args` in a script of one line and returns its stderr when it fails.
  def run(name, args) =
    script = path.concat(dir, "#{name}.liq")
    file.write(
      data="output.harbor(#{args}, %mp3, sine())\nexit(0)\n",
      script
    )
    r =
      process.run(
        timeout=120.,
        "liquidsoap #{process.quote(script)}"
      )
    (r.status.code, r.stderr)
  end

  def invalid(name, args, value) =
    let (code, stderr) = run(name, args)
    if
      code != 1
    then
      test.fail(
        "#{name}: exit code #{code}\n#{stderr}"
      )
    end
    if
      not string.contains(
        encoding="ascii",
        substring="Invalid value",
        stderr
      )
    then
      test.fail(
        "#{name}: not an invalid value\n#{stderr}"
      )
    end
    line =
      "output.harbor(#{args}, %mp3, sine())"
    start = string.index(substring=value, line)
    length = string.length(value)
    position =
      length == 1
        ? "line 1, char #{start}:"
        : "line 1, char #{start}-#{start + length}"
    if
      not string.contains(encoding="ascii", substring=position, stderr)
    then
      test.fail(
        "#{name}: error not at #{position}\n#{stderr}"
      )
    end
  end

  invalid(
    "empty_mount",
    'port=18777, mount=""',
    '""'
  )
  invalid(
    "burst",
    "port=18777, mount=\"a\", buffer=100, burst=200",
    "100"
  )
  invalid(
    "header_value",
    'port=18777, mount="a", headers=[("X-A", "a\\r\\nb")]',
    '("X-A", "a\\r\\nb")'
  )
  invalid(
    "header_name",
    'port=18777, mount="a", headers=[("X A", "a")]',
    '("X A", "a")'
  )
  invalid(
    "encoding",
    'port=18777, mount="a", encoding="no-such-charset"',
    '"no-such-charset"'
  )
  invalid(
    "metaint",
    'port=18777, mount="a", metaint=0',
    "0"
  )
  invalid(
    "timeout",
    'port=18777, mount="a", timeout=0.',
    "0."
  )

  let (code, stderr) =
    run(
      "dedicated_burst",
      "port=18777, mount=\"a\", dedicated_encoder=true, buffer=100, burst=200"
    )
  if
    code != 0
  then
    test.fail(
      "dedicated burst refused\n#{stderr}"
    )
  end
  test.pass()
end

test.check(f)
