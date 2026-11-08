%include "../test.liq"

# variants.md §2, operator.md §2: `%wav`, `%ogg` and `%flac` variants, and a
# variant with an empty name, are rejected before streaming, naming the
# variant; each case runs in its own process since the error ends the script.
dir = file.temp_dir("hls_construction_variants")
on_cleanup({file.rmdir(dir)})
out = string.quote("#{dir}/out")
stdlib = "../../src/libs/stdlib.liq"
liquidsoap_command =
  process.quote(liquidsoap.executable) ^
    (
      file.exists(stdlib)
        ? " --stdlib #{stdlib}"
        : ""
    )

# Each case: name, the argument lines (the first names the faulty argument),
# and a substring of the error message.
cases =
  [
    (
      "%wav",
      [
        "[(\"w\", %wav)],",
        "#{out},"
      ],
      "Variant w"
    ),
    (
      "%ogg",
      [
        "[(\"o\", %ogg(%vorbis))],",
        "#{out},"
      ],
      "Variant o"
    ),
    (
      "%flac",
      [
        "[(\"f\", %flac)],",
        "#{out},"
      ],
      "Variant f"
    ),
    (
      "empty variant name",
      [
        "[(\"\", %mp3)],",
        "#{out},"
      ],
      ""
    )
  ]

# Positional arguments must come in order: the variants line goes after the
# directory line when the case lists it first.
def arguments(lines) =
  first = list.hd(lines)
  rest = list.tl(lines)
  if string.contains(prefix="[", first) then [...rest, first] else lines end
end

def run_case(c) =
  let (name, lines, message) = c
  script = "#{dir}/case_#{string.length(name)}_#{random.int()}.liq"
  faulty = list.hd(lines)
  file.write(
    data=string.concat(
      separator="\n",
      [
        "s = sine()",
        "output.file.hls(",
        ...arguments(lines),
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
  log_lines = string.split(separator="\n", log_text)
  excerpt =
    string.concat(
      separator="\n",
      list.filter(
        fun (l) -> not string.contains(prefix="20", l) and l != "",
        log_lines
      )
    )
  if
    result.status != "exit" or result.status.code == 0
  then
    test.fail(
      "#{name}: not rejected (#{result.status} #{result.status.code}):\n#{
        excerpt
      }"
    )
  end
  if
    (
      string.index(
        substring="Starting top-level clock",
        log_text
      ) >=
        0
    )
  then
    test.fail(
      "#{name}: streaming started before the error:\n#{excerpt}"
    )
  end
  if
    not (
      string.index(
        substring="Error 7: Invalid value",
        log_text
      ) >=
        0
    )
  then
    test.fail(
      "#{name}: not an invalid-value error:\n#{excerpt}"
    )
  end
  if
    not (string.index(substring="\n#{faulty}\n", log_text) >= 0)
  then
    test.fail(
      "#{name}: the error does not point at #{faulty}:\n#{excerpt}"
    )
  end
  if
    not (string.index(substring=message, log_text) >= 0)
  then
    test.fail(
      "#{name}: message does not mention #{message}:\n#{excerpt}"
    )
  end
  print(
    "rejected: #{name}"
  )
end

checked = ref(0)

def run_all() =
  list.iter(
    fun (c) ->
      begin
        run_case(c)
        checked := checked() + 1
      end,
    cases
  )

  if
    checked() < 4
  then
    test.fail(
      "only #{checked()} cases checked"
    )
  end
  test.pass()
end

thread.run(run_all)
