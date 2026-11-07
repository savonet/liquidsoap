%include "../test.liq"

# operator.md §2, variants.md §2: each construction error stops the script
# before streaming, as an invalid value pointing at the argument; each case
# runs in its own process since the error ends the script.
dir = file.temp_dir("hls_construction_errors")
on_cleanup({file.rmdir(dir)})
not_a_dir = "#{dir}/not_a_dir"
file.write(data="", not_a_dir)
read_only = "#{dir}/read_only"
file.mkdir(perms=0o555, read_only)
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

# Each case: name, the argument lines (the first names the faulty argument),
# and a substring of the error message.
cases =
  [
    (
      "output directory under a read-only one",
      [
        string.quote("#{read_only}/out") ^ ",",
        "[(\"a\", %mp3)],"
      ],
      "Cannot create directory"
    ),
    (
      "output directory is a file",
      [
        string.quote(not_a_dir) ^ ",",
        "[(\"a\", %mp3)],"
      ],
      "Not a directory"
    ),
    (
      "temp_dir read-only",
      [
        "temp_dir=#{string.quote(read_only)},",
        "#{out},",
        "[(\"a\", %mp3)],"
      ],
      read_only
    ),
    (
      "temp_dir is a file",
      [
        "temp_dir=#{string.quote(not_a_dir)},",
        "#{out},",
        "[(\"a\", %mp3)],"
      ],
      not_a_dir
    ),
    (
      "persist_at parent under a file",
      [
        "persist_at=#{string.quote(not_a_dir ^ '/sub/state')},",
        "#{out},",
        "[(\"a\", %mp3)],"
      ],
      not_a_dir
    ),
    (
      "segment_duration under one frame",
      [
        "segment_duration=0.01,",
        "#{out},",
        "[(\"a\", %mp3)],"
      ],
      "segment_duration"
    ),
    (
      "segments 0",
      [
        "segments=0,",
        "#{out},",
        "[(\"a\", %mp3)],"
      ],
      "segments"
    ),
    (
      "negative segments_overhead",
      [
        "segments_overhead=-1,",
        "#{out},",
        "[(\"a\", %mp3)],"
      ],
      "segments_overhead"
    ),
    (
      "no variant",
      ["[],", "#{out},"],
      "variant"
    ),
    (
      "duplicate variant names",
      [
        "[(\"a\", %mp3), (\"a\", %shine)],",
        "#{out},"
      ],
      "unique"
    ),
    (
      "multi-line extra_tags entry",
      [
        "extra_tags=[\"#EXT-X-A\\n#EXT-X-B\"],",
        "#{out},",
        "[(\"a\", %mp3)],"
      ],
      "tag"
    ),
    (
      "multi-line variant extra_tags entry",
      [
        "[(\"a\", %mp3().{extra_tags=[\"#EXT-X-A\\n#EXT-X-B\"]})],",
        "#{out},"
      ],
      "tag"
    ),
    (
      "id3=false on %mp3",
      [
        "[(\"a\", %mp3().{id3=false})],",
        "#{out},"
      ],
      "ID3"
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
        let (name, _, _) = c
        if
          is_root and string.contains(substring="read-only", name)
        then
          print(
            "skipped as root: #{name}"
          )
        else
          run_case(c)
          checked := checked() + 1
        end
      end,
    cases
  )

  if
    checked() < 12
  then
    test.fail(
      "only #{checked()} cases checked"
    )
  end
  test.pass()
end

thread.run(run_all)
