%include "../test.liq"

# encoder-packed-audio.md §7, known-complexity.md KC18: the `%mp3` segment
# starting at 184 s with `title = "T1"` starts with the example's bytes.
dir = file.temp_dir("hls_id3_bytes")
on_cleanup({file.rmdir(dir)})
out = "#{dir}/out"

expected =
  string.concat(
    [
      "49443303000000000050",
      "5449543200000007c000",
      "01fffe54003100",
      "5052495600000035c000",
      "636f6d2e6170706c652e73747265616d",
      "696e672e7472616e73706f7274537472",
      "65616d54696d657374616d7000",
      "0000000000fcaf80"
    ]
  )

s = sine()
clock.assign_new(sync="none", [s])

def fail_on_error(err) =
  test.fail(
    "error while streaming: #{err}"
  )
  null
end

def hex(data) =
  string.concat(
    list.map(
      fun (n) ->
        string.hex_of_int(
          pad=2,
          string.binary.to_int(
            string.sub(encoding="ascii", data, start=n, length=1)
          )
        ),
      list.init(string.bytes.length(data), fun (n) -> n)
    )
  )
end

def check(event) =
  if
    not test.done() and event.path == "#{out}/a_93.mp3"
  then
    data = file.contents(event.path)
    head =
      hex(
        string.sub(
          encoding="ascii",
          data,
          start=0,
          length=string.length(expected) / 2
        )
      )
    if
      head != expected
    then
      test.fail(
        "a_93.mp3 starts with #{head}, expected #{expected}"
      )
    end
    test.pass()
  end
end

o =
  output.file.hls(
    reopen_on_error=fail_on_error,
    segment_duration=2.,
    out,
    [("a", %mp3)],
    s
  )

o.on_file_change(synchronous=true, check)
s.insert_metadata([("title", "T1")])
