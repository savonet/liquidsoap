%include "../test.liq"

# encoder-packed-audio.md §7, known-complexity.md KC18: every packed-audio
# segment starts with an ID3 tag whose PRIV timestamp is the segment's start
# in 90 kHz units, with frames that are not a whole number of 90 kHz units.
settings.frame.duration := 0.0232

dir = file.temp_dir("hls_id3_timestamp")
on_cleanup({file.rmdir(dir)})
out = "#{dir}/out"
segments_wanted = 25
owner = "com.apple.streaming.transportStreamTimestamp"

s = sine()
clock.assign_new(sync="none", [s])

def fail_on_error(err) =
  test.fail(
    "error while streaming: #{err}"
  )
  null
end

segment_ticks = ref([])
ticks_per_second = ref(0)

def segment_name(segment) =
  name = "#{segment.stream_name}_#{segment.position}.#{segment.extname}"
  if
    segment.ticks > 0
  then
    segment_ticks := (name, (segment.position, segment.ticks))::segment_ticks()
    if
      ticks_per_second() == 0
    then
      ticks_per_second :=
        int_of_float(float(segment.ticks) / segment.duration + 0.5)
    end
  end
  name
end

def byte(data, n) =
  string.binary.to_int(string.sub(encoding="ascii", data, start=n, length=1))
end

def be32(data, n) =
  string.binary.to_int(
    little_endian=false,
    string.sub(encoding="ascii", data, start=n, length=4)
  )
end

def syncsafe(data, n) =
  byte(data, n) * 2097152 + byte(data, n + 1) * 16384 +
    byte(data, n + 2) * 128 +
    byte(data, n + 3)
end

# The PRIV timestamp of the tag at the start of `data`, or a failure naming
# what is wrong.
def priv_timestamp(name, data) =
  if
    string.bytes.length(data) < 10
    or string.sub(encoding="ascii", data, start=0, length=3) != "ID3"
  then
    test.fail(
      "#{name} does not start with an ID3 tag"
    )
  end
  if
    byte(data, 3) != 3
  then
    test.fail(
      "#{name}: ID3v2.#{byte(data, 3)}, expected the default 3"
    )
  end
  tag_end = 10 + syncsafe(data, 6)
  timestamp = ref(-1)

  def rec frames(n) =
    if
      n + 10 <= tag_end
    then
      id = string.sub(encoding="ascii", data, start=n, length=4)
      size = be32(data, n + 4)
      if
        id == "PRIV"
      then
        body = string.sub(encoding="ascii", data, start=n + 10, length=size)
        if
          size != string.bytes.length(owner) + 9
          or string.sub(
            encoding="ascii",
            body,
            start=0,
            length=string.bytes.length(owner) + 1
          ) !=
            "#{owner}\000"
        then
          test.fail(
            "#{name}: PRIV frame #{string.quote(body)}"
          )
        end
        timestamp :=
          string.binary.to_int(
            little_endian=false,
            string.sub(
              encoding="ascii",
              body,
              start=string.bytes.length(owner) + 1,
              length=8
            )
          )
      end
      if id != "\000\000\000\000" and size > 0 then frames(n + 10 + size) end
    end
  end

  frames(10)
  if
    timestamp() < 0
  then
    test.fail(
      "#{name}: no PRIV frame"
    )
  end
  timestamp()
end

checks = ref(0)

def check_segment(event) =
  name = path.basename(event.path)
  if
    event.state == "created" and not string.contains(suffix=".m3u8", name)
  then
    let (segment_position, _) = list.assoc(name, segment_ticks())
    stream_name = list.hd(string.split(separator="_", name))
    start =
      list.fold(
        fun (t, el) ->
          begin
            let (other, (other_position, ticks)) = el
            if
              string.contains(prefix="#{stream_name}_", other)
              and other_position < segment_position
            then
              t + ticks
            else
              t
            end
          end,
        0,
        segment_ticks()
      )

    expected =
      int_of_float(float(start) * 90000. / float(ticks_per_second()) + 0.5)
    timestamp = priv_timestamp(name, file.contents(event.path))
    if
      timestamp != expected
    then
      test.fail(
        "#{name}: PRIV timestamp #{timestamp}, expected #{expected} (start #{
          start
        } ticks at #{ticks_per_second()} ticks/s)"
      )
    end
    checks := checks() + 1
    if
      segment_position == segments_wanted and stream_name == "c"
    then
      if
        checks() < 3 * segments_wanted
      then
        test.fail(
          "only #{checks()} segments checked"
        )
      end
      print(
        "#{checks()} segment timestamps checked"
      )
      test.pass()
    end
  end
end

o =
  output.file.hls(
    reopen_on_error=fail_on_error,
    segment_duration=1.,
    segment_name=segment_name,
    out,
    [
      ("a", %mp3),
      ("b", %shine(bitrate = 64)),
      ("c", %fdkaac(bitrate = 64, transmux = "adts"))
    ],
    s
  )

o.on_file_change(
  synchronous=true,
  fun (event) -> if not test.done() then check_segment(event) end
)
