%include "../test.liq"

# Native %opus dropped the input left over from its last whole frame and the
# encoder's lookahead, then ended the stream with an empty page at granule
# position + 1. With DTX, packets were dropped without their duration, so the
# granule position lagged behind the media. The last page must hold the last
# packet, with the end-of-stream flag and granule position pre-skip + input.

settings.frame.audio.samplerate := 48000

pending = ref(2)

def done() =
  pending := pending() - 1
  if pending() == 0 then test.pass() end
end

def bytes(data, start, length) =
  string.binary.to_int(
    little_endian=true,
    string.sub(encoding="ascii", data, start=start, length=length)
  )
end

def rec last_page(data, offset) =
  segments = bytes(data, offset + 26, 1)
  body = ref(0)
  for i = 0 to segments - 1 do
    body := body() + bytes(data, offset + 27 + i, 1)

  end
  next = offset + 27 + segments + body()
  if
    next >= string.bytes.length(data)
  then
    {
      flags = bytes(data, offset + 5, 1),
      granulepos = bytes(data, offset + 6, 8),
      segments = segments
    }
  else
    last_page(data, next)
  end
end

def pre_skip(data) =
  let [head, ..._] = r/OpusHead/.split(data)
  bytes(data, string.bytes.length(head) + 10, 2)
end

def run(name, fmt, s) =
  clock.assign_new(sync="none", [s])

  def on_close(filename) =
    data = file.contents(filename)
    page = last_page(data, 0)
    expected = pre_skip(data) + 96000
    print(
      "#{name}: last page flags=#{page.flags} segments=#{page.segments} \
       granulepos=#{page.granulepos}, expected #{expected}"
    )

    if
      page.flags == 4 and page.segments > 0 and page.granulepos == expected
    then
      done()
    else
      test.fail()
    end
  end

  output.file(
    fallible=true,
    on_close=on_close,
    fmt,
    file.temp(cleanup=true, "opus_end_of_stream_#{name}", ".opus"),
    s
  )
end

run("sine", %opus(samplerate = 48000), once(sine(duration=2.)))
run("dtx", %opus(samplerate = 48000, dtx = true), once(blank(duration=2.)))
