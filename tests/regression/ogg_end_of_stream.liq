%include "../test.liq"

# Native Ogg streams must end on the page holding their last packet, with the
# end-of-stream flag and the granule position of the input's end. %opus and
# %theora appended an empty page at granule position + 1 instead. %opus also
# dropped the input left over from its last whole frame and the encoder's
# lookahead, and with DTX dropped packets without their duration, so the
# granule position lagged behind the media.

settings.frame.audio.samplerate := 48000

pending = ref(3)

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

# 2 seconds of audio at 48kHz, after the pre-skip.
def opus_end(data) =
  let [head, ..._] = r/OpusHead/.split(data)
  bytes(data, string.bytes.length(head) + 10, 2) + 96000
end

# 2 seconds of video at 25fps, with libtheora's default granule shift: the
# last key frame's index, shifted, plus the images since.
def theora_end(_) =
  key_frames = 1
  lsl(key_frames, 6) + 50 - key_frames
end

def run(name, fmt, expected, s) =
  clock.assign_new(sync="none", [s])

  def on_close(filename) =
    data = file.contents(filename)
    page = last_page(data, 0)
    expected = expected(data)
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
    file.temp(cleanup=true, "ogg_end_of_stream_#{name}", ".ogg"),
    s
  )
end

run("sine", %opus(samplerate = 48000), opus_end, once(sine(duration=2.)))
run(
  "dtx",
  %opus(samplerate = 48000, dtx = true),
  opus_end,
  once(blank(duration=2.))
)
run("theora", %ogg(%theora), theora_end, once(video.fill(blank(duration=2.))))
