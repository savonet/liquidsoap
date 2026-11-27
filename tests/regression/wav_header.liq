%include "../test.liq"

# A %wav header's sizes cover the file on a file, appended runs included, and
# are the values readers take as "to the end of the stream" on a process.
# More than 16 bits or 2 channels use WAVE_FORMAT_EXTENSIBLE, the only tag
# defined for them.

pending = ref(4)

def bytes(filename, start, length) =
  string.sub(
    encoding="ascii",
    file.contents(filename),
    start=start,
    length=length
  )
end

def size(filename, start) =
  string.binary.to_int(little_endian=true, bytes(filename, start, 4))
end

def done(name, ok) =
  print(
    "#{name}: #{ok ? "ok" : "failed"}"
  )
  if
    ok
  then
    pending := pending() - 1
    if pending() == 0 then test.pass() end
  else
    test.fail()
  end
end

def source() =
  s = once(sine(duration=0.5))
  clock.assign_new(sync="none", [s])
  s
end

def probe(filename) =
  string.trim(
    process.read(
      "ffprobe -v error -show_entries stream=codec_name -of csv=p=0 #{
        process.quote(filename)
      }"
    )
  )
end

def check_file(name, format, ~tag, ~codec, ~header_length) =
  def on_close(filename) =
    length = file.size(filename)
    done(
      name,
      probe(filename) == codec
      and bytes(filename, 20, 2) == tag
      and size(filename, 4) == length - 8
      and size(filename, header_length - 4) == length - header_length
    )
  end

  output.file(
    fallible=true,
    on_close=on_close,
    format,
    file.temp(cleanup=true, "wav_header_#{name}", ".wav"),
    source()
  )
end

check_file("s16", %wav, tag="\x01\x00", codec="pcm_s16le", header_length=44)
check_file(
  "s24",
  %wav(samplesize = 24),
  tag="\xfe\xff",
  codec="pcm_s24le",
  header_length=68
)

# A second run appended to the file of a first one extends its stream: one
# header, whose sizes cover both.
appended = file.temp(cleanup=true, "wav_header_append", ".wav")

def append_second(first_length) =
  def on_close(filename) =
    length = file.size(filename)
    done(
      "append",
      length == 2 * first_length - 44
      and size(filename, 4) == length - 8
      and size(filename, 40) == length - 44
      and not string.contains(
        encoding="ascii",
        substring="RIFF",
        string.sub(
          encoding="ascii",
          file.contents(filename),
          start=4,
          length=length - 4
        )
      )
    )
  end

  output.file(
    fallible=true,
    append=true,
    on_close=on_close,
    %wav,
    appended,
    source()
  )
end

output.file(
  fallible=true,
  on_close=fun (filename) -> append_second(file.size(filename)),
  %wav,
  appended,
  source()
)

# A process is not seekable: the streaming form.
piped = file.temp(cleanup=true, "wav_header_pipe", ".wav")
o =
  output.external(
    fallible=true,
    %wav,
    "cat > #{process.quote(piped)}",
    source()
  )
o.on_stop(
  synchronous=true,
  fun () ->
    thread.run(
      delay=0.5,
      {
        done(
          "pipe",
          size(piped, 4) == 0xffffffff and probe(piped) == "pcm_s16le"
        )
      }
    )
)
