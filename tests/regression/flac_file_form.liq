%include "../test.liq"

# On a file, native %flac writes its tags, its cover as a PICTURE block and a
# complete STREAMINFO; a process gets the encoder identification only.

pending = ref(2)

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

# A 1x1 PNG.
cover =
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg=="

def source() =
  s =
    metadata.map(
      insert_missing=true,
      fun (_) -> [("title", "foo"), ("coverart", cover)],
      once(sine(duration=0.5))
    )
  clock.assign_new(sync="none", [s])
  s
end

def probe(entries, filename) =
  string.trim(
    process.read(
      "ffprobe -v error -show_entries #{entries} -of csv=p=0 #{
        process.quote(filename)
      }"
    )
  )
end

def on_close(filename) =
  codecs = probe("stream=codec_name", filename)
  tags = probe("format_tags=title,encoder", filename)
  duration = float_of_string(default=0., probe("format=duration", filename))
  print(
    "file: #{string.quote(codecs)} #{string.quote(tags)} #{duration}"
  )
  done(
    "file",
    string.contains(substring="flac", codecs)
    and string.contains(substring="png", codecs)
    and string.contains(substring="foo", tags)
    and string.contains(substring="Liquidsoap", tags)
    and duration > 0.45
    and duration < 0.55
  )
end

output.file(
  fallible=true,
  on_close=on_close,
  %flac,
  file.temp(cleanup=true, "flac_file_form", ".flac"),
  source()
)

piped = file.temp(cleanup=true, "flac_file_form_pipe", ".flac")
o =
  output.external(
    fallible=true,
    %flac,
    "cat > #{process.quote(piped)}",
    source()
  )
o.on_stop(
  synchronous=true,
  fun () ->
    thread.run(
      delay=0.5,
      {
        let codecs = probe("stream=codec_name", piped)
        let tags = probe("format_tags=title,encoder", piped)
        print(
          "pipe: #{string.quote(codecs)} #{string.quote(tags)}"
        )
        done(
          "pipe",
          codecs == "flac"
          and not string.contains(substring="foo", tags)
          and string.contains(substring="Liquidsoap", tags)
        )
      }
    )
)
