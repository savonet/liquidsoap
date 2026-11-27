%include "../test.liq"

# On a file, %mp3 and %shine write an ID3v2 tag by default and an
# encoder-information frame with the final frame count, delay and padding: the
# file decodes to its input's length. A process gets neither.

pending = ref(3)

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
  s =
    metadata.map(
      insert_missing=true,
      fun (_) -> [("title", "foo")],
      once(sine(duration=0.5))
    )
  clock.assign_new(sync="none", [s])
  s
end

def on_close(name, filename) =
  contents = file.contents(filename)

  # The frame count follows the identifier and its four bytes of flags.
  info = string.index(substring="Info", contents)
  frames =
    if
      info < 0
    then
      0
    else
      string.binary.to_int(
        little_endian=false,
        string.sub(encoding="ascii", contents, start=info + 8, length=4)
      )
    end
  decoded =
    int_of_string(
      default=0,
      string.trim(
        process.read(
          "ffmpeg -v quiet -i #{process.quote(filename)} -f s16le - | wc -c"
        )
      )
    )
  print(
    "#{name}: #{frames} frames, #{decoded} bytes decoded"
  )
  done(
    name,
    decoded == 88200
    and string.sub(encoding="ascii", contents, start=0, length=3) == "ID3"
    and frames > 10
  )
end

def check_file(name, format) =
  output.file(
    fallible=true,
    on_close=fun (filename) -> on_close(name, filename),
    format,
    file.temp(cleanup=true, "mp3_file_form_#{name}", ".mp3"),
    source()
  )
end

check_file("lame", %mp3)
%ifencoder %shine
check_file("shine", %shine)
%else
pending := pending() - 1
%endif

piped = file.temp(cleanup=true, "mp3_file_form_pipe", ".mp3")
o =
  output.external(
    fallible=true,
    %mp3,
    "cat > #{process.quote(piped)}",
    source()
  )
o.on_stop(
  synchronous=true,
  fun () ->
    thread.run(
      delay=0.5,
      {
        let contents = file.contents(piped)
        done(
          "pipe",
          string.length(contents) > 1000
          and string.sub(encoding="ascii", contents, start=0, length=3) != "ID3"
          and not string.contains(encoding="ascii", substring="Info", contents)
        )
      }
    )
)
