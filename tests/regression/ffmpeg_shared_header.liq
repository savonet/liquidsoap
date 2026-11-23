%include "../test.liq"

# A reader starting mid-stream on a shared harbor mount gets the container
# header first. With a codec that has a delay (AAC, LAME), the header used to
# leave with the first bytes and never be captured, so late readers got
# Matroska clusters with no EBML header.

port = 18461
s = sine()

def mount(name, codec) =
  output.harbor(
    port=port,
    mount=name,
    %ffmpeg(format = "matroska", %audio(codec = codec)),
    s
  )
end

mount("aac.mkv", "aac")
mount("mp2.mkv", "mp2")

pending = ref(2)

def check(name) =
  path = file.temp(cleanup=true, "ffmpeg_shared_header", ".mkv")
  ignore(
    process.run(
      "curl -s -m 3 -o #{process.quote(path)} http://127.0.0.1:#{port}/#{name}"
    )
  )

  magic = string.sub(file.contents(path), encoding="ascii", start=0, length=4)

  print(
    "#{name}: #{string.quote(magic)}"
  )
  if
    magic == "\x1a\x45\xdf\xa3"
  then
    pending := pending() - 1
    if pending() == 0 then test.pass() end
  else
    test.fail()
  end
end

thread.run(delay=2., {check("aac.mkv")})
thread.run(delay=2., {check("mp2.mkv")})
