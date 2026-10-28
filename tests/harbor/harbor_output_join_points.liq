%include "../test.liq"

# A late joiner starts at a point a decoder can start from. With an Ogg link
# change every second, its bytes, parsed directly, are whole consecutive pages
# from the first byte, the first page begins a logical stream, every page
# belongs to the stream begun last and at least two changes are seen; the same
# holds after a restart. A single-link stream with a short burst starts with
# the codec's identification header. An mp3 joiner gets no ID3v2 tag and no
# Xing frame.
port = 18116

def chained(mount, encoder) =
  s = insert_metadata(sine())
  thread.run(
    every=1.,
    fun () -> s.insert_metadata([("title", "#{random.int(max=1000000)}")])
  )
  output.harbor(port=port, mount=mount, encoder, s)
end

vorbis = chained("vorbis.ogg", %ogg(%vorbis))
opus = chained("opus.ogg", %ogg(%opus))
flac = chained("flac.ogg", %ogg(%flac))
single =
  output.harbor(
    port=port,
    mount="single.ogg",
    burst=4096,
    %ogg(%vorbis),
    sine()
  )
mp3 =
  output.harbor(
    port=port,
    mount="ffmpeg.mp3",
    %ffmpeg(format = "mp3", %audio(codec = "libmp3lame")),
    sine()
  )

def receive(mount, duration) =
  sock = socket.unix()
  sock.connect(
    socket.address.internet_address(socket.internet_address("127.0.0.1"), port)
  )
  sock.write(
    "GET /#{mount} HTTP/1.0\r\n\r\n"
  )
  start = time()
  chunks = ref([])
  def rec read() =
    if
      time() - start < duration
    then
      chunk =
        try
          sock.read(timeout=5.)
        catch _ do
          ""
        end
      if
        chunk == ""
      then
        test.fail(
          "#{mount} closed"
        )
      end
      chunks := chunk::chunks()
      read()
    end
  end
  read()
  sock.close()
  d = string.concat(list.rev(chunks()))
  header_end = string.index(substring="\r\n\r\n", d)
  string.sub(
    encoding="ascii",
    d,
    start=header_end + 4,
    length=string.length(encoding="ascii", d) - header_end - 4
  )
end

def le32(s, offset) =
  string.binary.to_int(
    little_endian=true,
    string.sub(encoding="ascii", s, start=offset, length=4)
  )
end

# Checks the pages of an Ogg body and returns the number of stream changes
# after the first page.
def pages(mount, body) =
  length = string.length(encoding="ascii", body)
  def rec page(offset, serial, changes) =
    if
      offset + 27 > length
    then
      changes
    else
      if
        string.sub(encoding="ascii", body, start=offset, length=4) != "OggS"
      then
        test.fail(
          "#{mount}: no page at #{offset}"
        )
      end
      segments = string.nth(body, offset + 26)
      def rec size(i, acc) =
        if
          i == segments
        then
          acc
        else
          size(i + 1, acc + string.nth(body, offset + 27 + i))
        end
      end
      next = offset + 27 + segments + size(0, 0)
      bos = string.nth(body, offset + 5) mod 4 >= 2
      page_serial = le32(body, offset + 14)
      if
        offset == 0 and not bos
      then
        test.fail(
          "#{mount}: first page does not begin a stream"
        )
      end
      if
        not bos and page_serial != serial
      then
        test.fail(
          "#{mount}: page at #{offset} from another stream"
        )
      end
      page(
        next,
        bos ? page_serial : serial,
        bos and offset > 0 and page_serial != serial ? changes + 1 : changes
      )
    end
  end
  page(0, -1, 0)
end

def joins(mount) =
  changes = pages(mount, receive(mount, 3.))
  if
    changes < 2
  then
    test.fail(
      "#{mount}: #{changes} stream changes"
    )
  end
end

def check() =
  joins("vorbis.ogg")
  joins("opus.ogg")
  joins("flac.ogg")

  vorbis.stop()
  vorbis.start()
  sleep(3.)
  joins("vorbis.ogg")

  body = receive("single.ogg", 1.)
  ignore(pages("single.ogg", body))
  test.equal(
    string.sub(encoding="ascii", body, start=28, length=7),
    "\001vorbis"
  )

  body = receive("ffmpeg.mp3", 2.)
  test.equal(string.contains(encoding="ascii", substring="ID3", body), false)
  test.equal(string.contains(encoding="ascii", substring="Xing", body), false)
  test.equal(string.contains(encoding="ascii", substring="Info", body), false)
  test.pass()
end

thread.run(delay=8., check)
