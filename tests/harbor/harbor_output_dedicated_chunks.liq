%include "../test.liq"

# Dedicated FFmpeg encoders keep each listener's stream in reused chunks: a
# Matroska stream whose clusters span many chunks reaches a plain listener and
# an ICY listener intact, and each decodes without error.
port = 18210
metaint = 1000
directory = file.temp_dir(cleanup=true, "dedicated-chunks")

o =
  output.harbor(
    port=port,
    mount="chunks.mkv",
    dedicated_encoder=true,
    metaint=metaint,
    %ffmpeg(format = "matroska", %audio(codec = "pcm_s16le")),
    sine()
  )

def connect(headers) =
  sock = socket.unix()
  sock.connect(
    socket.address.internet_address(socket.internet_address("127.0.0.1"), port)
  )
  sock.write(
    "GET /chunks.mkv HTTP/1.0\r\n#{headers}\r\n"
  )
  sock.{data = ref(""), closed = ref(false)}
end

def read_some(sock) =
  if
    not sock.closed()
  then
    chunk =
      try
        sock.read(timeout=0.2)
      catch _ do
        ""
      end
    if chunk == "" then () else sock.data := sock.data() ^ chunk end
  end
end

def body_of(d) =
  header_end = string.index(substring="\r\n\r\n", d)
  string.sub(
    encoding="ascii",
    d,
    start=header_end + 4,
    length=string.length(encoding="ascii", d) - header_end - 4
  )
end

def length(s) =
  string.length(encoding="ascii", s)
end

# The stream bytes of an ICY body: `metaint` bytes, then a length byte `n` and
# `16 n` bytes of metadata, and so on.
def strip_icy(body) =
  total = length(body)
  def rec go(offset, acc) =
    if
      offset + metaint >= total
    then
      [
        ...acc,
        string.sub(encoding="ascii", body, start=offset, length=total - offset)
      ]
    else
      data = string.sub(encoding="ascii", body, start=offset, length=metaint)
      n = string.nth(body, offset + metaint)
      if
        offset + metaint + 1 + 16 * n > total
      then
        test.fail(
          "ICY block at #{offset + metaint} runs past the body"
        )
      end
      go(offset + metaint + 1 + 16 * n, [...acc, data])
    end
  end
  string.concat(go(0, []))
end

def decodes(label, body) =
  capture = path.concat(directory, "#{label}.mkv")
  file.write(data=body, capture)
  r =
    process.run(
      "ffmpeg -nostdin -v error -xerror -i #{process.quote(capture)} -f null -"
    )
  if
    r.status != "exit" or r.status.code != 0 or r.stderr != ""
  then
    test.fail(
      "#{label} stream does not decode: #{r.status} #{r.status.code} #{
        r.stderr
      }"
    )
  end
end

def run() =
  plain = connect("")
  icy =
    connect(
      "Icy-MetaData: 1\r\n"
    )
  deadline = time() + 60.
  def rec collect() =
    read_some(plain)
    read_some(icy)
    if
      length(plain.data()) < 2000000 or length(icy.data()) < 2000000
    then
      if
        time() > deadline
      then
        test.fail(
          "only #{length(plain.data())} and #{length(icy.data())} bytes received"
        )
      end
      collect()
    end
  end
  collect()
  o.stop()
  def rec drain(sock) =
    chunk =
      try
        sock.read(timeout=10.)
      catch _ do
        ""
      end
    if
      chunk != ""
    then
      sock.data := sock.data() ^ chunk
      drain(sock)
    end
  end
  drain(plain)
  drain(icy)
  plain.close()
  icy.close()
  decodes("plain", body_of(plain.data()))
  decodes("icy", strip_icy(body_of(icy.data())))
  test.pass()
end

thread.run(delay=1., run)
