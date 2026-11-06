%include "../test.liq"

# An audio and video Matroska mount reaches its listeners with the default muxer
# options.
def f() =
  port = 18220
  server = icecast.server({port = port})
  let {audio} = source.tracks(sine())
  let {video} = source.tracks(video.testsrc.ffmpeg())

  output.icecast(
    %ffmpeg(
      format = "matroska",
      %audio(codec = "aac"),
      %video(codec = "mpeg4")
    ),
    host="localhost",
    port=port,
    password="hackme",
    mount="tv.mkv",
    format="video/x-matroska",
    send_icy_metadata=false,
    mksafe(source({audio = audio, video = video}))
  )

  def listen() =
    sock = socket.unix()
    try
      sock.connect(
        socket.address.internet_address(
          socket.internet_address("127.0.0.1"),
          port
        )
      )
      sock.write(
        "GET /tv.mkv HTTP/1.0\r\n\r\n"
      )
      data = ref("")
      deadline = time() + 30.
      while string.length(encoding="ascii", data()) < 100000 do
        if
          time() > deadline
        then
          test.fail(
            "only #{string.length(encoding="ascii", data())} bytes received"
          )
        end
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
            "connection closed early"
          )
        end
        data := data() ^ chunk

      end
      data()
    finally
      sock.close()
    end
  end

  def run() =
    d = listen()
    header_end = string.index(substring="\r\n\r\n", d)
    body = string.sub(encoding="ascii", d, start=header_end + 4, length=4)
    if
      body != "\x1a\x45\xdf\xa3"
    then
      test.fail(
        "the stream does not start with a Matroska header"
      )
    end
    test.pass()
  end

  server.on_source_connect(fun (_) -> thread.run(delay=1., run))
end

test.check(f)
