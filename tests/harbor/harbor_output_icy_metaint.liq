# A raw ICY client checks that metadata blocks land exactly every metaint
# bytes of stream data: a full block at the first boundary, an empty one at
# the next since the title did not change.
def f() =
  s =
    metadata.map(
      insert_missing=true,
      fun (_) ->
        [
          (
            "title",
            "icy test"
          )
        ],
      sine()
    )

  output.harbor(%mp3, mount="test", port=3474, metaint=1024, s)

  def check() =
    sock = socket.unix()
    sock.connect(
      socket.address.internet_address(
        socket.internet_address("127.0.0.1"),
        3474
      )
    )
    sock.write(
      "GET /test HTTP/1.0\r\nIcy-MetaData: 1\r\n\r\n"
    )
    data = ref("")
    while string.length(data()) < 4000 do
      chunk = sock.read(timeout=5.)
      if
        chunk == ""
      then
        test.fail(
          "connection closed early"
        )
      end
      data := data() ^ chunk

    end
    sock.close()
    d = data()
    test.equal(
      string.contains(
        encoding="ascii",
        substring="icy-metaint: 1024\r\n",
        d
      ),
      true
    )
    header_end = string.index(substring="\r\n\r\n", d)
    body =
      string.sub(
        d,
        encoding="ascii",
        start=header_end + 4,
        length=string.length(d) - header_end - 4
      )

    # "StreamTitle='icy test';" is 23 bytes, padded to 2 blocks of 16.
    test.equal(
      string.sub(body, encoding="ascii", start=1024, length=1),
      string.char(2)
    )
    block = string.sub(body, encoding="ascii", start=1025, length=32)
    test.equal(
      string.contains(
        encoding="ascii",
        substring="StreamTitle='icy test';",
        block
      ),
      true
    )

    # 1024 more bytes of stream, then an empty block.
    test.equal(
      string.sub(body, encoding="ascii", start=1024 + 33 + 1024, length=1),
      string.char(0)
    )
    test.pass()
  end

  thread.run(delay=1., check)
end

test.check(f)
