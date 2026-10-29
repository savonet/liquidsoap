%include "../test.liq"

# Dedicated encoders: two listeners admitted at once both get `on_connect` and
# each a stream starting from its own encoder's first byte; new titles reach
# them in band; an ICY listener gets its blocks; at stop each listener gets
# its encoder's end-of-stream page and one disconnect. A WAV listener gets its
# encoder's header once.
port = 18121
metaint = 2000
s = insert_metadata(sine())
title = ref(0)
thread.run(
  every=1.,
  fun () ->
    begin
      atomic(fun () -> title := title() + 1)
      s.insert_metadata(
        [
          (
            "title",
            "dedicated #{title()}"
          )
        ]
      )
    end
)

o =
  output.harbor(
    port=port,
    mount="dedicated.ogg",
    dedicated_encoder=true,
    %ogg(%vorbis),
    s
  )
icy =
  output.harbor(
    port=port,
    mount="dedicated.mp3",
    dedicated_encoder=true,
    metaint=metaint,
    %mp3,
    s
  )
wav =
  output.harbor(
    port=port,
    mount="dedicated.wav",
    dedicated_encoder=true,
    %wav,
    s
  )

connects = ref(0)
disconnects = ref(0)
o.on_connect(
  synchronous=true,
  fun (_) -> atomic(fun () -> connects := connects() + 1)
)
o.on_disconnect(
  synchronous=true,
  fun (_) -> atomic(fun () -> disconnects := disconnects() + 1)
)

def connect(mount, headers) =
  sock = socket.unix()
  sock.connect(
    socket.address.internet_address(socket.internet_address("127.0.0.1"), port)
  )
  sock.write(
    "GET /#{mount} HTTP/1.0\r\n#{headers}\r\n"
  )
  sock
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

# Reads until the server closes the connection.
def read_all(sock) =
  chunks = ref([])
  def rec read() =
    chunk =
      try
        sock.read(timeout=10.)
      catch _ do
        ""
      end
    if
      chunk != ""
    then
      chunks := chunk::chunks()
      read()
    end
  end
  read()
  sock.close()
  string.concat(list.rev(chunks()))
end

def last_page_type(body) =
  length = string.length(encoding="ascii", body)
  def rec page(offset, last) =
    if
      offset + 27 > length
    then
      last
    else
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
      page(offset + 27 + segments + size(0, 0), string.nth(body, offset + 5))
    end
  end
  page(0, 0)
end

def check_ogg(body) =
  test.equal(string.sub(encoding="ascii", body, start=0, length=4), "OggS")
  test.equal(
    string.sub(encoding="ascii", body, start=28, length=7),
    "\001vorbis"
  )
  titles =
    list.filter(
      fun (n) ->
        string.contains(
          encoding="ascii",
          substring="title=dedicated #{n}",
          body
        ),
      list.init(title() + 1, fun (n) -> n)
    )
  if
    list.length(titles) < 2
  then
    test.fail(
      "titles in band: #{titles}"
    )
  end
  test.equal(last_page_type(body) mod 8 >= 4, true)
end

def check() =
  first = ref(null)
  second = ref(null)
  def reader(result) =
    thread.run(
      fun () ->
        result := (body_of(read_all(connect("dedicated.ogg", ""))) : string?)
    )
  end
  reader(first)
  reader(second)

  icy_sock =
    connect(
      "dedicated.mp3",
      "Icy-MetaData: 1\r\n"
    )
  icy_chunks = ref([])
  start = time()
  def rec read_icy() =
    if
      time() - start < 4.
    then
      chunk =
        try
          icy_sock.read(timeout=5.)
        catch _ do
          ""
        end
      if
        chunk == ""
      then
        test.fail(
          "icy listener closed"
        )
      end
      icy_chunks := chunk::icy_chunks()
      read_icy()
    end
  end
  read_icy()
  icy_sock.close()
  icy_body = body_of(string.concat(list.rev(icy_chunks())))
  def rec blocks(position, acc) =
    position = position + metaint
    if
      position >= string.length(encoding="ascii", icy_body)
    then
      acc
    else
      n = string.nth(icy_body, position)
      whole = position + 1 + 16 * n <= string.length(encoding="ascii", icy_body)
      text =
        string.sub(
          encoding="ascii",
          icy_body,
          start=position + 1,
          length=16 * n
        )
      blocks(position + 1 + 16 * n, n > 0 and whole ? [...acc, text] : acc)
    end
  end
  titled = blocks(0, [])
  if
    list.length(titled) < 2
  then
    test.fail(
      "icy blocks: #{titled}"
    )
  end
  list.iter(
    fun (t) ->
      if
        not string.contains(
          encoding="ascii",
          substring="StreamTitle='dedicated ",
          t
        )
      then
        test.fail(
          "bad block #{t}"
        )
      end,
    titled
  )

  wav_sock = connect("dedicated.wav", "")
  wav_data = ref("")
  while string.length(encoding="ascii", wav_data()) < 1000 do
    wav_data := wav_data() ^ wav_sock.read(timeout=5.)

  end
  wav_sock.close()
  wav_body = body_of(wav_data())
  test.equal(string.sub(encoding="ascii", wav_body, start=0, length=4), "RIFF")
  test.equal(
    string.sub(encoding="ascii", wav_body, start=44, length=4) == "RIFF",
    false
  )

  test.equal(connects(), 2)
  o.stop()
  def rec wait(n) =
    if
      not null.defined(first()) or not null.defined(second())
    then
      if
        n == 0
      then
        test.fail(
          "listeners not closed at stop"
        )
      end
      thread.pause(0.2)
      wait(n - 1)
    end
  end
  wait(100)
  check_ogg(null.get(first()))
  check_ogg(null.get(second()))
  thread.pause(1.)
  test.equal(disconnects(), 2)
  test.pass()
end

o.on_start(
  synchronous=true,
  fun () -> if connects() == 0 then thread.run(delay=1., check) end
)
