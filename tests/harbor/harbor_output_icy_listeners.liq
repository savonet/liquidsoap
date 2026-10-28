%include "../test.liq"

# Two ICY listeners joining at different times, asking with a mixed-case
# header, each get every title change once and empty blocks in between. A
# 4000-byte title with a 300-byte url fills a block of 255 and the stream goes
# on. An Ogg mount asked for ICY answers without it.
port = 18114
metaint = 2000
s = insert_metadata(sine())
o =
  output.harbor(
    port=port,
    mount="icy.mp3",
    metaint=metaint,
    burst=null,
    %mp3,
    s
  )
long = insert_metadata(sine())
long_output =
  output.harbor(
    port=port,
    mount="long.mp3",
    metaint=metaint,
    url=string.make(char_code=117, 300),
    burst=null,
    %mp3,
    long
  )
ogg = output.harbor(port=port, mount="icy.ogg", %ogg(%vorbis), sine())

titles = ref(0)
thread.run(
  every=1.,
  fun () ->
    begin
      titles := titles() + 1
      s.insert_metadata(
        [
          (
            "title",
            "title #{titles()}"
          )
        ]
      )
      long.insert_metadata([("title", string.make(char_code=97, 4000))])
    end
)

def connect(mount) =
  sock = socket.unix()
  sock.connect(
    socket.address.internet_address(socket.internet_address("127.0.0.1"), port)
  )
  sock.write(
    "GET /#{mount} HTTP/1.0\r\niCy-MeTaDaTa: 1\r\n\r\n"
  )
  sock
end

# Head and body of what a listener receives in `duration` seconds.
def receive(mount, duration) =
  sock = connect(mount)
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
  (
    string.case(
      lower=true,
      string.sub(encoding="ascii", d, start=0, length=header_end)
    ),
    string.sub(
      encoding="ascii",
      d,
      start=header_end + 4,
      length=string.length(encoding="ascii", d) - header_end - 4
    )
  )
end

# The length byte and text of every block, in order.
def blocks(body) =
  length = string.length(encoding="ascii", body)
  def rec next(position, acc) =
    position = position + metaint
    if
      position >= length
    then
      list.rev(acc)
    else
      n = string.nth(body, position)
      text =
        string.sub(encoding="ascii", body, start=position + 1, length=16 * n)
      next(position + 1 + 16 * n, (n, text)::acc)
    end
  end
  next(0, [])
end

def changes(mount, duration) =
  let (head, body) = receive(mount, duration)
  test.equal(
    string.contains(
      encoding="ascii",
      substring="icy-metaint: #{metaint}",
      head
    ),
    true
  )
  b = blocks(body)
  titles = list.filter(fun (t) -> t != "", list.map(snd, b))
  if
    list.length(b) - list.length(titles) == 0
  then
    test.fail(
      "#{mount}: no empty block"
    )
  end
  def rec distinct(l) =
    if
      list.length(l) >= 2
    then
      if
        list.nth(l, 0) == list.nth(l, 1)
      then
        test.fail(
          "#{mount}: unchanged title sent twice"
        )
      end
      distinct(list.tl(l))
    end
  end
  distinct(titles)
  list.iter(
    fun (t) ->
      if
        not string.contains(
          encoding="ascii",
          substring="StreamTitle='title ",
          t
        )
      then
        test.fail(
          "#{mount}: bad block #{t}"
        )
      end,
    titles
  )
  titles
end

def check() =
  first = ref([])
  thread.run(fun () -> first := changes("icy.mp3", 5.))
  sleep(2.)
  late = changes("icy.mp3", 4.)
  if
    list.length(late) < 3
  then
    test.fail(
      "late listener got #{late}"
    )
  end
  def rec wait() =
    if
      first() == []
    then
      sleep(0.2)
      wait()
    end
  end
  wait()
  if
    list.length(first()) < 4
  then
    test.fail(
      "first listener got #{first()}"
    )
  end

  let (_, body) = receive("long.mp3", 3.)
  b = blocks(body)
  if
    not list.exists(fun (block) -> fst(block) == 255, b)
  then
    test.fail(
      "no 255 block in #{list.map(fst, b)}"
    )
  end
  if
    list.length(b) < 10
  then
    test.fail(
      "stream stopped after a long block"
    )
  end

  let (head, body) = receive("icy.ogg", 1.)
  test.equal(
    string.contains(encoding="ascii", substring="icy-metaint", head),
    false
  )
  test.equal(string.sub(encoding="ascii", body, start=0, length=4), "OggS")
  test.pass()
end

o.on_start(synchronous=true, fun () -> thread.run(delay=1.5, check))
