%include "../test.liq"

# A listener several seconds behind live, by its burst, receives each title
# change at the stream position where it took effect: titles come in order,
# the first one is older than the newest at join time, and two changes made
# two seconds apart are two seconds of stream apart.
port = 18115
metaint = 1000
byte_rate = 16000
s = insert_metadata(sine())
o =
  output.harbor(
    port=port,
    mount="stamp.mp3",
    metaint=metaint,
    burst=6 * byte_rate,
    %mp3(bitrate = 128),
    s
  )

current = ref(0)
thread.run(
  every=2.,
  fun () ->
    begin
      current := current() + 1
      s.insert_metadata([("title", "#{current()}")])
    end
)

def check() =
  newest = current()
  sock = socket.unix()
  sock.connect(
    socket.address.internet_address(socket.internet_address("127.0.0.1"), port)
  )
  sock.write(
    "GET /stamp.mp3 HTTP/1.0\r\nIcy-MetaData: 1\r\n\r\n"
  )
  chunks = ref([])
  start = time()
  def rec read() =
    if
      time() - start < 4.
    then
      chunk =
        try
          sock.read(timeout=5.)
        catch _ do
          ""
        end
      if chunk == "" then test.fail("closed") end
      chunks := chunk::chunks()
      read()
    end
  end
  read()
  sock.close()
  d = string.concat(list.rev(chunks()))
  header_end = string.index(substring="\r\n\r\n", d)
  body =
    string.sub(
      encoding="ascii",
      d,
      start=header_end + 4,
      length=string.length(encoding="ascii", d) - header_end - 4
    )
  length = string.length(encoding="ascii", body)

  # (stream offset, title) of every non-empty block.
  def rec changes(position, stream, acc) =
    position = position + metaint
    stream = stream + metaint
    if
      position >= length
    then
      list.rev(acc)
    else
      n = string.nth(body, position)
      text =
        string.sub(encoding="ascii", body, start=position + 1, length=16 * n)
      acc =
        if
          n == 0
        then
          acc
        else
          close = string.index(substring="';", text)
          title =
            string.sub(encoding="ascii", text, start=13, length=close - 13)
          (stream, int_of_string(title))::acc
        end
      changes(position + 1 + 16 * n, stream, acc)
    end
  end
  c = changes(0, 0, [])
  if
    list.length(c) < 4
  then
    test.fail(
      "only #{c}"
    )
  end
  let (_, first) = list.hd(c)
  if
    first >= newest
  then
    test.fail(
      "first title #{first} is not older than #{newest}"
    )
  end
  def rec spacing(l) =
    if
      list.length(l) >= 3
    then
      let [(a, ta), (b, tb), ...rest] = l
      ignore(rest)
      test.equal(tb, ta + 1)
      gap = b - a
      if
        gap < 2 * byte_rate - 4000 or gap > 2 * byte_rate + 4000
      then
        test.fail(
          "change #{tb} came #{gap} bytes after #{ta}"
        )
      end
      spacing(list.tl(l))
    end
  end
  spacing(list.tl(c))
  test.pass()
end

o.on_start(synchronous=true, fun () -> thread.run(delay=9., check))
