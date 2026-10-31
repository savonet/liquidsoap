%include "../test.liq"

# A preroll from the admission decision is sent after the response head and
# before the stream, in both encoder modes. With ICY, its bytes count towards
# the metadata interval and the blocks inside it are empty. The listener joins
# the stream once its preroll is sent, so a preroll much larger than `buffer`
# read by a stalled client does not overflow.
port = 18143
metaint = 1000
preroll = string.make(char_code=80, 2500)
large_preroll =
  list.fold(
    fun (s, _) -> s ^ s,
    string.make(char_code=81, 1024),
    list.init(13, fun (_) -> ())
  )

s = insert_metadata(sine())
thread.run(delay=1., every=1., fun () -> s.insert_metadata([("title", "Live")]))

def accept(l) =
  ua = list.assoc(default="", "user-agent", l.headers)
  if ua == "large" then {preroll = large_preroll} else {preroll = preroll} end
end

output.harbor(
  port=port,
  mount="shared.mp3",
  metaint=metaint,
  buffer=20000,
  burst=10000,
  accept=accept,
  %mp3,
  s
)
output.harbor(
  port=port,
  mount="dedicated.mp3",
  metaint=metaint,
  dedicated_encoder=true,
  buffer=20000,
  accept=accept,
  %mp3,
  s
)

# Body bytes of a request, once at least `length` arrived or the connection
# closed, after an optional stall before the first read.
def body(~agent="test", ~icy=false, ~stall=0., ~length, mount) =
  sock = socket.unix()
  data = ref("")
  try
    sock.connect(
      socket.address.internet_address(
        socket.internet_address("127.0.0.1"),
        port
      )
    )
    sock.write(
      "GET /#{mount} HTTP/1.0\r\nUser-Agent: #{agent}\r\n#{
        icy
          ? "Icy-MetaData: 1\r\n"
          : ""
      }\r\n"
    )
    if stall > 0. then thread.pause(stall) end
    deadline = time() + 30.
    def rec read() =
      chunk =
        try
          sock.read(timeout=5.)
        catch _ do
          ""
        end
      data := data() ^ chunk
      if
        chunk != ""
        and time() < deadline
        and string.length(encoding="ascii", data()) < length
      then
        read()
      end
    end
    read()
  finally
    sock.close()
  end
  index = string.index(substring="\r\n\r\n", data())
  if
    index < 0
  then
    test.fail(
      "#{mount}: no response head"
    )
  end
  string.sub(
    encoding="ascii",
    data(),
    start=index + 4,
    length=string.length(encoding="ascii", data()) - index - 4
  )
end

def sub(s, start, length) =
  string.sub(encoding="ascii", s, start=start, length=length)
end

def check_plain(mount) =
  data = body(length=10000, mount)
  if
    sub(data, 0, 2500) != preroll
  then
    test.fail(
      "#{mount}: body does not start with the preroll"
    )
  end
  if
    string.length(encoding="ascii", data) < 5000
  then
    test.fail(
      "#{mount}: no stream after the preroll"
    )
  end
end

# Splits an ICY body into its stream bytes and its blocks.
def deinterleave(mount, data) =
  total = string.length(encoding="ascii", data)
  def rec go(offset, stream, blocks) =
    if
      offset + metaint >= total
    then
      (string.concat(list.rev(stream)), list.rev(blocks))
    else
      stream = sub(data, offset, metaint)::stream
      size = string.nth(data, offset + metaint) * 16
      if
        offset + metaint + 1 + size > total
      then
        (string.concat(list.rev(stream)), list.rev(blocks))
      else
        go(
          offset + metaint + 1 + size,
          stream,
          sub(data, offset + metaint + 1, size)::blocks
        )
      end
    end
  end
  ignore(mount)
  go(0, [], [])
end

def check_icy(mount) =
  let (stream, blocks) =
    deinterleave(mount, body(icy=true, length=60000, mount))
  if
    sub(stream, 0, 2500) != preroll
  then
    test.fail(
      "#{mount}: ICY stream does not start with the preroll"
    )
  end
  if
    list.nth(default="?", blocks, 0) != ""
    or list.nth(default="?", blocks, 1) != ""
  then
    test.fail(
      "#{mount}: non-empty block inside the preroll"
    )
  end
  if
    not list.exists(
      fun (block) -> string.contains(encoding="ascii", substring="Live", block),
      blocks
    )
  then
    test.fail(
      "#{mount}: no title after the preroll"
    )
  end
end

def check_large(mount) =
  size = string.length(encoding="ascii", large_preroll)
  data = body(agent="large", stall=3., length=size + 20000, mount)
  if
    sub(data, 0, size) != large_preroll
  then
    test.fail(
      "#{mount}: large preroll truncated: #{
        string.length(encoding="ascii", data)
      } bytes"
    )
  end
  if
    string.length(encoding="ascii", data) < size + 20000
  then
    test.fail(
      "#{mount}: listener disconnected after a large preroll: #{
        string.length(encoding="ascii", data) - size
      } stream bytes"
    )
  end
end

def check() =
  check_plain("shared.mp3")
  check_plain("dedicated.mp3")
  check_icy("shared.mp3")
  check_icy("dedicated.mp3")
  check_large("shared.mp3")
  check_large("dedicated.mp3")
  test.pass()
end

thread.run(delay=3., check)
