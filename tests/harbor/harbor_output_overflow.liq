%include "../test.liq"

# A throttled ICY client slower than the stream is disconnected once it is more
# than `buffer` behind, never trimmed: every byte it got up to then has a
# block exactly every `metaint` stream bytes. A client that never reads is
# closed within its timeout plus the retry interval once its socket is full.
port = 18110
metaint = 1000
s = insert_metadata(sine())
o =
  output.harbor(
    port=port,
    mount="slow.wav",
    metaint=metaint,
    buffer=1000000,
    burst=null,
    %wav(samplerate = 192000, stereo),
    s
  )

stalled =
  output.harbor(
    port=port,
    mount="stalled.wav",
    buffer=100000000,
    timeout=2.,
    burst=null,
    %wav(samplerate = 192000, stereo),
    sine()
  )

disconnected = ref([])
o.on_disconnect(
  synchronous=true,
  fun (l) -> atomic(fun () -> disconnected := [...disconnected(), l.id])
)
stalled_closed = ref(0.)
stalled.on_disconnect(synchronous=true, fun (_) -> stalled_closed := time())

thread.run(
  every=0.3,
  fun () ->
    s.insert_metadata(
      [
        (
          "title",
          "title #{random.int(max=100000)}"
        )
      ]
    )
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

# Checks the ICY framing of a body and returns the number of blocks checked.
def framing(body) =
  length = string.length(encoding="ascii", body)
  def rec check(position, blocks) =
    if
      position + metaint >= length
    then
      blocks
    else
      position = position + metaint
      n = string.nth(body, position)
      size = 16 * n
      if
        n > 0 and position + 1 + size <= length
      then
        text = string.sub(encoding="ascii", body, start=position + 1, length=13)
        if
          text != "StreamTitle='"
        then
          test.fail(
            "block at #{position} reads #{text}"
          )
        end
      end
      check(position + 1 + size, blocks + 1)
    end
  end
  check(0, 0)
end

def throttled() =
  sock =
    connect(
      "slow.wav",
      "Icy-MetaData: 1\r\n"
    )
  chunks = ref([])
  def rec read() =
    chunk =
      try
        sock.read(timeout=5.)
      catch _ do
        ""
      end
    if
      chunk != ""
    then
      chunks := chunk::chunks()
      if disconnected() == [] then thread.pause(0.05) end
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
  blocks = framing(body)
  if
    blocks < 100
  then
    test.fail(
      "only #{blocks} blocks checked"
    )
  end
  test.equal(list.length(disconnected()), 1)
end

def check() =
  sock = connect("stalled.wav", "")
  ignore(sock)
  opened = time()
  throttled()
  def rec wait() =
    if
      stalled_closed() == 0.
    then
      if
        time() - opened > 60.
      then
        test.fail(
          "stalled client not closed"
        )
      end
      thread.pause(0.2)
      wait()
    end
  end
  wait()
  sock.close()
  if
    stalled_closed() - opened > 30.
  then
    test.fail(
      "stalled client closed late"
    )
  end
  test.pass()
end

o.on_start(synchronous=true, fun () -> thread.run(delay=1., check))
