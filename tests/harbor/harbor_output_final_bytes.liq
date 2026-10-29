%include "../test.liq"

# Stopping an Ogg output delivers its end-of-stream page to a reading
# listener: its bytes are whole Ogg pages and the last one ends the stream.
port = 18112
o =
  output.harbor(port=port, mount="final.ogg", burst=null, %ogg(%vorbis), sine())
stopped = ref(false)
o.on_stop(synchronous=true, fun () -> stopped := true)

# The header type of the last page, after checking every page is whole.
def last_page_type(body) =
  length = string.length(encoding="ascii", body)
  def rec page(offset, last, count) =
    if
      offset == length
    then
      (last, count)
    else
      if
        string.sub(encoding="ascii", body, start=offset, length=4) != "OggS"
      then
        test.fail(
          "no page at #{offset}"
        )
      end
      segments = string.nth(body, offset + 26)
      def rec size(i, acc) =
        i == segments
          ? acc
          : size(i + 1, acc + string.nth(body, offset + 27 + i))
      end
      next = offset + 27 + segments + size(0, 0)
      if
        next > length
      then
        test.fail(
          "truncated page at #{offset}"
        )
      end
      page(next, string.nth(body, offset + 5), count + 1)
    end
  end
  page(0, 0, 0)
end

def check() =
  sock = socket.unix()
  sock.connect(
    socket.address.internet_address(socket.internet_address("127.0.0.1"), port)
  )
  sock.write(
    "GET /final.ogg HTTP/1.0\r\n\r\n"
  )
  thread.run(delay=2., fun () -> o.stop())
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

  # The socket closes during the stop, before on_stop runs.
  def rec wait_stopped(n) =
    if
      not stopped()
    then
      if
        n == 0
      then
        test.fail(
          "on_stop did not run"
        )
      end
      thread.pause(0.1)
      wait_stopped(n - 1)
    end
  end
  wait_stopped(50)
  d = string.concat(list.rev(chunks()))
  header_end = string.index(substring="\r\n\r\n", d)
  body =
    string.sub(
      encoding="ascii",
      d,
      start=header_end + 4,
      length=string.length(encoding="ascii", d) - header_end - 4
    )
  let (last, count) = last_page_type(body)
  if
    count < 3
  then
    test.fail(
      "only #{count} pages"
    )
  end
  if
    last mod 8 < 4
  then
    test.fail(
      "last page does not end the stream (header type #{last})"
    )
  end
  test.pass()
end

o.on_start(synchronous=true, fun () -> thread.run(delay=0.5, check))
