# Shared by the SRT checks. Ports 19500 to 19599 belong to them.
srt_peer = "./srt_peer.exe"

def wait_until(~timeout=15., ~what, f) =
  deadline = time() + timeout
  def rec loop() =
    if
      f()
    then
      ()
    elsif
      time() > deadline
    then
      test.fail("timed out waiting for #{what}")
    else
      thread.pause(0.05)
      loop()
    end
  end
  loop()
end

def run_peer(args) =
  r = process.run(timeout=60., "#{srt_peer} #{args}")
  if
    r.status != "exit" and not test.done()
  then
    test.fail(
      "peer #{args} did not exit: #{r.status} #{r.status.description} \
       #{r.stderr}"
    )
  end
  r.status.code
end

def lines_of(text) =
  list.filter(fun (l) -> l != "", string.split(separator="\n", text))
end

def lines(file_name) =
  list.filter(
    fun (l) -> l != "", string.split(separator="\n", file.contents(file_name))
  )
end

def payload_sizes(file_name) =
  list.map(int_of_string, lines(file_name))
end

# Offset of the last Ogg page in data, or -1.
def last_ogg_page(data) =
  def rec find(i) =
    if
      i < 0
    then
      -1
    elsif
      string.sub(encoding="ascii", data, start=i, length=4) == "OggS"
    then
      i
    else
      find(i - 1)
    end
  end
  find(string.length(encoding="ascii", data) - 4)
end

def ends_with_ogg_eos(data) =
  i = last_ogg_page(data)
  i >= 0 and (string.nth(data, i + 5) / 4) mod 2 == 1
end

# Codec name and packet count ffprobe reads from a received stream.
def probe(file_name) =
  r =
    process.run(
      timeout=30.,
      "ffprobe -v error -count_packets -show_entries \
       stream=codec_name,nb_read_packets -of csv=p=0 #{process.quote(file_name)}"
    )
  first = list.hd(default="", lines_of(r.stdout))
  let [codec, packets] =
    list.append(string.split(separator=",", first), ["", "0"])
  (codec, int_of_string(default=0, packets))
end

def check_decodes(~what, ~codec, file_name) =
  let (found, packets) = probe(file_name)
  if
    found != codec or packets < 10
  then
    test.fail(
      "#{what}: expected #{codec} with packets, got #{found} with #{packets}"
    )
  end
end

def ogg_stream_starts(data) =
  def rec count(i, n) =
    if
      i < 0
    then
      n
    elsif
      string.sub(encoding="ascii", data, start=i, length=4) == "OggS"
      and (string.nth(data, i + 5) / 2) mod 2 == 1
    then
      count(i - 1, n + 1)
    else
      count(i - 1, n)
    end
  end
  count(string.length(encoding="ascii", data) - 4, 0)
end

def file_size(file_name) =
  string.length(encoding="ascii", file.contents(file_name))
end
