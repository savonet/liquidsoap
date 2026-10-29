%include "../test.liq"

# Over TLS and SSL, a listener that stops reading delays nobody: with one
# scheduler domain, hence one delivery group, a reading listener keeps
# receiving at the stream's rate while the stalled one's socket is full.
settings.scheduler.domains := 1
byte_rate = 192000 * 4

tls = http.transport.tls(certificate="./ssl.cert", key="./ssl.key")
ssl = http.transport.ssl(certificate="./ssl.cert", key="./ssl.key")

def serve(port, transport) =
  output.harbor(
    port=port,
    transport=transport,
    mount="slow.wav",
    buffer=100000000,
    timeout=60.,
    burst=null,
    %wav(samplerate = 192000, stereo),
    sine()
  )
end

outputs = [(18122, serve(18122, tls)), (18123, serve(18123, ssl))]

# `openssl s_client` reads the stream as fast as its output is consumed: into
# /dev/null, or not at all once the pipe to a sleeping process is full.
def client(port, agent, sink) =
  thread.run(
    fun () ->
      ignore(
        process.run(
          timeout=60.,
          "printf 'GET /slow.wav HTTP/1.0\\r\\nUser-Agent: #{
            agent
          }\\r\\n\\r\\n' | openssl s_client -quiet -connect 127.0.0.1:#{port} \
           2>/dev/null #{sink}"
        )
      )
  )
end

def sent(o, stalled) =
  list.fold(
    fun (total, l) ->
      if
        (list.assoc(default="", "user-agent", l.headers) == "stalled") ==
          stalled
      then
        total + l.bytes_sent
      else
        total
      end,
    0,
    o.listeners()
  )
end

def check() =
  list.iter(
    fun (p) ->
      begin
        let (port, o) = p
        test.equal(list.length(o.listeners()), 2)
        stalled_before = sent(o, true)
        reading_before = sent(o, false)
        thread.pause(4.)
        stalled_gain = sent(o, true) - stalled_before
        reading_gain = sent(o, false) - reading_before
        print(
          "port #{port}: reading +#{reading_gain}, stalled +#{stalled_gain}"
        )
        if
          stalled_gain > byte_rate
        then
          test.fail(
            "port #{port}: stalled listener still reading"
          )
        end
        if
          reading_gain < 3 * byte_rate
        then
          test.fail(
            "port #{port}: reading listener got #{reading_gain}"
          )
        end
      end,
    outputs
  )
  test.pass()
end

def start() =
  list.iter(
    fun (p) ->
      begin
        let (port, _) = p
        client(
          port,
          "stalled",
          "| sleep 60"
        )
        client(
          port,
          "reading",
          "> /dev/null"
        )
      end,
    outputs
  )
  thread.run(delay=15., check)
end

thread.run(delay=1., start)
