%include "../test.liq"

# input.harbor: `auth` sees the request headers by lowercase name; `accept`
# refuses a source client with its own status, reason and headers, an invalid
# refusal or a raising `accept` answers 403, `null` admits; a source client
# without Content-Type gets 400. A request without credentials reaches `auth`
# and `accept` with empty ones when `accept` is given, and is refused 401
# without calling `auth` otherwise.
port = 18184
checks = ref(0)
seen_headers = ref([])
anonymous_logins = ref([])
plain_auth_calls = ref(0)

def check(label, cond) =
  checks := checks() + 1
  if not cond then test.fail(label) end
end

def auth(request) =
  seen_headers := request.headers
  (request.user == "u" and request.password == "p")
  or list.assoc.mem("x-anonymous", request.headers)
end

def accept(request) =
  def has(name) =
    list.assoc.mem(name, request.headers)
  end

  if
    has("x-anonymous")
  then
    anonymous_logins :=
      [...anonymous_logins(), (request.user, request.password)]
    {refuse = {status = 418, reason = "Anonymous"}}
  elsif
    has("x-refuse")
  then
    {refuse = {status = 409, reason = "Busy", headers = [("X-Why", "taken")]}}
  elsif
    has("x-invalid")
  then
    {refuse = {status = 200, reason = "OK"}}
  elsif
    has("x-raise")
  then
    error.raise(
      error.not_found,
      "accept failed"
    )
  else
    null
  end
end

live = input.harbor(port=port, auth=auth, accept=accept, "acc")
output.dummy(fallible=true, live)
def plain_auth(_) =
  plain_auth_calls := plain_auth_calls() + 1
  true
end

plain = input.harbor(port=port, auth=plain_auth, "plain")
output.dummy(fallible=true, plain)

# The reply head to a SOURCE request on `mount` carrying `headers`, with
# credentials unless `anonymous`.
def source_request(~anonymous=false, ~mount="acc", headers) =
  sock = socket.unix()
  data = ref("")
  try
    sock.connect(
      socket.address.internet_address(
        socket.internet_address("127.0.0.1"),
        port
      )
    )
    credentials =
      anonymous
        ? ""
        : "Authorization: Basic #{string.base64.encode("u:p")}\r\n"
    sock.write(
      "SOURCE /#{mount} HTTP/1.0\r\n#{credentials}#{headers}\r\n"
    )
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
        and not string.contains(encoding="ascii", substring="\r\n\r\n", data())
      then
        read()
      end
    end

    read()
  finally
    sock.close()
  end
  data()
end

def run() =
  mime =
    "Content-Type: audio/mpeg\r\n"
  missing = source_request("")
  check(
    "no Content-Type is 400: #{missing}",
    string.contains(
      prefix="HTTP/1.0 400",
      missing
    )
  )
  refused =
    source_request(
      "#{mime}X-Refuse: 1\r\nX-Test: A\r\n"
    )
  check(
    "accept's refusal is sent as given: #{refused}",
    string.contains(
      prefix="HTTP/1.0 409 Busy\r\n",
      refused
    )
    and string.contains(
      substring="X-Why: taken\r\n",
      refused
    )
  )
  check(
    "auth sees lowercase header names: #{seen_headers()}",
    list.assoc(default="", "x-test", seen_headers()) == "A"
  )
  invalid =
    source_request(
      "#{mime}X-Invalid: 1\r\n"
    )
  check(
    "an invalid refusal is 403: #{invalid}",
    string.contains(
      prefix="HTTP/1.0 403",
      invalid
    )
  )
  raised =
    source_request(
      "#{mime}X-Raise: 1\r\n"
    )
  check(
    "a raising accept is 403: #{raised}",
    string.contains(
      prefix="HTTP/1.0 403",
      raised
    )
  )
  anonymous =
    source_request(
      anonymous=true,
      "#{mime}X-Anonymous: 1\r\n"
    )
  check(
    "with accept, a request without credentials reaches it with empty ones: #{
      anonymous
    } #{anonymous_logins()}",
    string.contains(
      prefix="HTTP/1.0 418",
      anonymous
    )
    and anonymous_logins() == [("", "")]
  )
  plain_reply = source_request(anonymous=true, mount="plain", mime)
  check(
    "without accept, a request without credentials is 401 before auth: #{
      plain_reply
    } #{plain_auth_calls()}",
    string.contains(
      prefix="HTTP/1.0 401",
      plain_reply
    )
    and plain_auth_calls() == 0
  )
  admitted = source_request(mime)
  check(
    "a null decision admits: #{admitted}",
    string.contains(
      prefix="HTTP/1.0 200",
      admitted
    )
  )
  if
    checks() != 8
  then
    test.fail(
      "only #{checks()} checks ran"
    )
  end
  test.pass()
end

thread.run(delay=1., run)
