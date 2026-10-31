%include "../test.liq"
%include "../../src/libs/extra/icecast_server/settings.liq"
%include "../../src/libs/extra/icecast_server/prerolls.liq"

port = 18156
base = "http://127.0.0.1:#{port}"
checks = ref(0)

def check_equal(label, got, expected) =
  checks := checks() + 1
  if
    got != expected
  then
    test.fail(
      "#{label}: got #{got}, expected #{expected}"
    )
  end
end

# The ad server double: its reply is set by the test, and it records the
# If-None-Match of each request it gets.
ad_reply = ref({status = 200, etag = "e1", body = "AD-ONE"})
ad_requests = ref([])
def ad(request, response) =
  headers =
    list.map(
      fun (h) -> (string.case(lower=true, fst(h)), snd(h)),
      request.headers
    )
  ad_requests :=
    [...ad_requests(), list.assoc(default="", "if-none-match", headers)]
  reply = ad_reply()
  response.status_code(reply.status)
  if reply.etag != "" then response.header("ETag", reply.etag) end
  response.data(reply.body)
end
harbor.http.register(port=port, method="GET", "/ad", ad)

broken_requests = ref(0)
def broken(_, response) =
  broken_requests := broken_requests() + 1
  response.status_code(500)
  response.data("")
end
harbor.http.register(port=port, method="GET", "/broken", broken)

def big(_, response) =
  response.data(string.make(char_code=65, 2000))
end
harbor.http.register(port=port, method="GET", "/big", big)
harbor.http.register(port=port, method="GET", "/big2", big)

def check_revalidation() =
  fresh = icecast.server.preroll_cache(preroll_revalidate=3600.)
  url = "#{base}/ad"
  check_equal(
    "first fetch",
    fresh(url),
    "AD-ONE"
  )
  check_equal(
    "fresh copy used",
    fresh(url),
    "AD-ONE"
  )
  check_equal(
    "one request while fresh",
    ad_requests(),
    [""]
  )

  another_view = icecast.server.preroll_cache(preroll_revalidate=3600.)
  check_equal(
    "store shared by views",
    another_view(url),
    "AD-ONE"
  )
  check_equal(
    "no request from another view",
    list.length(ad_requests()),
    1
  )

  stale = icecast.server.preroll_cache(preroll_revalidate=0.)
  ad_reply := {status = 304, etag = "", body = ""}
  check_equal(
    "304 keeps the copy",
    stale(url),
    "AD-ONE"
  )
  check_equal(
    "revalidation carries the ETag",
    ad_requests(),
    ["", "e1"]
  )

  ad_reply := {status = 200, etag = "e2", body = "AD-TWO"}
  check_equal(
    "2xx replaces the copy",
    stale(url),
    "AD-TWO"
  )
  ad_reply := {status = 500, etag = "", body = ""}
  check_equal(
    "failure keeps the copy",
    stale(url),
    "AD-TWO"
  )
  check_equal(
    "ETag of the replacement",
    ad_requests(),
    ["", "e1", "e1", "e2"]
  )

  check_equal(
    "failed first fetch",
    fresh("#{base}/broken"),
    null
  )
  check_equal(
    "failed load not remembered",
    fresh("#{base}/broken"),
    null
  )
  check_equal(
    "each failed load fetches again",
    broken_requests(),
    2
  )
end

def check_sizes() =
  large = icecast.server.preroll_cache()
  small = icecast.server.preroll_cache(preroll_max_size=50)
  check_equal(
    "large view",
    string.length(large("#{base}/big") ?? ""),
    2000
  )
  check_equal(
    "cached preroll above the view's limit",
    small("#{base}/big"),
    null
  )
  check_equal(
    "fetch above the view's limit",
    small("#{base}/big2"),
    null
  )
  check_equal(
    "fetch above the limit not stored",
    list.mem("#{base}/big2", list.map(fst, icecast.server.prerolls.store())),
    false
  )
end

def check_files(directory) =
  view = icecast.server.preroll_cache(directory=directory)
  intro = path.concat(directory, "intro.mp3")
  def write(data, mtime) =
    file.write(data=data, intro)
    ignore(
      process.run(
        "touch -d @#{mtime} #{process.quote(intro)}"
      )
    )
  end

  write("INTRO-ONE", 1000000000)
  check_equal(
    "local file",
    view("intro.mp3"),
    "INTRO-ONE"
  )
  check_equal(
    "leading slash is relative",
    view("/intro.mp3"),
    "INTRO-ONE"
  )
  write("INTRO-TWO", 1000000000)
  check_equal(
    "unchanged mtime is not read again",
    view("intro.mp3"),
    "INTRO-ONE"
  )
  write("INTRO-TWO", 1000000100)
  check_equal(
    "changed file is read again",
    view("intro.mp3"),
    "INTRO-TWO"
  )
  inner = path.concat(directory, "inner")
  file.mkdir(inner)
  inner_view = icecast.server.preroll_cache(directory=inner)
  check_equal(
    "path outside the directory",
    inner_view("../intro.mp3"),
    null
  )
  check_equal(
    "missing file",
    view("missing.mp3"),
    null
  )
  check_equal(
    "backend local path ignored",
    view.backend("intro.mp3"),
    ""
  )
  check_equal(
    "backend failed load",
    view.backend("#{base}/broken"),
    ""
  )
  check_equal(
    "view exposes its max size",
    view.preroll_max_size,
    10485760
  )
end

def check_budget(directory) =
  settings.icecast.server.preroll_cache_size := 250
  view = icecast.server.preroll_cache(directory=directory)
  list.iter(
    fun (name) ->
      file.write(
        data=string.make(char_code=66, 100),
        path.concat(directory, name)
      ),
    ["a", "b", "c"]
  )
  def keys() =
    list.map(
      fun (entry) -> path.basename(fst(entry)),
      icecast.server.prerolls.store()
    )
  end

  def total() =
    list.fold(
      fun (total, entry) -> total + string.length(snd(entry).data),
      0,
      icecast.server.prerolls.store()
    )
  end

  icecast.server.prerolls.store := []
  ad_reply := {status = 200, etag = "e3", body = "AD-THREE"}
  url = "#{base}/ad"
  ignore(view(url))
  ignore(view("a"))
  ignore(view("b"))
  ignore(view("a"))
  check_equal(
    "within budget",
    keys(),
    ["a", "b", "ad"]
  )
  ignore(view("c"))
  check_equal(
    "least recently used dropped",
    keys(),
    ["c", "a"]
  )
  check_equal(
    "total within budget",
    total() <= 250,
    true
  )

  requests = list.length(ad_requests())
  check_equal(
    "dropped URL fetched again",
    view(url),
    "AD-THREE"
  )
  check_equal(
    "fetched without validator",
    list.nth(default="?", ad_requests(), requests),
    ""
  )
  settings.icecast.server.preroll_cache_size := 268435456
end

def f() =
  directory = file.temp_dir(cleanup=true, "icecast-prerolls")
  check_revalidation()
  check_sizes()
  check_files(directory)
  check_budget(directory)
  if
    checks() < 30
  then
    test.fail(
      "only #{checks()} checks ran"
    )
  end
  test.pass()
end

test.check(f)
