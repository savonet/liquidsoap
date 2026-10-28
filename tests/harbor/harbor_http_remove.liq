%include "../test.liq"

# Registering a path already registered on the port raises; a registration's
# `remove` frees its path only, and removing it twice is a no-op.
port = 18135
first =
  harbor.http.register.simple(
    port=port,
    "/x",
    fun (_) -> http.response(data="first")
  )
harbor.http.register.simple(
  port=port,
  "/keep",
  fun (_) -> http.response(data="keep")
)

def fetch(route) =
  response = http.get("http://127.0.0.1:#{port}#{route}")
  (response.status_code, "#{response}")
end

def check() =
  test.equal(fetch("/x"), (200, "first"))
  duplicate =
    try
      ignore(
        harbor.http.register.simple(
          port=port,
          "/x",
          fun (_) -> http.response(data="duplicate")
        )
      )
      false
    catch err : [error.http] do
      string.contains(
        substring="already registered",
        err.message
      )
    end
  test.equal(duplicate, true)
  test.equal(fetch("/x"), (200, "first"))

  first.remove()
  first.remove()
  test.equal(fst(fetch("/x")), 404)
  test.equal(fetch("/keep"), (200, "keep"))

  harbor.http.register.simple(
    port=port,
    "/x",
    fun (_) -> http.response(data="second")
  )
  test.equal(fetch("/x"), (200, "second"))
  test.pass()
end

thread.run(delay=1., check)
