%include "../test.liq"

# A handler sees the query string as the client sent it: undecoded, in its
# original order.
port = 18149

def f() =
  seen = ref("")
  harbor.http.register(
    port=port,
    method="GET",
    "/signed",
    fun (request, response) ->
      begin
        seen := request.query_string
        response.data("ok")
      end
  )
  ignore(
    http.get(
      normalize_url=false,
      "http://127.0.0.1:#{port}/signed?b=2&a=%2F&sig=x%2By"
    )
  )
  if
    seen() != "b=2&a=%2F&sig=x%2By"
  then
    test.fail(
      "query_string: #{seen()}"
    )
  end
  test.pass()
end

test.check(f)
