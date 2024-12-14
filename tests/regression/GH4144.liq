def metadata_test(_) =
  data = {test={test="好吗"}}
  try
    http.response(
      status_code=200,
      headers=
        [
          ("Content-Type", "application/json"),
          ("Access-Control-Allow-Origin", "*")
        ],
      content_type=
        "application/json; charset=UTF-8",
      data=json.stringify(data, compact=true)
    )
  catch error do
    log.severe(error.message, label="http")
    http.response(
      status_code=500,
      content_type=
        "application/json; charset=UTF-8",
      data=
        '{"status"="error","message"="Failed to serialize response data"}' ^
          "\n"
    )
  end
end

port = 4144

harbor.http.register.simple("/test", metadata_test, port=port, method="GET")

def check() =
  resp = http.get("http://localhost:#{port}/test")
  test.equal(resp, '{"test":{"test":"好吗"}}')
  test.pass()
end

test.check(check)
