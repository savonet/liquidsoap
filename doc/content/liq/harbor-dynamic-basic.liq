input.harbor.dynamic(
  port=8005,
  "/example",
  on_connect=fun (c) ->
    begin
      log(
        "Client connected on #{c.uri}, format: #{c.format}"
      )
      fun (source) ->
        output.file(c.copy_encoder(), "/tmp/record-#{time()}.%(ext)", source)
    end
)
