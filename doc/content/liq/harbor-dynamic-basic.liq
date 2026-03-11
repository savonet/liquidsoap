input.harbor.dynamic(
  port=8005,
  "/example",
  on_connection=fun (c) ->
    begin
      log(
        "Client connected on #{c.uri}, format: #{c.format}"
      )
      output.file(c.copy_encoder(), "/tmp/record-#{time()}.%(ext)", c.source)
    end
)
