# Match /live/<name> and relay each stream to a separate harbor mountpoint.
# The :name placeholder is available as a named key in c.query.
input.harbor.dynamic(
  port=8005,
  "/live/:name",
  on_connect=fun (c) ->
    begin
      let name = list.assoc(default="unknown", "name", c.query)
      log(
        "Stream '#{name}' connected, format: #{c.format}"
      )
      fun (source) ->
        output.harbor(c.copy_encoder(), mount="/relay/#{name}", source)
    end
)
