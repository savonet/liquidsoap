# Match /live/foo-<number>, e.g. /live/foo-42.
# Named capture groups are available by name in c.query.
input.harbor.dynamic.regexp(
  port=8005,
  r/^\/live\/foo-(?<id>\d+)$/,
  on_connection=fun (c) ->
    begin
      let id = list.assoc(default="0", "id", c.query)
      in
      log(
        "Stream foo-#{id} connected, format: #{c.format}"
      )
      output.harbor(c.copy_encoder(), mount="/relay/foo-#{id}", c.source)
    end
)
