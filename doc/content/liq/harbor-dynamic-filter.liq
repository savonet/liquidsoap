input.harbor.dynamic(
  port=8005,
  "/example",
  on_connect=fun (c) ->
    begin
      list.iter(
        fun (s) ->
          log(
            "Stream: #{s.field} type=#{s.type} codec=#{s.codec}"
          ),
        c.streams
      )
      if
        list.exists(fun (s) -> s.type == "audio", c.streams)
      then
        fun (source) -> output.file(c.copy_encoder(), "/tmp/audio.ogg", source)
      else
        error.raise(
          error.not_found,
          "No audio stream, rejecting connection"
        )
      end
    end
)
