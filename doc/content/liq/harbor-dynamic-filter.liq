input.harbor.dynamic(
  port=8005,
  on_connection=fun(c) ->
    begin
      list.iter(
        fun(s) -> log("Stream: #{s.field} type=#{s.type} codec=#{s.codec}"),
        c.streams
      )
      if list.exists(fun(s) -> s.type == "audio", c.streams) then
        output.file(c.copy_encoder(), "/tmp/audio.ogg", c.source)
      else
        error.raise(error.not_found, "No audio stream, rejecting connection")
      end
    end
)
