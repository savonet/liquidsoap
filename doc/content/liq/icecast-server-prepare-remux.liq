icecast.server(
  port=8000,
  password="hackme",
  prepare=fun (
    {
      copy_encoder,
      mount,
      headers,
      burst,
      timeout,
      port,
      transport,
      dumpfile,
      dedicated_encoder
    }
  ) ->
    fun ({source = s, fallback = f}) ->
      begin
        output_source =
          if
            null.defined(f)
          then
            fallback(track_sensitive=false, [s, null.get(f)])
          else
            s
          end
        output.harbor(
          fallible=true,
          mount=mount,
          port=port,
          transport=transport,
          headers=headers,
          timeout=timeout,
          burst=burst,
          dumpfile=dumpfile,
          dedicated_encoder=dedicated_encoder,
          format="audio/ogg",
          copy_encoder("ogg"),
          output_source
        )
      end
)
