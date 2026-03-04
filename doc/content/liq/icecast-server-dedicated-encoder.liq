icecast.server(
  port=8000,
  password="hackme",
  prepare=fun ({
    source = s,
    fallback = f,
    copy_encoder,
    mount,
    headers,
    burst,
    timeout,
    port,
    transport,
    dumpfile
  }) ->
  begin
    output_source =
      if null.defined(f) then
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
      dedicated_encoder=true,
      format="video/x-matroska",
      copy_encoder("matroska"),
      output_source
    )
  end
)
