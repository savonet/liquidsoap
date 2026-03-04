# Video source to loop when the sender sends audio only
video_bg = source.tracks(playlist("/path/to/videos")).video

icecast.server(
  port=8000,
  password="hackme",
  dedicated_encoder=true,
  prepare=fun ({
    source = s,
    fallback = f,
    streams,
    mount,
    headers,
    burst,
    timeout,
    port,
    transport,
    dumpfile
  }) ->
  begin
    def output(s) =
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
        %ffmpeg(format="mkv", %audio.copy, %video),
        output_source
      )
    end
    has_video = list.exists(fun (s) -> s.type == "video", streams)
    if not has_video then
      # Replace the (empty) video track with one from the background playlist
      output(source(source.tracks(s)).{video=video_bg})
    else
      output(s)
    end
  end
)
