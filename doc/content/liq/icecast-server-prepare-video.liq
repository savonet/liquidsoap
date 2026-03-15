# Video source to loop when the sender sends audio only
video_bg = source.tracks(playlist("/path/to/videos")).video

def output_has_video(
  ~mount,
  ~port,
  ~transport,
  ~headers,
  ~burst,
  ~timeout,
  ~dumpfile,
  s
) =
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
    %ffmpeg(format = "matroska", %audio.copy, %video),
    s
  )
end

def output_no_video(
  ~mount,
  ~port,
  ~transport,
  ~headers,
  ~burst,
  ~timeout,
  ~dumpfile,
  s
) =
  let {audio = a, metadata = m, track_marks = tm} = source.tracks(s)
  s = source({audio = a, video = video_bg, metadata = m, track_marks = tm})
  output_has_video(
    mount=mount,
    port=port,
    transport=transport,
    headers=headers,
    burst=burst,
    timeout=timeout,
    dumpfile=dumpfile,
    s
  )
end

icecast.server(
  port=8000,
  password="hackme",
  dedicated_encoder=true,
  prepare=fun (
    {streams, mount, headers, burst, timeout, port, transport, dumpfile}
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
        if
          list.exists(fun (s) -> s.type == "video", streams)
        then
          output_has_video(
            mount=mount,
            port=port,
            transport=transport,
            headers=headers,
            burst=burst,
            timeout=timeout,
            dumpfile=dumpfile,
            output_source
          )
        else
          output_no_video(
            mount=mount,
            port=port,
            transport=transport,
            headers=headers,
            burst=burst,
            timeout=timeout,
            dumpfile=dumpfile,
            output_source
          )
        end
      end
)
