def f() =
  playlist =
    hls.playlist.main(
      extra_tags=["foo", "bar"],
      version=54,
      prefix="prefix",
      [
        "stream_a".{bandwidth=123, codecs="codecs_a"},
        "steam_b".{bandwidth=456, codecs="codecs_b", video_size=(12, 34)}
      ]
    )
  test.equals(
    playlist,
    '#EXTM3U\r
#EXT-X-VERSION:54\r
foo\r
bar\r
#EXT-X-STREAM-INF:BANDWIDTH=123,CODECS="codecs_a"\r
prefix/stream_a.m3u8\r
#EXT-X-STREAM-INF:BANDWIDTH=456,CODECS="codecs_b",RESOLUTION=12x34\r
prefix/steam_b.m3u8\r
'
  )

  playlist =
    hls.playlist.main(
      extra_tags=["foo", "bar"],
      version=54,
      prefix="prefix/",
      [
        "stream_a".{bandwidth=123, codecs="codecs_a"},
        "steam_b".{bandwidth=456, codecs="codecs_b", video_size=(12, 34)}
      ]
    )
  test.equals(
    playlist,
    '#EXTM3U\r
#EXT-X-VERSION:54\r
foo\r
bar\r
#EXT-X-STREAM-INF:BANDWIDTH=123,CODECS="codecs_a"\r
prefix/stream_a.m3u8\r
#EXT-X-STREAM-INF:BANDWIDTH=456,CODECS="codecs_b",RESOLUTION=12x34\r
prefix/steam_b.m3u8\r
'
  )

  playlist =
    hls.playlist.main(
      version=54,
      [
        "stream_a".{bandwidth=123, codecs="codecs_a"},
        "steam_b".{bandwidth=456, codecs="codecs_b", video_size=(12, 34)}
      ]
    )
  test.equals(
    playlist,
    '#EXTM3U\r
#EXT-X-VERSION:54\r
#EXT-X-STREAM-INF:BANDWIDTH=123,CODECS="codecs_a"\r
stream_a.m3u8\r
#EXT-X-STREAM-INF:BANDWIDTH=456,CODECS="codecs_b",RESOLUTION=12x34\r
steam_b.m3u8\r
'
  )

  test.pass()
end

test.check(f)
