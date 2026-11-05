%include "../test.liq"

# playlists.md §3.1, §4: every byte of `hls.playlist.main`, with extra tags,
# prefixes with and without a trailing `/`, CODECS and RESOLUTION present and
# absent.
checks = ref(0)

def expect(name, result, lines) =
  expected = string.concat(separator="\r\n", lines) ^ "\r\n"
  if
    result != expected
  then
    test.fail(
      "#{name}: got #{string.quote(result)}, expected #{string.quote(expected)}"
    )
  end
  checks := checks() + 1
end

a = "stream_a".{bandwidth = 123, codecs = "mp4a.40.2"}
b =
  "stream_b".{
    bandwidth = 456,
    codecs = "avc1.64001f,mp4a.40.2",
    video_size = (1280, 720)
  }

no_codecs = "c".{bandwidth = 789, codecs = ""}
video_only = "d".{bandwidth = 1000, codecs = "", video_size = (640, 360)}
spaced =
  "e f".{bandwidth = 1, codecs = "mp4a.40.2"}

example =
  [
    "#EXTM3U",
    "#EXT-X-VERSION:6",
    "#EXT-X-STREAM-INF:BANDWIDTH=123,CODECS=\"mp4a.40.2\"",
    "prefix/stream_a.m3u8",
    "#EXT-X-STREAM-INF:BANDWIDTH=456,CODECS=\"avc1.64001f,mp4a.40.2\",RESOLUTION=1280x720",
    "prefix/stream_b.m3u8"
  ]

expect(
  "prefix without /",
  hls.playlist.main(prefix="prefix", version=6, [a, b]),
  example
)

expect(
  "prefix with /",
  hls.playlist.main(prefix="prefix/", version=6, [a, b]),
  example
)

expect(
  "no prefix, default version",
  hls.playlist.main([a, b]),
  [
    "#EXTM3U",
    "#EXT-X-VERSION:7",
    "#EXT-X-STREAM-INF:BANDWIDTH=123,CODECS=\"mp4a.40.2\"",
    "stream_a.m3u8",
    "#EXT-X-STREAM-INF:BANDWIDTH=456,CODECS=\"avc1.64001f,mp4a.40.2\",RESOLUTION=1280x720",
    "stream_b.m3u8"
  ]
)

expect(
  "extra tags",
  hls.playlist.main(
    extra_tags=["#EXT-X-INDEPENDENT-SEGMENTS", "#EXT-X-START:TIME-OFFSET=-6"],
    version=6,
    [a]
  ),
  [
    "#EXTM3U",
    "#EXT-X-VERSION:6",
    "#EXT-X-INDEPENDENT-SEGMENTS",
    "#EXT-X-START:TIME-OFFSET=-6",
    "#EXT-X-STREAM-INF:BANDWIDTH=123,CODECS=\"mp4a.40.2\"",
    "stream_a.m3u8"
  ]
)

expect(
  "no CODECS, RESOLUTION without CODECS",
  hls.playlist.main(prefix="/hls", version=6, [no_codecs, video_only]),
  [
    "#EXTM3U",
    "#EXT-X-VERSION:6",
    "#EXT-X-STREAM-INF:BANDWIDTH=789",
    "/hls/c.m3u8",
    "#EXT-X-STREAM-INF:BANDWIDTH=1000,RESOLUTION=640x360",
    "/hls/d.m3u8"
  ]
)

expect(
  "percent-encoded name",
  hls.playlist.main(prefix="http://example.com/live/", version=6, [spaced]),
  [
    "#EXTM3U",
    "#EXT-X-VERSION:6",
    "#EXT-X-STREAM-INF:BANDWIDTH=1,CODECS=\"mp4a.40.2\"",
    "http://example.com/live/e%20f.m3u8"
  ]
)

expect(
  "no stream",
  hls.playlist.main(version=6, []),
  ["#EXTM3U", "#EXT-X-VERSION:6"]
)

if
  checks() != 7
then
  test.fail(
    "#{checks()} of 7 cases checked"
  )
end
test.pass()
