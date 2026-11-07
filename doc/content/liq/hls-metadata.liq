radio = mksafe(playlist("~/music"))

# BEGIN
o =
  output.file.hls(
    "/var/www/hls",
    cut_on_metadata=true,
    [
      ("aac", %fdkaac(bitrate = 128).{id3_version = 4}),
      (
        "ts",
        %ffmpeg(format = "mpegts", %audio(codec = "aac")).{replay_id3 = false}
      ),
      ("plain", %mp3(bitrate = 128).{id3 = false})
    ],
    radio
  )

# Mark the start of an advertisement break in every media playlist.
def ad_break(seconds) =
  o.insert_tag("#EXT-X-CUE-OUT:DURATION=#{seconds}")
end

# A tag for one variant only.
def mark_ts(tag) =
  ts = list.find(fun (v) -> v.name == "ts", o.streams())
  ts.insert_tag(tag)
end

thread.run(every=600., {ad_break(30.)})
thread.run(
  delay=60.,
  {mark_ts("#EXT-X-DATERANGE:ID=\"intro\",START-DATE=\"2026-01-01T00:00:00Z\"")}
)

# Print what each variant announces.
thread.run(
  delay=30.,
  {
    list.iter(
      fun (v) ->
        print(
          "#{v.name}: #{v.bandwidth} bit/s, codecs #{v.codecs}"
        ),
      o.streams()
    )
  }
)
# END
