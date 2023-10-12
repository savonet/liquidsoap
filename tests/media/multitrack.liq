s1 = sine(440.)
audio = source.tracks(s1).audio

out = "test-multistream.mp4"

s2 = sine(880.)
audio_2 = source.tracks(s2).audio

s3 = single("@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4")
video = source.tracks(s3).video

s4 = blank()
s4 = insert_metadata(s4)
metadata = source.tracks(s4).metadata

s5 = blank()
s5 = chop(s5)
track_marks = source.tracks(s5).track_marks

s = source(id="muxed", {
  audio = audio,
  audio_2 = audio_2,
  video = video,
  metadata = metadata,
  track_marks = track_marks
})

meta_seen = ref(false)
track_seen = ref(false)

s.on_metadata(fun (m) -> if m["foo"] == "bla" then meta_seen := true end)
s.on_track(fun (_) -> track_seen := true)

# Remove track marks and metadata
let {metadata=_, track_marks=_, ...tracks} = source.tracks(s)

file_no_meta_seen = ref(true)
file_track_seen = ref(0)

s = source(id="muxed_final", tracks)

s.on_metadata(fun (_) -> file_no_meta_seen := false)
s.on_track(fun (_) -> ref.incr(file_track_seen))

should_stop = ref(false)

s = switch(track_sensitive=false, [({not should_stop()}, s)])

enc = %ffmpeg(
  format="mp4",
  %audio(codec="aac"),
  %audio_2(codec="libmp3lame", channels=1),
  %video.copy
)

def on_close(filename) =
  if not test.done() then
    test.equals(meta_seen(), true)
    test.equals(track_seen(), true)
    test.equals(file_no_meta_seen(), true)
    test.equals(file_track_seen(), 1)

    ojson = process.read("ffprobe -v quiet -print_format json -show_streams #{process.quote(filename)}")

    let json.parse ( parsed : {
      streams: [{
        channel_layout: string?,
        sample_rate: string?,
        sample_fmt: string?,
        codec_name: string?,
        pix_fmt: string?
      }]
    }) = ojson

    test.equals(list.length(parsed.streams), 3)

    video_stream = list.find((fun (stream) -> null.defined(stream.pix_fmt)), parsed.streams)
    aac_stream = list.find((fun (stream) -> stream.codec_name == "aac"), parsed.streams)
    mp3_stream = list.find((fun (stream) -> stream.codec_name == "mp3"), parsed.streams)

    test.equals(video_stream.codec_name, "h264")
    test.equals(video_stream.pix_fmt, "yuv420p")
    test.equals(aac_stream.channel_layout, "stereo")
    test.equals(mp3_stream.channel_layout, "mono")

    test.pass()
  end
end

output.file(fallible=true, on_close=on_close, enc, out, s)

meta_count = ref(0)

thread.run(every=2., fun () -> begin
  if meta_count() < 2 then
    s4.insert_metadata([("foo", "bla")])
    ref.incr(meta_count)
  else
   should_stop := true
  end
end)
