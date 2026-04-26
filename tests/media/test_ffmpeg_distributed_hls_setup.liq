log.level.set(5)
audio.samplerate.set(48000)
settings.audio.converter.samplerate.converters.set(["native"])

debian_version =
  string.trim(
    process.run(
      "cat /etc/os-release | grep VERSION_ID | cut -d'=' -f 2 | xargs"
    ).stdout
  )

if debian_version == "10" then test.skip() end

fname = argv(default="", 1)

main_encoder =
  %ffmpeg(
    %audio(codec = "aac", b = "128k", channels = 2, ar = 44100),
    %video(codec = "libx264", b = "5M", flags = "+global_header")
  )

mpegts = %ffmpeg(format = "mpegts", %audio.copy, %video.copy)
mp4 =
  %ffmpeg(
    format = "mp4",
    movflags = "+dash+skip_sidx+skip_trailer+frag_custom",
    frag_duration = 2,
    %audio.copy,
    %video.copy
  )

s = single(fname)
encoded = ffmpeg.encode.audio_video(main_encoder, s)
streams = [("mp4", mp4), ("mpegts", mpegts)]

# Fixed relative path so dune can track it as a directory target.
output_dir = "hls_resume_output"

ignore(process.run("mkdir -p #{output_dir}"))

# Use position-only names (no timestamp) for deterministic output that
# dune can cache across runs.
def segment_name(metadata) =
  let {position, extname, stream_name} = metadata
  if position >= 5 then test.pass() end
  "#{stream_name}_#{position}.#{extname}"
end

output.file.hls(
  playlist="live.m3u8",
  segment_duration=2.0,
  segments=5,
  segments_overhead=5,
  segment_name=segment_name,
  output_dir,
  persist_at="#{output_dir}/state.json",
  streams,
  fallible=true,
  encoded
)
