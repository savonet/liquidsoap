let stanza name libraries =
  Printf.printf "(executable\n (name %s)\n (modules %s)\n (libraries %s))\n\n"
    name name
    (String.concat " " libraries)

let () =
  if Sys.argv.(1) = "true" then begin
    stanza "test_resample" ["ffmpeg-av"; "ffmpeg-swresample"];
    stanza "test_info" ["ffmpeg-av"; "ffmpeg-swresample"];
    stanza "test_subtitle_read" ["ffmpeg-av"];
    stanza "test_unhandled_packet" ["ffmpeg-av"];
    print_string
      {|
(rule
 (alias ffmpeg_citest)
 (package ffmpeg)
 (deps
  (:runner test_runner.exe)
  (:list_filters ../examples/list_filters.exe)
  (:all_codecs ../examples/all_codecs.exe)
  (:all_channel_layouts ../examples/all_channel_layouts.exe)
  (:all_bitstream_filters ../examples/all_bitstream_filters.exe)
  (:hw_encode ../examples/hw_encode.exe)
  (:interrupt ../examples/interrupt.exe)
  (:resample test_resample.exe)
  (:info test_info.exe)
  (:encode_audio ../examples/encode_audio.exe)
  (:encode_video ../examples/encode_video.exe)
  (:encode_stream ../examples/encode_stream.exe)
  (:encoding ../examples/encoding.exe)
  (:decode_stream ../examples/decode_stream.exe)
  (:audio_decoding ../examples/audio_decoding.exe)
  (:remuxing ../examples/remuxing.exe)
  (:transcode_aac ../examples/transcode_aac.exe)
  (:transcoding ../examples/transcoding.exe)
  (:decoding ../examples/decoding.exe)
  (:fps ../examples/fps.exe)
  (:aresample ../examples/aresample.exe)
  (:demuxing_decoding ../examples/demuxing_decoding.exe)
  (:read_metadata ../examples/read_metadata.exe)
  (:subtitle_read test_subtitle_read.exe)
  (:unhandled_packet test_unhandled_packet.exe)
  (:subtitle_remux ../examples/subtitle_remux.exe)
  (:normalize normalize_line_endings.exe)
  (:srt fixtures/sample.srt))
 (action
  (progn
   (run %{runner} "list_filters" %{list_filters})
   (run %{runner} "all_codecs" %{all_codecs})
   (run %{runner} "all_channel_layouts" %{all_channel_layouts})
   (run %{runner} "all_bitstream_filters" %{all_bitstream_filters})
   (run %{runner} "hw_encode_device" %{hw_encode} nvenc.mp4 h264_nvenc device)
   (run %{runner} "hw_encode_frame" %{hw_encode} nvenc.mp4 h264_nvenc frame)
   (run %{runner} "interrupt" %{interrupt})
   (run %{runner} "resample" %{resample})
   (run %{runner} "info" %{info})
   (run %{runner} "encode_audio_flac" %{encode_audio} A4.flac flac)
   (run %{runner} "encode_audio_mp2" %{encode_audio} A4.mp2 mp2)
   (run %{runner} "encode_video_webm" %{encode_video} video.webm yuva420p libvpx-vp9)
   (run %{runner} "encode_stream_flac" %{encode_stream} S4.flac flac)
   (run %{runner} "encode_stream_mp2" %{encode_stream} S4.mp2 mp2)
   (run %{runner} "encoding" %{encoding} out.mkv aac mpeg4 ass)
   (run %{runner} "decode_stream_mp3" %{decode_stream} A4.flac A4.mp3 libmp3lame)
   (run %{runner} "decode_stream_ogg" %{decode_stream} A4.mp2 A4.ogg libvorbis)
   (run %{runner} "audio_decoding_ogg" %{audio_decoding} A4.ogg ogg A4)
   (run %{runner} "audio_decoding_mkv" %{audio_decoding} out.mkv matroska out)
   (run %{runner} "remuxing" %{remuxing} out.mkv out_remuxed.mp4)
   (run %{runner} "transcode_aac" %{transcode_aac} A4.ogg A4_transcoded.mp4)
   (run %{runner} "transcoding" %{transcoding} out.mkv out_transcoded.mp4)
   (run %{runner} "decoding" %{decoding} out.mkv)
   (run %{runner} "fps" %{fps} out.mkv out_fps.mp4)
   (run %{runner} "aresample" %{aresample} out.mkv out_aresample.mp4)
   (run %{runner} "demuxing_decoding" %{demuxing_decoding} out.mkv vo.raw ao.raw)
   (run %{runner} "read_metadata_flac" %{read_metadata} A4.flac flac A4.mp3 libmp3lame)
   (run %{runner} "read_metadata_mp2" %{read_metadata} A4.mp2 mp2 A4.ogg libvorbis)
   (run
    ffmpeg
    -y
    -f
    lavfi
    -i
    "color=c=blue:s=320x240:d=25"
    -f
    lavfi
    -i
    "anullsrc=r=44100:cl=stereo:d=25"
    -i
    %{srt}
    -c:v
    mpeg4
    -c:a
    aac
    -c:s
    srt
    -shortest
    test_with_subs.mkv)
   (run %{runner} "subtitle_read" %{subtitle_read} test_with_subs.mkv)
   (run %{runner} "unhandled_packet" %{unhandled_packet} test_with_subs.mkv)
   (run %{subtitle_remux} test_with_subs.mkv raw_remuxed_subs.srt subrip)
   (run %{normalize} raw_remuxed_subs.srt remuxed_subs.srt)
   (run diff fixtures/sample.srt remuxed_subs.srt))))
|}
  end
