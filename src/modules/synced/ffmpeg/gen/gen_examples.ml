let stanza name libraries =
  Printf.printf "(executable\n (name %s)\n (modules %s)\n (libraries %s))\n\n"
    name name
    (String.concat " " libraries)

let () =
  if Has_ffmpeg.available then begin
    stanza "hw_encode" ["ffmpeg-av"];
    stanza "encode_video" ["ffmpeg-av"];
    stanza "read_metadata" ["ffmpeg-av"];
    stanza "remuxing" ["ffmpeg-av"];
    stanza "transcoding" ["ffmpeg-av"];
    stanza "decoding" ["ffmpeg-av"];
    stanza "interrupt" ["ffmpeg-av"];
    stanza "subtitle_remux" ["ffmpeg-av"];
    stanza "aresample" ["ffmpeg-av"; "ffmpeg-avfilter"];
    stanza "decode_stream" ["ffmpeg-av"; "ffmpeg-avfilter"];
    stanza "fps" ["ffmpeg-av"; "ffmpeg-avfilter"];
    stanza "fps_samplerate" ["ffmpeg-av"; "ffmpeg-avfilter"];
    stanza "transcode_aac" ["ffmpeg-av"; "ffmpeg-avfilter"];
    stanza "audio_decoding" ["ffmpeg-av"; "ffmpeg-swresample"];
    stanza "encoding" ["ffmpeg-av"; "ffmpeg-swresample"];
    stanza "encode_stream" ["ffmpeg-av"; "ffmpeg-avfilter"; "ffmpeg-swresample"];
    stanza "demuxing_decoding"
      ["ffmpeg-av"; "ffmpeg-swresample"; "ffmpeg-swscale"];
    stanza "player" ["ffmpeg-av"; "ffmpeg-avdevice"];
    stanza "webcam" ["ffmpeg-av"; "ffmpeg-avdevice"];
    stanza "bitmap_subtitle_to_jpeg" ["ffmpeg-av"; "ffmpeg-swscale"];
    stanza "encode_audio" ["ffmpeg-avcodec"; "ffmpeg-swresample"];
    stanza "reenc_vp9" ["ffmpeg-av"; "ffmpeg-avcodec"];
    stanza "all_codecs" ["ffmpeg-avcodec"];
    stanza "all_bitstream_filters" ["ffmpeg-avcodec"];
    stanza "all_channel_layouts" ["ffmpeg-avutil"];
    stanza "list_filters" ["ffmpeg-avfilter"];
    stanza "filter_info" ["ffmpeg-avfilter"];
    stanza "audio_device" ["ffmpeg-avdevice"]
  end
