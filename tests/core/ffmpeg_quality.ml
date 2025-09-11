open Ffmpeg_format

let () = Frame_settings.lazy_config_eval := true

let () =
  let vorbis_enc =
    {
      format = Some "ogg";
      interleaved = `Default;
      output = `Stream;
      metadata = Frame.Metadata.empty;
      streams =
        [
          ( Frame.Fields.audio,
            `Encode
              {
                mode = `Internal;
                codec = Some "libvorbis";
                options =
                  `Audio
                    {
                      pcm_kind = Content.Audio.kind;
                      channels = 2;
                      samplerate = lazy 44100;
                      sample_format = None;
                    };
                opts = Hashtbl.create 0;
              } );
        ];
      opts = Hashtbl.create 0;
    }
  in
  let mk_encoder = Encoder.get_factory (Encoder.Ffmpeg vorbis_enc) in
  let enc = mk_encoder ~pos:None "ffmpeg" Frame.Metadata.Export.empty in
  assert (Encoder.(enc.hls.bitrate ()) = None)

let () =
  let mixed_enc =
    {
      format = Some "ogg";
      interleaved = `Default;
      output = `Stream;
      metadata = Frame.Metadata.empty;
      streams =
        [
          ( Frame.Fields.audio,
            `Encode
              {
                mode = `Internal;
                codec = Some "libvorbis";
                options =
                  `Audio
                    {
                      pcm_kind = Content.Audio.kind;
                      channels = 2;
                      samplerate = lazy 44100;
                      sample_format = None;
                    };
                opts = Hashtbl.create 0;
              } );
          ( Frame.Fields.audio,
            `Encode
              {
                mode = `Internal;
                codec = Some "libmp3lame";
                options =
                  `Audio
                    {
                      pcm_kind = Content.Audio.kind;
                      channels = 2;
                      samplerate = lazy 44100;
                      sample_format = None;
                    };
                opts =
                  (let m = Hashtbl.create 0 in
                   Hashtbl.replace m "b" (`String "128k");
                   m);
              } );
        ];
      opts = Hashtbl.create 0;
    }
  in
  let mk_encoder = Encoder.get_factory (Encoder.Ffmpeg mixed_enc) in
  let enc = mk_encoder ~pos:None "ffmpeg" Frame.Metadata.Export.empty in
  assert (Encoder.(enc.hls.bitrate ()) = Some 128000)
