#!../../src/liquidsoap ../../libs/pervasives.liq

%include "test.liq"

ignore(%ffmpeg(%video(codec="h264_nvenc")))

ignore(%ffmpeg(%video(codec="h264_nvenc",hwaccel=none)))

ignore(%ffmpeg(%video(codec="h264_nvenc",hwaccel=auto,hwaccel_device=none)))

ignore(%ffmpeg(%video(codec="h264_nvenc",hwaccel_device="foo")))

ignore(%ffmpeg(format="mpegts",
                %audio(
                  codec="aac",
                  channels=2,
                  ar=44100
                )))

ignore(%ffmpeg(format="mpegts",
               %audio(
                 codec="aac",
                 channels=2,
                 ar=44100,
                 b="96k"
               )))

ignore(%ffmpeg(format="mpegts",
               %audio(
                codec="aac",
                channels=2,
                ar=44100,
                b="192k"
              )))

ignore(%ffmpeg(
         format="mpegts",
         %audio(
            codec="aac",
            b="128k",
            channels=2,
            ar=44100
         ),
         %video(
           codec="libx264",
           b="5M"
         )
       ))

ignore(%ffmpeg(
        format="mp4",
        movflags="+dash+skip_sidx+skip_trailer+frag_custom",
        frag_duration=10,
        %audio(
          codec="aac",
          b="128k",
          channels=2,
          ar=44100),
        %video(
          codec="libx264",
          b="5M"
        )
      ))

ignore(%ffmpeg(
         format="mpegts",
         %audio.raw(
            codec="aac",
            b="128k",
            channels=2,
            ar=44100
         ),
         %video.raw(
           codec="libx264",
           b="5M"
         )
       ))

ignore(%ffmpeg(
        format="mp4",
        movflags="+dash+skip_sidx+skip_trailer+frag_custom",
        frag_duration=10,
        %audio.raw(
          codec="aac",
          b="128k",
          channels=2,
          ar=44100),
        %video.raw(
          codec="libx264",
          b="5M"
        )
      ))

ignore(%ffmpeg(
         format="mpegts",
         %audio.copy,
         %video.copy))

ignore(%ffmpeg(
        format="mp4",
        movflags="+dash+skip_sidx+skip_trailer+frag_custom",
        frag_duration=10,
        %audio.copy,
        %video.copy))

test.pass()

output.dummy(blank())
