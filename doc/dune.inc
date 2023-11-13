
(rule
  (alias doc)
  (deps )
  (target protocols.md)
  (action
    (with-stdout-to protocols.md
      (setenv PAGER none
        (run %{bin:liquidsoap} --list-protocols-md)))))

(rule
  (alias doc)
  (deps (:header content/reference-header.md))
  (target reference.md)
  (action
    (with-stdout-to reference.md
      (progn (cat %{header}) (echo "\n")
      (setenv PAGER none
        (run %{bin:liquidsoap} --list-functions-md)))))
      )

(rule
  (alias doc)
  (deps (:header content/reference-header.md))
  (target reference-extras.md)
  (action
    (with-stdout-to reference-extras.md
      (progn (cat %{header}) (echo "\n")
      (setenv PAGER none
        (run %{bin:liquidsoap} --list-extra-functions-md)))))
      )

(rule
  (alias doc)
  (deps (:header content/reference-header.md))
  (target reference-deprecated.md)
  (action
    (with-stdout-to reference-deprecated.md
      (progn (cat %{header}) (echo "\n")
      (setenv PAGER none
        (run %{bin:liquidsoap} --list-deprecated-functions-md)))))
      )

(rule
  (alias doc)
  (deps )
  (target settings.md)
  (action
    (with-stdout-to settings.md
      (setenv PAGER none
        (run %{bin:liquidsoap} --list-settings)))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target protocols.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md protocols.md))
  (target protocols.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=protocols --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target reference.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md reference.md))
  (target reference.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=reference --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target reference-extras.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md reference-extras.md))
  (target reference-extras.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=reference-extras --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target reference-deprecated.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md reference-deprecated.md))
  (target reference-deprecated.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=reference-deprecated --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target settings.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md settings.md))
  (target settings.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=settings --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target beets.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/beets.md))
  (target beets.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=beets --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target blank.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/blank.md))
  (target blank.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=blank --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target book.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/book.md))
  (target book.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=book --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target build.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/build.md))
  (target build.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=build --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target clocks.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/clocks.md))
  (target clocks.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=clocks --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target complete_case.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/complete_case.md))
  (target complete_case.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=complete_case --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target cookbook.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/cookbook.md))
  (target cookbook.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=cookbook --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target crossfade.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/crossfade.md))
  (target crossfade.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=crossfade --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target custom-path.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/custom-path.md))
  (target custom-path.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=custom-path --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target documentation.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/documentation.md))
  (target documentation.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=documentation --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target dynamic_sources.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/dynamic_sources.md))
  (target dynamic_sources.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=dynamic_sources --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target encoding_formats.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/encoding_formats.md))
  (target encoding_formats.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=encoding_formats --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target external_decoders.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/external_decoders.md))
  (target external_decoders.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=external_decoders --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target external_encoders.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/external_encoders.md))
  (target external_encoders.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=external_encoders --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target external_streams.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/external_streams.md))
  (target external_streams.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=external_streams --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target faq.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/faq.md))
  (target faq.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=faq --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target ffmpeg.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/ffmpeg.md))
  (target ffmpeg.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=ffmpeg --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target ffmpeg_cookbook.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/ffmpeg_cookbook.md))
  (target ffmpeg_cookbook.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=ffmpeg_cookbook --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target ffmpeg_encoder.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/ffmpeg_encoder.md))
  (target ffmpeg_encoder.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=ffmpeg_encoder --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target ffmpeg_filters.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/ffmpeg_filters.md))
  (target ffmpeg_filters.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=ffmpeg_filters --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target flows_devel.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/flows_devel.md))
  (target flows_devel.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=flows_devel --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target frequence3.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/frequence3.md))
  (target frequence3.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=frequence3 --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target geekradio.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/geekradio.md))
  (target geekradio.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=geekradio --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target gstreamer_encoder.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/gstreamer_encoder.md))
  (target gstreamer_encoder.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=gstreamer_encoder --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target harbor.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/harbor.md))
  (target harbor.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=harbor --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target harbor_http.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/harbor_http.md))
  (target harbor_http.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=harbor_http --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target help.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/help.md))
  (target help.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=help --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target hls_output.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/hls_output.md))
  (target hls_output.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=hls_output --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target http_input.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/http_input.md))
  (target http_input.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=http_input --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target icy_metadata.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/icy_metadata.md))
  (target icy_metadata.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=icy_metadata --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target in_production.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/in_production.md))
  (target in_production.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=in_production --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target index.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/index.md))
  (target index.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=index --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target install.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/install.md))
  (target install.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=install --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target json.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/json.md))
  (target json.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=json --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target ladspa.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/ladspa.md))
  (target ladspa.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=ladspa --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target language.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/language.md))
  (target language.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=language --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target memory.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/memory.md))
  (target memory.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=memory --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target metadata.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/metadata.md))
  (target metadata.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=metadata --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target migrating.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/migrating.md))
  (target migrating.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=migrating --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target multitrack.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/multitrack.md))
  (target multitrack.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=multitrack --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target on2.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/on2.md))
  (target on2.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=on2 --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target on2_part1.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/on2_part1.md))
  (target on2_part1.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=on2_part1 --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target on2_part2.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/on2_part2.md))
  (target on2_part2.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=on2_part2 --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target on2_part3.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/on2_part3.md))
  (target on2_part3.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=on2_part3 --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target phases.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/phases.md))
  (target phases.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=phases --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target playlist_parsers.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/playlist_parsers.md))
  (target playlist_parsers.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=playlist_parsers --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target presentations.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/presentations.md))
  (target presentations.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=presentations --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target profiling.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/profiling.md))
  (target profiling.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=profiling --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target prometheus.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/prometheus.md))
  (target prometheus.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=prometheus --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target protocols-presentation.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/protocols-presentation.md))
  (target protocols-presentation.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=protocols-presentation --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target publications.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/publications.md))
  (target publications.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=publications --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target quick_start.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/quick_start.md))
  (target quick_start.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=quick_start --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target radiopi.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/radiopi.md))
  (target radiopi.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=radiopi --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target reference-header.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/reference-header.md))
  (target reference-header.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=reference-header --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target release-assets.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/release-assets.md))
  (target release-assets.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=release-assets --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target replay_gain.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/replay_gain.md))
  (target replay_gain.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=replay_gain --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target request_sources.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/request_sources.md))
  (target request_sources.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=request_sources --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target requests.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/requests.md))
  (target requests.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=requests --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target rolling-release.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/rolling-release.md))
  (target rolling-release.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=rolling-release --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target script_loading.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/script_loading.md))
  (target script_loading.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=script_loading --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target seek.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/seek.md))
  (target seek.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=seek --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target server.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/server.md))
  (target server.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=server --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target shoutcast.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/shoutcast.md))
  (target shoutcast.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=shoutcast --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target sources.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/sources.md))
  (target sources.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=sources --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target split-cue.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/split-cue.md))
  (target split-cue.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=split-cue --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target stereotool.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/stereotool.md))
  (target stereotool.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=stereotool --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target stream_content.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/stream_content.md))
  (target stream_content.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=stream_content --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target strings_encoding.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/strings_encoding.md))
  (target strings_encoding.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=strings_encoding --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target video-static.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/video-static.md))
  (target video-static.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=video-static --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target video.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/video.md))
  (target video.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=video --template=template.html -o %{target}))))

(rule
  (alias doc)
  (enabled_if (not %{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target yaml.html)
  (action (run cp %{no_pandoc} %{target})))

(rule
  (alias doc)
  (enabled_if %{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    content/liq/append-silence.liq
    content/liq/beets-amplify.liq
    content/liq/beets-protocol.liq
    content/liq/beets-source.liq
    content/liq/blank-detect.liq
    content/liq/blank-sorry.liq
    content/liq/complete-case.liq
    content/liq/cross.smart.liq
    content/liq/dump-hourly.liq
    content/liq/dump-hourly2.liq
    content/liq/dynamic-source.liq
    content/liq/fallback.liq
    content/liq/ffmpeg-filter-dynamic-volume.liq
    content/liq/ffmpeg-filter-flanger-highpass.liq
    content/liq/ffmpeg-filter-hflip.liq
    content/liq/ffmpeg-filter-hflip2.liq
    content/liq/ffmpeg-filter-parallel-flanger-highpass.liq
    content/liq/ffmpeg-live-switch.liq
    content/liq/ffmpeg-relay-ondemand.liq
    content/liq/ffmpeg-relay.liq
    content/liq/ffmpeg-shared-encoding-rtmp.liq
    content/liq/ffmpeg-shared-encoding.liq
    content/liq/frame-size.liq
    content/liq/harbor-insert-metadata.liq
    content/liq/harbor-metadata.liq
    content/liq/harbor.http.register.liq
    content/liq/harbor.http.response.liq
    content/liq/hls-metadata.liq
    content/liq/hls-mp4.liq
    content/liq/http-input.liq
    content/liq/icy-update.liq
    content/liq/live-switch.liq
    content/liq/medialib.liq
    content/liq/multitrack-add-video-track.liq
    content/liq/multitrack-add-video-track2.liq
    content/liq/multitrack-default-video-track.liq
    content/liq/multitrack.liq
    content/liq/multitrack2.liq
    content/liq/multitrack3.liq
    content/liq/output.file.hls.liq
    content/liq/prometheus-callback.liq
    content/liq/prometheus-settings.liq
    content/liq/radiopi.liq
    content/liq/re-encode.liq
    content/liq/regular.liq
    content/liq/replaygain-metadata.liq
    content/liq/replaygain-playlist.liq
    content/liq/rtmp.liq
    content/liq/samplerate3.liq
    content/liq/scheduling.liq
    content/liq/settings.liq
    content/liq/shoutcast.liq
    content/liq/space_overhead.liq
    content/liq/split-cue.liq
    content/liq/srt-receiver.liq
    content/liq/srt-sender.liq
    content/liq/transcoding.liq
    content/liq/video-anonymizer.liq
    content/liq/video-bluescreen.liq
    content/liq/video-in-video.liq
    content/liq/video-logo.liq
    content/liq/video-osc.liq
    content/liq/video-simple.liq
    content/liq/video-static.liq
    content/liq/video-text.liq
    content/liq/video-transition.liq
    content/liq/video-weather.liq
    content/liq/video-webcam.liq
    (:md content/yaml.md))
  (target yaml.html)
  (action
    (ignore-outputs
      (run pandoc --filter=pandoc-include --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=yaml --template=template.html -o %{target}))))

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/append-silence.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/append-silence.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/beets-amplify.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/beets-amplify.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/beets-protocol.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/beets-protocol.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/beets-source.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/beets-source.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/blank-detect.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/blank-detect.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/blank-sorry.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/blank-sorry.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/complete-case.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/complete-case.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/cross.smart.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/cross.smart.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/dump-hourly.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/dump-hourly.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/dump-hourly2.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/dump-hourly2.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/dynamic-source.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/dynamic-source.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/fallback.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/fallback.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/ffmpeg-filter-dynamic-volume.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/ffmpeg-filter-dynamic-volume.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/ffmpeg-filter-flanger-highpass.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/ffmpeg-filter-flanger-highpass.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/ffmpeg-filter-hflip.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/ffmpeg-filter-hflip.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/ffmpeg-filter-hflip2.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/ffmpeg-filter-hflip2.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/ffmpeg-filter-parallel-flanger-highpass.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/ffmpeg-filter-parallel-flanger-highpass.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/ffmpeg-live-switch.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/ffmpeg-live-switch.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/ffmpeg-relay-ondemand.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/ffmpeg-relay-ondemand.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/ffmpeg-relay.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/ffmpeg-relay.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/ffmpeg-shared-encoding-rtmp.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/ffmpeg-shared-encoding-rtmp.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/ffmpeg-shared-encoding.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/ffmpeg-shared-encoding.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/frame-size.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/frame-size.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/harbor-insert-metadata.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/harbor-insert-metadata.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/harbor-metadata.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/harbor-metadata.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/harbor.http.register.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/harbor.http.register.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/harbor.http.response.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/harbor.http.response.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/hls-metadata.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/hls-metadata.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/hls-mp4.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/hls-mp4.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/http-input.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/http-input.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/icy-update.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/icy-update.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/live-switch.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/live-switch.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/medialib.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/medialib.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/multitrack-add-video-track.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/multitrack-add-video-track.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/multitrack-add-video-track2.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/multitrack-add-video-track2.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/multitrack-default-video-track.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/multitrack-default-video-track.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/multitrack.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/multitrack.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/multitrack2.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/multitrack2.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/multitrack3.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/multitrack3.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/output.file.hls.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/output.file.hls.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/prometheus-callback.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/prometheus-callback.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/prometheus-settings.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/prometheus-settings.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/radiopi.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/radiopi.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/re-encode.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/re-encode.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/regular.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/regular.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/replaygain-metadata.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/replaygain-metadata.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/replaygain-playlist.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/replaygain-playlist.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/rtmp.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/rtmp.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/samplerate3.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/samplerate3.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/scheduling.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/scheduling.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/settings.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/settings.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/shoutcast.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/shoutcast.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/space_overhead.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/space_overhead.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/split-cue.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/split-cue.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/srt-receiver.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/srt-receiver.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/srt-sender.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/srt-sender.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/transcoding.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/transcoding.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/video-anonymizer.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/video-anonymizer.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/video-bluescreen.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/video-bluescreen.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/video-in-video.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/video-in-video.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/video-logo.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/video-logo.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/video-osc.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/video-osc.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/video-simple.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/video-simple.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/video-static.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/video-static.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/video-text.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/video-text.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/video-transition.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/video-transition.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/video-weather.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/video-weather.liq))
)

(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    ../src/libs/audio.liq
    ../src/libs/clock.liq
    ../src/libs/error.liq
    ../src/libs/fades.liq
    ../src/libs/ffmpeg.liq
    ../src/libs/file.liq
    ../src/libs/getter.liq
    ../src/libs/hls.liq
    ../src/libs/http.liq
    ../src/libs/http_codes.liq
    ../src/libs/icecast.liq
    ../src/libs/io.liq
    ../src/libs/liquidsoap.liq
    ../src/libs/list.liq
    ../src/libs/log.liq
    ../src/libs/math.liq
    ../src/libs/medialib.liq
    ../src/libs/metadata.liq
    ../src/libs/null.liq
    ../src/libs/playlist.liq
    ../src/libs/predicate.liq
    ../src/libs/process.liq
    ../src/libs/profiler.liq
    ../src/libs/protocols.liq
    ../src/libs/ref.liq
    ../src/libs/replaygain.liq
    ../src/libs/request.liq
    ../src/libs/resolvers.liq
    ../src/libs/runtime.liq
    ../src/libs/server.liq
    ../src/libs/settings.liq
    ../src/libs/socket.liq
    ../src/libs/source.liq
    ../src/libs/stdlib.liq
    ../src/libs/string.liq
    ../src/libs/switches.liq
    ../src/libs/testing.liq
    ../src/libs/thread.liq
    ../src/libs/tracks.liq
    ../src/libs/utils.liq
    ../src/libs/video.liq
    (:stdlib ../src/libs/stdlib.liq)
    (:test_liq content/liq/video-webcam.liq)
  )
  (action (run %{bin:liquidsoap} --no-stdlib %{stdlib} --check --no-fallible-check content/liq/video-webcam.liq))
)

(install
  (section doc)
  (package liquidsoap)
  (files
    (beets.html as html/beets.html)
    (blank.html as html/blank.html)
    (book.html as html/book.html)
    (build.html as html/build.html)
    (clocks.html as html/clocks.html)
    (complete_case.html as html/complete_case.html)
    (cookbook.html as html/cookbook.html)
    (crossfade.html as html/crossfade.html)
    (custom-path.html as html/custom-path.html)
    (documentation.html as html/documentation.html)
    (dynamic_sources.html as html/dynamic_sources.html)
    (encoding_formats.html as html/encoding_formats.html)
    (external_decoders.html as html/external_decoders.html)
    (external_encoders.html as html/external_encoders.html)
    (external_streams.html as html/external_streams.html)
    (faq.html as html/faq.html)
    (ffmpeg.html as html/ffmpeg.html)
    (ffmpeg_cookbook.html as html/ffmpeg_cookbook.html)
    (ffmpeg_encoder.html as html/ffmpeg_encoder.html)
    (ffmpeg_filters.html as html/ffmpeg_filters.html)
    (flows_devel.html as html/flows_devel.html)
    (frequence3.html as html/frequence3.html)
    (geekradio.html as html/geekradio.html)
    (gstreamer_encoder.html as html/gstreamer_encoder.html)
    (harbor.html as html/harbor.html)
    (harbor_http.html as html/harbor_http.html)
    (help.html as html/help.html)
    (hls_output.html as html/hls_output.html)
    (http_input.html as html/http_input.html)
    (icy_metadata.html as html/icy_metadata.html)
    (in_production.html as html/in_production.html)
    (index.html as html/index.html)
    (install.html as html/install.html)
    (json.html as html/json.html)
    (ladspa.html as html/ladspa.html)
    (language.html as html/language.html)
    (memory.html as html/memory.html)
    (metadata.html as html/metadata.html)
    (migrating.html as html/migrating.html)
    (multitrack.html as html/multitrack.html)
    (on2.html as html/on2.html)
    (on2_part1.html as html/on2_part1.html)
    (on2_part2.html as html/on2_part2.html)
    (on2_part3.html as html/on2_part3.html)
    (orig/css/homepage.css as html/css/homepage.css)
    (orig/css/style.css as html/css/style.css)
    (orig/fosdem2020/clock.png as html/fosdem2020/clock.png)
    (orig/fosdem2020/index.html as html/fosdem2020/index.html)
    (orig/fosdem2020/logo.png as html/fosdem2020/logo.png)
    (orig/fosdem2020/radio.gif as html/fosdem2020/radio.gif)
    (orig/fosdem2020/remark.js as html/fosdem2020/remark.js)
    (orig/images/basic-radio-graph.png as html/images/basic-radio-graph.png)
    (orig/images/design/background.png as html/images/design/background.png)
    (orig/images/design/background_page.png as html/images/design/background_page.png)
    (orig/images/design/logo.png as html/images/design/logo.png)
    (orig/images/grab.png as html/images/grab.png)
    (orig/images/graph_clocks.png as html/images/graph_clocks.png)
    (orig/images/liqgraph.png as html/images/liqgraph.png)
    (orig/images/schema-webradio-inkscape.png as html/images/schema-webradio-inkscape.png)
    (orig/images/stream.png as html/images/stream.png)
    (orig/images/tabs/tab_API.png as html/images/tabs/tab_API.png)
    (orig/images/tabs/tab_about.png as html/images/tabs/tab_about.png)
    (orig/images/tabs/tab_developers.png as html/images/tabs/tab_developers.png)
    (orig/images/tabs/tab_docs.png as html/images/tabs/tab_docs.png)
    (orig/images/tabs/tab_snippets.png as html/images/tabs/tab_snippets.png)
    (phases.html as html/phases.html)
    (playlist_parsers.html as html/playlist_parsers.html)
    (presentations.html as html/presentations.html)
    (profiling.html as html/profiling.html)
    (prometheus.html as html/prometheus.html)
    (protocols-presentation.html as html/protocols-presentation.html)
    (protocols.html as html/protocols.html)
    (publications.html as html/publications.html)
    (quick_start.html as html/quick_start.html)
    (radiopi.html as html/radiopi.html)
    (reference-deprecated.html as html/reference-deprecated.html)
    (reference-extras.html as html/reference-extras.html)
    (reference-header.html as html/reference-header.html)
    (reference.html as html/reference.html)
    (release-assets.html as html/release-assets.html)
    (replay_gain.html as html/replay_gain.html)
    (request_sources.html as html/request_sources.html)
    (requests.html as html/requests.html)
    (rolling-release.html as html/rolling-release.html)
    (script_loading.html as html/script_loading.html)
    (seek.html as html/seek.html)
    (server.html as html/server.html)
    (settings.html as html/settings.html)
    (shoutcast.html as html/shoutcast.html)
    (sources.html as html/sources.html)
    (split-cue.html as html/split-cue.html)
    (stereotool.html as html/stereotool.html)
    (stream_content.html as html/stream_content.html)
    (strings_encoding.html as html/strings_encoding.html)
    (video-static.html as html/video-static.html)
    (video.html as html/video.html)
    (yaml.html as html/yaml.html)
  )
)
