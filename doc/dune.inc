(rule
 (alias doc)
 (package liquidsoap)
 (deps
  (source_tree ../src/libs))
 (target protocols.md)
 (action
  (with-stdout-to
   protocols.md
   (setenv
    PAGER
    none
    (run %{bin:liquidsoap} --list-protocols-md --disable-deprecated)))))

(rule
 (alias doc)
 (package liquidsoap)
 (deps
  (:header content/reference-header.md)
  (source_tree ../src/libs))
 (target reference.md)
 (action
  (with-stdout-to
   reference.md
   (progn
    (cat %{header})
    (echo "\n")
    (setenv
     PAGER
     none
     (run %{bin:liquidsoap} --list-functions-md --disable-deprecated))))))

(rule
 (alias doc)
 (package liquidsoap)
 (deps
  (:header content/reference-header.md)
  (source_tree ../src/libs))
 (target reference-extras.md)
 (action
  (with-stdout-to
   reference-extras.md
   (progn
    (cat %{header})
    (echo "\n")
    (setenv
     PAGER
     none
     (run
      %{bin:liquidsoap}
      --no-external-plugins
      --list-extra-functions-md
      --disable-deprecated))))))

(rule
 (alias doc)
 (package liquidsoap)
 (deps
  (:header content/reference-header.md)
  (source_tree ../src/libs))
 (target reference-deprecated.md)
 (action
  (with-stdout-to
   reference-deprecated.md
   (progn
    (cat %{header})
    (echo "\n")
    (setenv
     PAGER
     none
     (run
      %{bin:liquidsoap}
      --list-deprecated-functions-md
      --enable-deprecated))))))

(rule
 (alias doc)
 (package liquidsoap)
 (deps
  (source_tree ../src/libs))
 (target settings.md)
 (action
  (with-stdout-to
   settings.md
   (setenv
    PAGER
    none
    (run %{bin:liquidsoap} --list-settings --disable-deprecated)))))

(rule
 (alias test-doc-append-silence)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/append-silence.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/append-silence.liq)))

(rule
 (alias test-doc-archive-cleaner)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/archive-cleaner.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/archive-cleaner.liq)))

(rule
 (alias test-doc-basic-radio)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/basic-radio.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/basic-radio.liq)))

(rule
 (alias test-doc-beets-amplify)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/beets-amplify.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/beets-amplify.liq)))

(rule
 (alias test-doc-beets-protocol-short)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/beets-protocol-short.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/beets-protocol-short.liq)))

(rule
 (alias test-doc-beets-protocol)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/beets-protocol.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/beets-protocol.liq)))

(rule
 (alias test-doc-beets-source)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/beets-source.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/beets-source.liq)))

(rule
 (alias test-doc-blank-detect)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/blank-detect.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/blank-detect.liq)))

(rule
 (alias test-doc-blank-sorry)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/blank-sorry.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/blank-sorry.liq)))

(rule
 (alias test-doc-callback-collect)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/callback-collect.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/callback-collect.liq)))

(rule
 (alias test-doc-callback-release-per-call)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/callback-release-per-call.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/callback-release-per-call.liq)))

(rule
 (alias test-doc-callback-release)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/callback-release.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/callback-release.liq)))

(rule
 (alias test-doc-clock-alsa-file)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/clock-alsa-file.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/clock-alsa-file.liq)))

(rule
 (alias test-doc-clock-alsa-pulseaudio-buffer)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/clock-alsa-pulseaudio-buffer.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/clock-alsa-pulseaudio-buffer.liq)))

(rule
 (alias test-doc-clock-alsa-pulseaudio-conflict)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/clock-alsa-pulseaudio-conflict.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/clock-alsa-pulseaudio-conflict.liq)))

(rule
 (alias test-doc-clock-alsa-pulseaudio-self-sync)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/clock-alsa-pulseaudio-self-sync.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/clock-alsa-pulseaudio-self-sync.liq)))

(rule
 (alias test-doc-clock-decoupling-conflict)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/clock-decoupling-conflict.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/clock-decoupling-conflict.liq)))

(rule
 (alias test-doc-clock-decoupling)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/clock-decoupling.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/clock-decoupling.liq)))

(rule
 (alias test-doc-clock-inspect)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/clock-inspect.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/clock-inspect.liq)))

(rule
 (alias test-doc-clock-log-delay)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/clock-log-delay.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/clock-log-delay.liq)))

(rule
 (alias test-doc-clock-parallel-sequential)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/clock-parallel-sequential.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/clock-parallel-sequential.liq)))

(rule
 (alias test-doc-clock-parallel)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/clock-parallel.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/clock-parallel.liq)))

(rule
 (alias test-doc-clock-sine-file)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/clock-sine-file.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/clock-sine-file.liq)))

(rule
 (alias test-doc-clock-srt-alsa-fallback)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/clock-srt-alsa-fallback.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/clock-srt-alsa-fallback.liq)))

(rule
 (alias test-doc-clock-srt-crossfade-buffer)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/clock-srt-crossfade-buffer.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/clock-srt-crossfade-buffer.liq)))

(rule
 (alias test-doc-clock-srt-crossfade-conflict)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/clock-srt-crossfade-conflict.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/clock-srt-crossfade-conflict.liq)))

(rule
 (alias test-doc-complete-case)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/complete-case.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/complete-case.liq)))

(rule
 (alias test-doc-cron_add)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/cron_add.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/cron_add.liq)))

(rule
 (alias test-doc-cron_id)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/cron_id.liq))
 (action
  (run %{bin:liquidsoap} --check --no-fallible-check content/liq/cron_id.liq)))

(rule
 (alias test-doc-cron_id_arg)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/cron_id_arg.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/cron_id_arg.liq)))

(rule
 (alias test-doc-cron_remove)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/cron_remove.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/cron_remove.liq)))

(rule
 (alias test-doc-cross.custom)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/cross.custom.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/cross.custom.liq)))

(rule
 (alias test-doc-crossfade)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/crossfade.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/crossfade.liq)))

(rule
 (alias test-doc-decoder-openmpt)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/decoder-openmpt.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/decoder-openmpt.liq)))

(rule
 (alias test-doc-dump-hourly)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/dump-hourly.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/dump-hourly.liq)))

(rule
 (alias test-doc-dump-hourly2)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/dump-hourly2.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/dump-hourly2.liq)))

(rule
 (alias test-doc-dynamic-source)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/dynamic-source.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/dynamic-source.liq)))

(rule
 (alias test-doc-enc-fdkaac)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/enc-fdkaac.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/enc-fdkaac.liq)))

(rule
 (alias test-doc-enc-ffmpeg)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/enc-ffmpeg.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/enc-ffmpeg.liq)))

(rule
 (alias test-doc-enc-flac)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/enc-flac.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/enc-flac.liq)))

(rule
 (alias test-doc-enc-mp3)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/enc-mp3.liq))
 (action
  (run %{bin:liquidsoap} --check --no-fallible-check content/liq/enc-mp3.liq)))

(rule
 (alias test-doc-enc-opus)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/enc-opus.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/enc-opus.liq)))

(rule
 (alias test-doc-enc-vorbis)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/enc-vorbis.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/enc-vorbis.liq)))

(rule
 (alias test-doc-enc-wav)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/enc-wav.liq))
 (action
  (run %{bin:liquidsoap} --check --no-fallible-check content/liq/enc-wav.liq)))

(rule
 (alias test-doc-external-output.file)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/external-output.file.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/external-output.file.liq)))

(rule
 (alias test-doc-fallback)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/fallback.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/fallback.liq)))

(rule
 (alias test-doc-ffmpeg-filter-dynamic-volume)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/ffmpeg-filter-dynamic-volume.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/ffmpeg-filter-dynamic-volume.liq)))

(rule
 (alias test-doc-ffmpeg-filter-flanger-highpass)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/ffmpeg-filter-flanger-highpass.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/ffmpeg-filter-flanger-highpass.liq)))

(rule
 (alias test-doc-ffmpeg-filter-hflip)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/ffmpeg-filter-hflip.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/ffmpeg-filter-hflip.liq)))

(rule
 (alias test-doc-ffmpeg-filter-hflip2)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/ffmpeg-filter-hflip2.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/ffmpeg-filter-hflip2.liq)))

(rule
 (alias test-doc-ffmpeg-filter-parallel-flanger-highpass)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/ffmpeg-filter-parallel-flanger-highpass.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/ffmpeg-filter-parallel-flanger-highpass.liq)))

(rule
 (alias test-doc-ffmpeg-live-switch)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/ffmpeg-live-switch.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/ffmpeg-live-switch.liq)))

(rule
 (alias test-doc-ffmpeg-relay-ondemand)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/ffmpeg-relay-ondemand.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/ffmpeg-relay-ondemand.liq)))

(rule
 (alias test-doc-ffmpeg-relay)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/ffmpeg-relay.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/ffmpeg-relay.liq)))

(rule
 (alias test-doc-ffmpeg-shared-encoding-rtmp)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/ffmpeg-shared-encoding-rtmp.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/ffmpeg-shared-encoding-rtmp.liq)))

(rule
 (alias test-doc-ffmpeg-shared-encoding)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/ffmpeg-shared-encoding.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/ffmpeg-shared-encoding.liq)))

(rule
 (alias test-doc-fixed-time1)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/fixed-time1.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/fixed-time1.liq)))

(rule
 (alias test-doc-fixed-time2)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/fixed-time2.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/fixed-time2.liq)))

(rule
 (alias test-doc-frame-size)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/frame-size.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/frame-size.liq)))

(rule
 (alias test-doc-harbor-auth)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/harbor-auth.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/harbor-auth.liq)))

(rule
 (alias test-doc-harbor-dynamic-basic)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/harbor-dynamic-basic.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/harbor-dynamic-basic.liq)))

(rule
 (alias test-doc-harbor-dynamic-filter)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/harbor-dynamic-filter.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/harbor-dynamic-filter.liq)))

(rule
 (alias test-doc-harbor-dynamic-regexp)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/harbor-dynamic-regexp.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/harbor-dynamic-regexp.liq)))

(rule
 (alias test-doc-harbor-dynamic-routing)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/harbor-dynamic-routing.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/harbor-dynamic-routing.liq)))

(rule
 (alias test-doc-harbor-dynamic)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/harbor-dynamic.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/harbor-dynamic.liq)))

(rule
 (alias test-doc-harbor-insert-metadata)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/harbor-insert-metadata.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/harbor-insert-metadata.liq)))

(rule
 (alias test-doc-harbor-metadata)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/harbor-metadata.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/harbor-metadata.liq)))

(rule
 (alias test-doc-harbor-redirect)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/harbor-redirect.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/harbor-redirect.liq)))

(rule
 (alias test-doc-harbor-simple)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/harbor-simple.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/harbor-simple.liq)))

(rule
 (alias test-doc-harbor-usage)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/harbor-usage.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/harbor-usage.liq)))

(rule
 (alias test-doc-harbor.http.register)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/harbor.http.register.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/harbor.http.register.liq)))

(rule
 (alias test-doc-harbor.http.response)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/harbor.http.response.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/harbor.http.response.liq)))

(rule
 (alias test-doc-hls-metadata)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/hls-metadata.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/hls-metadata.liq)))

(rule
 (alias test-doc-http-input)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/http-input.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/http-input.liq)))

(rule
 (alias test-doc-icecast-server-basic)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/icecast-server-basic.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/icecast-server-basic.liq)))

(rule
 (alias test-doc-icecast-server-complete)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/icecast-server-complete.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/icecast-server-complete.liq)))

(rule
 (alias test-doc-icecast-server-config)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/icecast-server-config.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/icecast-server-config.liq)))

(rule
 (alias test-doc-icecast-server-dedicated-encoder)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/icecast-server-dedicated-encoder.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/icecast-server-dedicated-encoder.liq)))

(rule
 (alias test-doc-icecast-server-serve-auth)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/icecast-server-serve-auth.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/icecast-server-serve-auth.liq)))

(rule
 (alias test-doc-icecast-server-serve-json)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/icecast-server-serve-json.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/icecast-server-serve-json.liq)))

(rule
 (alias test-doc-icy-update)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/icy-update.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/icy-update.liq)))

(rule
 (alias test-doc-implicit-float-ok)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/implicit-float-ok.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/implicit-float-ok.liq)))

(rule
 (alias test-doc-input.mplayer)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/input.mplayer.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/input.mplayer.liq)))

(rule
 (alias test-doc-jack-buffer)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/jack-buffer.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/jack-buffer.liq)))

(rule
 (alias test-doc-jack-connect)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/jack-connect.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/jack-connect.liq)))

(rule
 (alias test-doc-jack-low-latency)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/jack-low-latency.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/jack-low-latency.liq)))

(rule
 (alias test-doc-jack-passthrough)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/jack-passthrough.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/jack-passthrough.liq)))

(rule
 (alias test-doc-jack-record)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/jack-record.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/jack-record.liq)))

(rule
 (alias test-doc-jack-server-ports)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/jack-server-ports.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/jack-server-ports.liq)))

(rule
 (alias test-doc-jack-server)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/jack-server.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/jack-server.liq)))

(rule
 (alias test-doc-jingle-hour)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/jingle-hour.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/jingle-hour.liq)))

(rule
 (alias test-doc-json-ex)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/json-ex.liq))
 (action
  (run %{bin:liquidsoap} --check --no-fallible-check content/liq/json-ex.liq)))

(rule
 (alias test-doc-json-stringify)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/json-stringify.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/json-stringify.liq)))

(rule
 (alias test-doc-json1)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/json1.liq))
 (action
  (run %{bin:liquidsoap} --check --no-fallible-check content/liq/json1.liq)))

(rule
 (alias test-doc-labeled_arguments)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/labeled_arguments.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/labeled_arguments.liq)))

(rule
 (alias test-doc-live-switch)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/live-switch.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/live-switch.liq)))

(rule
 (alias test-doc-loudness-correction)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/loudness-correction.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/loudness-correction.liq)))

(rule
 (alias test-doc-medialib-predicate)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/medialib-predicate.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/medialib-predicate.liq)))

(rule
 (alias test-doc-medialib)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/medialib.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/medialib.liq)))

(rule
 (alias test-doc-medialib.sqlite)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/medialib.sqlite.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/medialib.sqlite.liq)))

(rule
 (alias test-doc-multitrack-add-video-track2)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/multitrack-add-video-track2.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/multitrack-add-video-track2.liq)))

(rule
 (alias test-doc-multitrack-default-video-track)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/multitrack-default-video-track.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/multitrack-default-video-track.liq)))

(rule
 (alias test-doc-multitrack2)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/multitrack2.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/multitrack2.liq)))

(rule
 (alias test-doc-output.file.hls)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/output.file.hls.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/output.file.hls.liq)))

(rule
 (alias test-doc-playlists)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/playlists.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/playlists.liq)))

(rule
 (alias test-doc-prometheus-callback)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/prometheus-callback.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/prometheus-callback.liq)))

(rule
 (alias test-doc-prometheus-settings)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/prometheus-settings.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/prometheus-settings.liq)))

(rule
 (alias test-doc-radiopi)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/radiopi.liq))
 (action
  (run %{bin:liquidsoap} --check --no-fallible-check content/liq/radiopi.liq)))

(rule
 (alias test-doc-raw-string-delimited)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/raw-string-delimited.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/raw-string-delimited.liq)))

(rule
 (alias test-doc-raw-string)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/raw-string.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/raw-string.liq)))

(rule
 (alias test-doc-re-encode)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/re-encode.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/re-encode.liq)))

(rule
 (alias test-doc-regular)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/regular.liq))
 (action
  (run %{bin:liquidsoap} --check --no-fallible-check content/liq/regular.liq)))

(rule
 (alias test-doc-replaygain-playlist)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/replaygain-playlist.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/replaygain-playlist.liq)))

(rule
 (alias test-doc-request.dynamic)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/request.dynamic.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/request.dynamic.liq)))

(rule
 (alias test-doc-rtmp)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/rtmp.liq))
 (action
  (run %{bin:liquidsoap} --check --no-fallible-check content/liq/rtmp.liq)))

(rule
 (alias test-doc-samplerate3)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/samplerate3.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/samplerate3.liq)))

(rule
 (alias test-doc-scheduling)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/scheduling.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/scheduling.liq)))

(rule
 (alias test-doc-scheduling_9am)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/scheduling_9am.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/scheduling_9am.liq)))

(rule
 (alias test-doc-scheduling_queue)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/scheduling_queue.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/scheduling_queue.liq)))

(rule
 (alias test-doc-scheduling_queue_midnight)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/scheduling_queue_midnight.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/scheduling_queue_midnight.liq)))

(rule
 (alias test-doc-scheduling_simple)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/scheduling_simple.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/scheduling_simple.liq)))

(rule
 (alias test-doc-seek-telnet)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/seek-telnet.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/seek-telnet.liq)))

(rule
 (alias test-doc-server-harbor)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/server-harbor.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/server-harbor.liq)))

(rule
 (alias test-doc-server-interactive-float)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/server-interactive-float.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/server-interactive-float.liq)))

(rule
 (alias test-doc-server-interactive-harbor)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/server-interactive-harbor.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/server-interactive-harbor.liq)))

(rule
 (alias test-doc-server-interactive-persistent)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/server-interactive-persistent.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/server-interactive-persistent.liq)))

(rule
 (alias test-doc-server-socket)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/server-socket.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/server-socket.liq)))

(rule
 (alias test-doc-server-switch)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/server-switch.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/server-switch.liq)))

(rule
 (alias test-doc-settings)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/settings.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/settings.liq)))

(rule
 (alias test-doc-shoutcast)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/shoutcast.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/shoutcast.liq)))

(rule
 (alias test-doc-single)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/single.liq))
 (action
  (run %{bin:liquidsoap} --check --no-fallible-check content/liq/single.liq)))

(rule
 (alias test-doc-source-cue)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/source-cue.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/source-cue.liq)))

(rule
 (alias test-doc-space_overhead)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/space_overhead.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/space_overhead.liq)))

(rule
 (alias test-doc-split-cue)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/split-cue.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/split-cue.liq)))

(rule
 (alias test-doc-sqlite)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/sqlite.liq))
 (action
  (run %{bin:liquidsoap} --check --no-fallible-check content/liq/sqlite.liq)))

(rule
 (alias test-doc-srt-receiver)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/srt-receiver.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/srt-receiver.liq)))

(rule
 (alias test-doc-srt-sender)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/srt-sender.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/srt-sender.liq)))

(rule
 (alias test-doc-switch-show)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/switch-show.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/switch-show.liq)))

(rule
 (alias test-doc-transcoding)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/transcoding.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/transcoding.liq)))

(rule
 (alias test-doc-video-anonymizer)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/video-anonymizer.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/video-anonymizer.liq)))

(rule
 (alias test-doc-video-bluescreen)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/video-bluescreen.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/video-bluescreen.liq)))

(rule
 (alias test-doc-video-canvas-example)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/video-canvas-example.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/video-canvas-example.liq)))

(rule
 (alias test-doc-video-default-canvas)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/video-default-canvas.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/video-default-canvas.liq)))

(rule
 (alias test-doc-video-in-video)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/video-in-video.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/video-in-video.liq)))

(rule
 (alias test-doc-video-logo)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/video-logo.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/video-logo.liq)))

(rule
 (alias test-doc-video-osc)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/video-osc.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/video-osc.liq)))

(rule
 (alias test-doc-video-simple)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/video-simple.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/video-simple.liq)))

(rule
 (alias test-doc-video-static)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/video-static.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/video-static.liq)))

(rule
 (alias test-doc-video-text)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/video-text.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/video-text.liq)))

(rule
 (alias test-doc-video-transition)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/video-transition.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/video-transition.liq)))

(rule
 (alias test-doc-video-weather)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/video-weather.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/video-weather.liq)))

(rule
 (alias test-doc-video-webcam)
 (package liquidsoap)
 (deps
  (alias_rec ../install)
  (source_tree ../src/libs)
  (:test_liq content/liq/video-webcam.liq))
 (action
  (run
   %{bin:liquidsoap}
   --check
   --no-fallible-check
   content/liq/video-webcam.liq)))

(alias
 (name doctest)
 (deps
  (alias test-doc-append-silence)
  (alias test-doc-archive-cleaner)
  (alias test-doc-basic-radio)
  (alias test-doc-beets-amplify)
  (alias test-doc-beets-protocol)
  (alias test-doc-beets-protocol-short)
  (alias test-doc-beets-source)
  (alias test-doc-blank-detect)
  (alias test-doc-blank-sorry)
  (alias test-doc-callback-collect)
  (alias test-doc-callback-release)
  (alias test-doc-callback-release-per-call)
  (alias test-doc-clock-alsa-file)
  (alias test-doc-clock-alsa-pulseaudio-buffer)
  (alias test-doc-clock-alsa-pulseaudio-conflict)
  (alias test-doc-clock-alsa-pulseaudio-self-sync)
  (alias test-doc-clock-decoupling)
  (alias test-doc-clock-decoupling-conflict)
  (alias test-doc-clock-inspect)
  (alias test-doc-clock-log-delay)
  (alias test-doc-clock-parallel)
  (alias test-doc-clock-parallel-sequential)
  (alias test-doc-clock-sine-file)
  (alias test-doc-clock-srt-alsa-fallback)
  (alias test-doc-clock-srt-crossfade-buffer)
  (alias test-doc-clock-srt-crossfade-conflict)
  (alias test-doc-complete-case)
  (alias test-doc-cron_add)
  (alias test-doc-cron_id)
  (alias test-doc-cron_id_arg)
  (alias test-doc-cron_remove)
  (alias test-doc-cross.custom)
  (alias test-doc-crossfade)
  (alias test-doc-decoder-openmpt)
  (alias test-doc-dump-hourly)
  (alias test-doc-dump-hourly2)
  (alias test-doc-dynamic-source)
  (alias test-doc-enc-fdkaac)
  (alias test-doc-enc-ffmpeg)
  (alias test-doc-enc-flac)
  (alias test-doc-enc-mp3)
  (alias test-doc-enc-opus)
  (alias test-doc-enc-vorbis)
  (alias test-doc-enc-wav)
  (alias test-doc-external-output.file)
  (alias test-doc-fallback)
  (alias test-doc-ffmpeg-filter-dynamic-volume)
  (alias test-doc-ffmpeg-filter-flanger-highpass)
  (alias test-doc-ffmpeg-filter-hflip)
  (alias test-doc-ffmpeg-filter-hflip2)
  (alias test-doc-ffmpeg-filter-parallel-flanger-highpass)
  (alias test-doc-ffmpeg-live-switch)
  (alias test-doc-ffmpeg-relay)
  (alias test-doc-ffmpeg-relay-ondemand)
  (alias test-doc-ffmpeg-shared-encoding)
  (alias test-doc-ffmpeg-shared-encoding-rtmp)
  (alias test-doc-fixed-time1)
  (alias test-doc-fixed-time2)
  (alias test-doc-frame-size)
  (alias test-doc-harbor-auth)
  (alias test-doc-harbor-dynamic)
  (alias test-doc-harbor-dynamic-basic)
  (alias test-doc-harbor-dynamic-filter)
  (alias test-doc-harbor-dynamic-regexp)
  (alias test-doc-harbor-dynamic-routing)
  (alias test-doc-harbor-insert-metadata)
  (alias test-doc-harbor-metadata)
  (alias test-doc-harbor-redirect)
  (alias test-doc-harbor-simple)
  (alias test-doc-harbor-usage)
  (alias test-doc-harbor.http.register)
  (alias test-doc-harbor.http.response)
  (alias test-doc-hls-metadata)
  (alias test-doc-http-input)
  (alias test-doc-icecast-server-basic)
  (alias test-doc-icecast-server-complete)
  (alias test-doc-icecast-server-config)
  (alias test-doc-icecast-server-dedicated-encoder)
  (alias test-doc-icecast-server-serve-auth)
  (alias test-doc-icecast-server-serve-json)
  (alias test-doc-icy-update)
  (alias test-doc-implicit-float-ok)
  (alias test-doc-input.mplayer)
  (alias test-doc-jack-buffer)
  (alias test-doc-jack-connect)
  (alias test-doc-jack-low-latency)
  (alias test-doc-jack-passthrough)
  (alias test-doc-jack-record)
  (alias test-doc-jack-server)
  (alias test-doc-jack-server-ports)
  (alias test-doc-jingle-hour)
  (alias test-doc-json-ex)
  (alias test-doc-json-stringify)
  (alias test-doc-json1)
  (alias test-doc-labeled_arguments)
  (alias test-doc-live-switch)
  (alias test-doc-loudness-correction)
  (alias test-doc-medialib)
  (alias test-doc-medialib-predicate)
  (alias test-doc-medialib.sqlite)
  (alias test-doc-multitrack-add-video-track2)
  (alias test-doc-multitrack-default-video-track)
  (alias test-doc-multitrack2)
  (alias test-doc-output.file.hls)
  (alias test-doc-playlists)
  (alias test-doc-prometheus-callback)
  (alias test-doc-prometheus-settings)
  (alias test-doc-radiopi)
  (alias test-doc-raw-string)
  (alias test-doc-raw-string-delimited)
  (alias test-doc-re-encode)
  (alias test-doc-regular)
  (alias test-doc-replaygain-playlist)
  (alias test-doc-request.dynamic)
  (alias test-doc-rtmp)
  (alias test-doc-samplerate3)
  (alias test-doc-scheduling)
  (alias test-doc-scheduling_9am)
  (alias test-doc-scheduling_queue)
  (alias test-doc-scheduling_queue_midnight)
  (alias test-doc-scheduling_simple)
  (alias test-doc-seek-telnet)
  (alias test-doc-server-harbor)
  (alias test-doc-server-interactive-float)
  (alias test-doc-server-interactive-harbor)
  (alias test-doc-server-interactive-persistent)
  (alias test-doc-server-socket)
  (alias test-doc-server-switch)
  (alias test-doc-settings)
  (alias test-doc-shoutcast)
  (alias test-doc-single)
  (alias test-doc-source-cue)
  (alias test-doc-space_overhead)
  (alias test-doc-split-cue)
  (alias test-doc-sqlite)
  (alias test-doc-srt-receiver)
  (alias test-doc-srt-sender)
  (alias test-doc-switch-show)
  (alias test-doc-transcoding)
  (alias test-doc-video-anonymizer)
  (alias test-doc-video-bluescreen)
  (alias test-doc-video-canvas-example)
  (alias test-doc-video-default-canvas)
  (alias test-doc-video-in-video)
  (alias test-doc-video-logo)
  (alias test-doc-video-osc)
  (alias test-doc-video-simple)
  (alias test-doc-video-static)
  (alias test-doc-video-text)
  (alias test-doc-video-transition)
  (alias test-doc-video-weather)
  (alias test-doc-video-webcam)))
