
(rule
  (alias doc)
  (deps
    
    (:liquidsoap ../src/bin/liquidsoap.exe))
  (target protocols.md)
  (action
    (with-stdout-to protocols.md
      
      (setenv PAGER none
        (run %{liquidsoap} --list-protocols-md)))))
      

(rule
  (alias doc)
  (deps
    (:header content/reference-header.md)
    (:liquidsoap ../src/bin/liquidsoap.exe))
  (target reference.md)
  (action
    (with-stdout-to reference.md
      (progn (cat %{header})
      (setenv PAGER none
        (run %{liquidsoap} --list-functions-md)))))
      )

(rule
  (alias doc)
  (deps
    (:header content/reference-header.md)
    (:liquidsoap ../src/bin/liquidsoap.exe))
  (target reference-extras.md)
  (action
    (with-stdout-to reference-extras.md
      (progn (cat %{header})
      (setenv PAGER none
        (run %{liquidsoap} --list-extra-functions-md)))))
      )

(rule
  (alias doc)
  (deps
    
    (:liquidsoap ../src/bin/liquidsoap.exe))
  (target settings.md)
  (action
    (with-stdout-to settings.md
      
      (setenv PAGER none
        (run %{liquidsoap} --list-settings)))))
      

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md protocols.md))
  (target protocols.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=protocols --template=template.html -o protocols.html || cp no-pandoc protocols.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md reference.md))
  (target reference.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=reference --template=template.html -o reference.html || cp no-pandoc reference.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md reference-extras.md))
  (target reference-extras.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=reference-extras --template=template.html -o reference-extras.html || cp no-pandoc reference-extras.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md settings.md))
  (target settings.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=settings --template=template.html -o settings.html || cp no-pandoc settings.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/reference-header.md))
  (target reference-header.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=reference-header --template=template.html -o reference-header.html || cp no-pandoc reference-header.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/language.md))
  (target language.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=language --template=template.html -o language.html || cp no-pandoc language.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/geekradio.md))
  (target geekradio.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=geekradio --template=template.html -o geekradio.html || cp no-pandoc geekradio.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/on2_part3.md))
  (target on2_part3.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=on2_part3 --template=template.html -o on2_part3.html || cp no-pandoc on2_part3.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/prometheus.md))
  (target prometheus.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=prometheus --template=template.html -o prometheus.html || cp no-pandoc prometheus.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/hls_output.md))
  (target hls_output.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=hls_output --template=template.html -o hls_output.html || cp no-pandoc hls_output.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/book.md))
  (target book.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=book --template=template.html -o book.html || cp no-pandoc book.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/http_input.md))
  (target http_input.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=http_input --template=template.html -o http_input.html || cp no-pandoc http_input.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/gstreamer_encoder.md))
  (target gstreamer_encoder.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=gstreamer_encoder --template=template.html -o gstreamer_encoder.html || cp no-pandoc gstreamer_encoder.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/phases.md))
  (target phases.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=phases --template=template.html -o phases.html || cp no-pandoc phases.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/crossfade.md))
  (target crossfade.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=crossfade --template=template.html -o crossfade.html || cp no-pandoc crossfade.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/custom-path.md))
  (target custom-path.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=custom-path --template=template.html -o custom-path.html || cp no-pandoc custom-path.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/on2_part2.md))
  (target on2_part2.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=on2_part2 --template=template.html -o on2_part2.html || cp no-pandoc on2_part2.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/sources.md))
  (target sources.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=sources --template=template.html -o sources.html || cp no-pandoc sources.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/blank.md))
  (target blank.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=blank --template=template.html -o blank.html || cp no-pandoc blank.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/flows_devel.md))
  (target flows_devel.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=flows_devel --template=template.html -o flows_devel.html || cp no-pandoc flows_devel.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/rolling-release.md))
  (target rolling-release.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=rolling-release --template=template.html -o rolling-release.html || cp no-pandoc rolling-release.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/clocks.md))
  (target clocks.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=clocks --template=template.html -o clocks.html || cp no-pandoc clocks.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/script_loading.md))
  (target script_loading.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=script_loading --template=template.html -o script_loading.html || cp no-pandoc script_loading.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/video.md))
  (target video.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=video --template=template.html -o video.html || cp no-pandoc video.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/json.md))
  (target json.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=json --template=template.html -o json.html || cp no-pandoc json.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/replay_gain.md))
  (target replay_gain.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=replay_gain --template=template.html -o replay_gain.html || cp no-pandoc replay_gain.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/complete_case.md))
  (target complete_case.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=complete_case --template=template.html -o complete_case.html || cp no-pandoc complete_case.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/dynamic_sources.md))
  (target dynamic_sources.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=dynamic_sources --template=template.html -o dynamic_sources.html || cp no-pandoc dynamic_sources.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/seek.md))
  (target seek.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=seek --template=template.html -o seek.html || cp no-pandoc seek.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/playlist_parsers.md))
  (target playlist_parsers.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=playlist_parsers --template=template.html -o playlist_parsers.html || cp no-pandoc playlist_parsers.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/profiling.md))
  (target profiling.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=profiling --template=template.html -o profiling.html || cp no-pandoc profiling.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/release-assets.md))
  (target release-assets.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=release-assets --template=template.html -o release-assets.html || cp no-pandoc release-assets.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/faq.md))
  (target faq.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=faq --template=template.html -o faq.html || cp no-pandoc faq.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/ffmpeg_encoder.md))
  (target ffmpeg_encoder.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=ffmpeg_encoder --template=template.html -o ffmpeg_encoder.html || cp no-pandoc ffmpeg_encoder.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/help.md))
  (target help.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=help --template=template.html -o help.html || cp no-pandoc help.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/requests.md))
  (target requests.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=requests --template=template.html -o requests.html || cp no-pandoc requests.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/protocols-presentation.md))
  (target protocols-presentation.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=protocols-presentation --template=template.html -o protocols-presentation.html || cp no-pandoc protocols-presentation.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/encoding_formats.md))
  (target encoding_formats.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=encoding_formats --template=template.html -o encoding_formats.html || cp no-pandoc encoding_formats.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/metadata.md))
  (target metadata.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=metadata --template=template.html -o metadata.html || cp no-pandoc metadata.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/external_streams.md))
  (target external_streams.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=external_streams --template=template.html -o external_streams.html || cp no-pandoc external_streams.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/publications.md))
  (target publications.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=publications --template=template.html -o publications.html || cp no-pandoc publications.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/radiopi.md))
  (target radiopi.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=radiopi --template=template.html -o radiopi.html || cp no-pandoc radiopi.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/index.md))
  (target index.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=index --template=template.html -o index.html || cp no-pandoc index.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/external_encoders.md))
  (target external_encoders.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=external_encoders --template=template.html -o external_encoders.html || cp no-pandoc external_encoders.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/presentations.md))
  (target presentations.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=presentations --template=template.html -o presentations.html || cp no-pandoc presentations.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/harbor_http.md))
  (target harbor_http.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=harbor_http --template=template.html -o harbor_http.html || cp no-pandoc harbor_http.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/video-static.md))
  (target video-static.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=video-static --template=template.html -o video-static.html || cp no-pandoc video-static.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/harbor.md))
  (target harbor.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=harbor --template=template.html -o harbor.html || cp no-pandoc harbor.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/server.md))
  (target server.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=server --template=template.html -o server.html || cp no-pandoc server.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/stream_content.md))
  (target stream_content.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=stream_content --template=template.html -o stream_content.html || cp no-pandoc stream_content.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/on2.md))
  (target on2.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=on2 --template=template.html -o on2.html || cp no-pandoc on2.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/in_production.md))
  (target in_production.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=in_production --template=template.html -o in_production.html || cp no-pandoc in_production.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/documentation.md))
  (target documentation.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=documentation --template=template.html -o documentation.html || cp no-pandoc documentation.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/beets.md))
  (target beets.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=beets --template=template.html -o beets.html || cp no-pandoc beets.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/split-cue.md))
  (target split-cue.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=split-cue --template=template.html -o split-cue.html || cp no-pandoc split-cue.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/ffmpeg.md))
  (target ffmpeg.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=ffmpeg --template=template.html -o ffmpeg.html || cp no-pandoc ffmpeg.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/cookbook.md))
  (target cookbook.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=cookbook --template=template.html -o cookbook.html || cp no-pandoc cookbook.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/frequence3.md))
  (target frequence3.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=frequence3 --template=template.html -o frequence3.html || cp no-pandoc frequence3.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/migrating.md))
  (target migrating.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=migrating --template=template.html -o migrating.html || cp no-pandoc migrating.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/icy_metadata.md))
  (target icy_metadata.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=icy_metadata --template=template.html -o icy_metadata.html || cp no-pandoc icy_metadata.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/on2_part1.md))
  (target on2_part1.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=on2_part1 --template=template.html -o on2_part1.html || cp no-pandoc on2_part1.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/request_sources.md))
  (target request_sources.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=request_sources --template=template.html -o request_sources.html || cp no-pandoc request_sources.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/shoutcast.md))
  (target shoutcast.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=shoutcast --template=template.html -o shoutcast.html || cp no-pandoc shoutcast.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/ffmpeg_cookbook.md))
  (target ffmpeg_cookbook.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=ffmpeg_cookbook --template=template.html -o ffmpeg_cookbook.html || cp no-pandoc ffmpeg_cookbook.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/ffmpeg_filters.md))
  (target ffmpeg_filters.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=ffmpeg_filters --template=template.html -o ffmpeg_filters.html || cp no-pandoc ffmpeg_filters.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/quick_start.md))
  (target quick_start.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=quick_start --template=template.html -o quick_start.html || cp no-pandoc quick_start.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/ladspa.md))
  (target ladspa.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=ladspa --template=template.html -o ladspa.html || cp no-pandoc ladspa.html"))))

(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md content/external_decoders.md))
  (target external_decoders.html)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %{md} --metadata pagetitle=external_decoders --template=template.html -o external_decoders.html || cp no-pandoc external_decoders.html"))))

(install
 (section doc)
 (package liquidsoap)
 (files
    (orig/fosdem2020/clock.png as html/fosdem2020/clock.png)
(orig/fosdem2020/remark.js as html/fosdem2020/remark.js)
(orig/fosdem2020/radio.gif as html/fosdem2020/radio.gif)
(orig/fosdem2020/logo.png as html/fosdem2020/logo.png)
(orig/fosdem2020/index.html as html/fosdem2020/index.html)
(orig/images/grab.png as html/images/grab.png)
(orig/images/schema-webradio-inkscape.png as html/images/schema-webradio-inkscape.png)
(orig/images/liqgraph.png as html/images/liqgraph.png)
(orig/images/stream.png as html/images/stream.png)
(orig/images/graph_clocks.png as html/images/graph_clocks.png)
(orig/images/design/background_page.png as html/images/design/background_page.png)
(orig/images/design/logo.png as html/images/design/logo.png)
(orig/images/design/background.png as html/images/design/background.png)
(orig/images/basic-radio-graph.png as html/images/basic-radio-graph.png)
(orig/images/tabs/tab_API.png as html/images/tabs/tab_API.png)
(orig/images/tabs/tab_developers.png as html/images/tabs/tab_developers.png)
(orig/images/tabs/tab_snippets.png as html/images/tabs/tab_snippets.png)
(orig/images/tabs/tab_docs.png as html/images/tabs/tab_docs.png)
(orig/images/tabs/tab_about.png as html/images/tabs/tab_about.png)
(orig/css/style.css as html/css/style.css)
(orig/css/homepage.css as html/css/homepage.css)
    (protocols.html as html/protocols.html)
(reference.html as html/reference.html)
(reference-extras.html as html/reference-extras.html)
(settings.html as html/settings.html)
    (reference-header.html as html/reference-header.html)
(language.html as html/language.html)
(geekradio.html as html/geekradio.html)
(on2_part3.html as html/on2_part3.html)
(prometheus.html as html/prometheus.html)
(hls_output.html as html/hls_output.html)
(book.html as html/book.html)
(http_input.html as html/http_input.html)
(gstreamer_encoder.html as html/gstreamer_encoder.html)
(phases.html as html/phases.html)
(crossfade.html as html/crossfade.html)
(custom-path.html as html/custom-path.html)
(on2_part2.html as html/on2_part2.html)
(sources.html as html/sources.html)
(blank.html as html/blank.html)
(flows_devel.html as html/flows_devel.html)
(rolling-release.html as html/rolling-release.html)
(clocks.html as html/clocks.html)
(script_loading.html as html/script_loading.html)
(video.html as html/video.html)
(json.html as html/json.html)
(replay_gain.html as html/replay_gain.html)
(complete_case.html as html/complete_case.html)
(dynamic_sources.html as html/dynamic_sources.html)
(seek.html as html/seek.html)
(playlist_parsers.html as html/playlist_parsers.html)
(profiling.html as html/profiling.html)
(release-assets.html as html/release-assets.html)
(faq.html as html/faq.html)
(ffmpeg_encoder.html as html/ffmpeg_encoder.html)
(help.html as html/help.html)
(requests.html as html/requests.html)
(protocols-presentation.html as html/protocols-presentation.html)
(encoding_formats.html as html/encoding_formats.html)
(metadata.html as html/metadata.html)
(external_streams.html as html/external_streams.html)
(publications.html as html/publications.html)
(radiopi.html as html/radiopi.html)
(index.html as html/index.html)
(external_encoders.html as html/external_encoders.html)
(presentations.html as html/presentations.html)
(harbor_http.html as html/harbor_http.html)
(video-static.html as html/video-static.html)
(harbor.html as html/harbor.html)
(server.html as html/server.html)
(stream_content.html as html/stream_content.html)
(on2.html as html/on2.html)
(in_production.html as html/in_production.html)
(documentation.html as html/documentation.html)
(beets.html as html/beets.html)
(split-cue.html as html/split-cue.html)
(ffmpeg.html as html/ffmpeg.html)
(cookbook.html as html/cookbook.html)
(frequence3.html as html/frequence3.html)
(migrating.html as html/migrating.html)
(icy_metadata.html as html/icy_metadata.html)
(on2_part1.html as html/on2_part1.html)
(request_sources.html as html/request_sources.html)
(shoutcast.html as html/shoutcast.html)
(ffmpeg_cookbook.html as html/ffmpeg_cookbook.html)
(ffmpeg_filters.html as html/ffmpeg_filters.html)
(quick_start.html as html/quick_start.html)
(ladspa.html as html/ladspa.html)
(external_decoders.html as html/external_decoders.html)))
  