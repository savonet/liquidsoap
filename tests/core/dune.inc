
(executable
 (name clock_utils_test)
 (modules clock_utils_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias clock_utils_test)
 (package liquidsoap)
 (deps
  
  (:clock_utils_test clock_utils_test.exe))
 (action (run %{clock_utils_test} )))


(executable
 (name content_test)
 (modules content_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias content_test)
 (package liquidsoap)
 (deps
  
  (:content_test content_test.exe))
 (action (run %{content_test} )))


(executable
 (name decoder_test)
 (modules decoder_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias decoder_test)
 (package liquidsoap)
 (deps
  
  (:decoder_test decoder_test.exe))
 (action (run %{decoder_test} )))


(executable
 (name ffmpeg_quality)
 (modules ffmpeg_quality)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias ffmpeg_quality)
 (package liquidsoap)
 (deps
  
  (:ffmpeg_quality ffmpeg_quality.exe))
 (action (run %{ffmpeg_quality} )))


(executable
 (name flush_test)
 (modules flush_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias flush_test)
 (package liquidsoap)
 (deps
  
  (:flush_test flush_test.exe))
 (action (run %{flush_test} )))


(executable
 (name frame_checksum_test)
 (modules frame_checksum_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias frame_checksum_test)
 (package liquidsoap)
 (deps
  
  (:frame_checksum_test frame_checksum_test.exe))
 (action (run %{frame_checksum_test} )))


(executable
 (name frame_test)
 (modules frame_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias frame_test)
 (package liquidsoap)
 (deps
  
  (:frame_test frame_test.exe))
 (action (run %{frame_test} )))


(executable
 (name generator_test)
 (modules generator_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias generator_test)
 (package liquidsoap)
 (deps
  
  (:generator_test generator_test.exe))
 (action (run %{generator_test} )))


(executable
 (name http_test)
 (modules http_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias http_test)
 (package liquidsoap)
 (deps
  
  (:http_test http_test.exe))
 (action (run %{http_test} )))


(executable
 (name is_url)
 (modules is_url)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias is_url)
 (package liquidsoap)
 (deps
  
  (:is_url is_url.exe))
 (action (run %{is_url} )))


(executable
 (name json_test)
 (modules json_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias json_test)
 (package liquidsoap)
 (deps
  (:json ./json) (:json5 ./json5) (:big-list-of-naughty-strings ./big-list-of-naughty-strings)
  (:json_test json_test.exe))
 (action (run %{json_test} )))


(executable
 (name meth)
 (modules meth)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias meth)
 (package liquidsoap)
 (deps
  
  (:meth meth.exe))
 (action (run %{meth} )))


(executable
 (name more_types)
 (modules more_types)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias more_types)
 (package liquidsoap)
 (deps
  
  (:more_types more_types.exe))
 (action (run %{more_types} )))


(executable
 (name output_encoded_test)
 (modules output_encoded_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias output_encoded_test)
 (package liquidsoap)
 (deps
  
  (:output_encoded_test output_encoded_test.exe))
 (action (run %{output_encoded_test} )))


(executable
 (name output_test)
 (modules output_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias output_test)
 (package liquidsoap)
 (deps
  
  (:output_test output_test.exe))
 (action (run %{output_test} )))


(executable
 (name parsesrt)
 (modules parsesrt)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias parsesrt)
 (package liquidsoap)
 (deps
  (:test_srt ./test.srt)
  (:parsesrt parsesrt.exe))
 (action (run %{parsesrt} %{test_srt})))


(executable
 (name playlist_basic_test)
 (modules playlist_basic_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias playlist_basic_test)
 (package liquidsoap)
 (deps
  
  (:playlist_basic_test playlist_basic_test.exe))
 (action (run %{playlist_basic_test} )))


(executable
 (name pool_test)
 (modules pool_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias pool_test)
 (package liquidsoap)
 (deps
  
  (:pool_test pool_test.exe))
 (action (run %{pool_test} )))


(executable
 (name start_stop_test)
 (modules start_stop_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias start_stop_test)
 (package liquidsoap)
 (deps
  
  (:start_stop_test start_stop_test.exe))
 (action (run %{start_stop_test} )))


(executable
 (name stream_decoder_test)
 (modules stream_decoder_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias stream_decoder_test)
 (package liquidsoap)
 (deps
  (:test_wav ./test.wav) (:test_mp3 ./test.mp3)
  (:stream_decoder_test stream_decoder_test.exe))
 (action (progn (run %{stream_decoder_test} %{test_wav} bla.wav) (run %{stream_decoder_test} %{test_mp3} bla.wav))))


(executable
 (name string_test)
 (modules string_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias string_test)
 (package liquidsoap)
 (deps
  
  (:string_test string_test.exe))
 (action (run %{string_test} )))


(executable
 (name strings_test)
 (modules strings_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias strings_test)
 (package liquidsoap)
 (deps
  
  (:strings_test strings_test.exe))
 (action (run %{strings_test} )))


(executable
 (name timezone)
 (modules timezone)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias timezone)
 (package liquidsoap)
 (deps
  
  (:timezone timezone.exe))
 (action (run %{timezone} )))


(executable
 (name types)
 (modules types)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias types)
 (package liquidsoap)
 (deps
  
  (:types types.exe))
 (action (run %{types} )))


(executable
 (name unifier_test)
 (modules unifier_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias unifier_test)
 (package liquidsoap)
 (deps
  
  (:unifier_test unifier_test.exe))
 (action (run %{unifier_test} )))


(executable
 (name utils_test)
 (modules utils_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias utils_test)
 (package liquidsoap)
 (deps
  
  (:utils_test utils_test.exe))
 (action (run %{utils_test} )))


(executable
 (name version_test)
 (modules version_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias version_test)
 (package liquidsoap)
 (deps
  
  (:version_test version_test.exe))
 (action (run %{version_test} )))


(executable
 (name weak_queue_test)
 (modules weak_queue_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias weak_queue_test)
 (package liquidsoap)
 (deps
  
  (:weak_queue_test weak_queue_test.exe))
 (action (run %{weak_queue_test} )))

(alias
  (name citest)
  (deps
    (alias clock_utils_test)
    (alias content_test)
    (alias decoder_test)
    (alias ffmpeg_quality)
    (alias flush_test)
    (alias frame_checksum_test)
    (alias frame_test)
    (alias generator_test)
    (alias http_test)
    (alias is_url)
    (alias json_test)
    (alias meth)
    (alias more_types)
    (alias output_encoded_test)
    (alias output_test)
    (alias parsesrt)
    (alias playlist_basic_test)
    (alias pool_test)
    (alias start_stop_test)
    (alias stream_decoder_test)
    (alias string_test)
    (alias strings_test)
    (alias timezone)
    (alias types)
    (alias unifier_test)
    (alias utils_test)
    (alias version_test)
    (alias weak_queue_test)))
