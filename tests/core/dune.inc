
(executable
 (name content_test)
 (modules content_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  
  (:content_test content_test.exe))
 (action (run %{content_test} )))


(executable
 (name decoder_test)
 (modules decoder_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  
  (:decoder_test decoder_test.exe))
 (action (run %{decoder_test} )))


(executable
 (name ffmpeg_quality)
 (modules ffmpeg_quality)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  
  (:ffmpeg_quality ffmpeg_quality.exe))
 (action (run %{ffmpeg_quality} )))


(executable
 (name flush_test)
 (modules flush_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  
  (:flush_test flush_test.exe))
 (action (run %{flush_test} )))


(executable
 (name frame_test)
 (modules frame_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  
  (:frame_test frame_test.exe))
 (action (run %{frame_test} )))


(executable
 (name generator_test)
 (modules generator_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  
  (:generator_test generator_test.exe))
 (action (run %{generator_test} )))


(executable
 (name http_test)
 (modules http_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  
  (:http_test http_test.exe))
 (action (run %{http_test} )))


(executable
 (name is_url)
 (modules is_url)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  
  (:is_url is_url.exe))
 (action (run %{is_url} )))


(executable
 (name json_test)
 (modules json_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias citest)
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
 (alias citest)
 (package liquidsoap)
 (deps
  
  (:meth meth.exe))
 (action (run %{meth} )))


(executable
 (name more_types)
 (modules more_types)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  
  (:more_types more_types.exe))
 (action (run %{more_types} )))


(executable
 (name output_encoded_test)
 (modules output_encoded_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  
  (:output_encoded_test output_encoded_test.exe))
 (action (run %{output_encoded_test} )))


(executable
 (name output_test)
 (modules output_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  
  (:output_test output_test.exe))
 (action (run %{output_test} )))


(executable
 (name parsesrt)
 (modules parsesrt)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias citest)
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
 (alias citest)
 (package liquidsoap)
 (deps
  
  (:playlist_basic_test playlist_basic_test.exe))
 (action (run %{playlist_basic_test} )))


(executable
 (name pool_test)
 (modules pool_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  
  (:pool_test pool_test.exe))
 (action (run %{pool_test} )))


(executable
 (name start_stop_test)
 (modules start_stop_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  
  (:start_stop_test start_stop_test.exe))
 (action (run %{start_stop_test} )))


(executable
 (name stream_decoder_test)
 (modules stream_decoder_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias citest)
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
 (alias citest)
 (package liquidsoap)
 (deps
  
  (:string_test string_test.exe))
 (action (run %{string_test} )))


(executable
 (name strings_test)
 (modules strings_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  
  (:strings_test strings_test.exe))
 (action (run %{strings_test} )))


(executable
 (name timezone)
 (modules timezone)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  
  (:timezone timezone.exe))
 (action (run %{timezone} )))


(executable
 (name types)
 (modules types)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  
  (:types types.exe))
 (action (run %{types} )))


(executable
 (name unifier_test)
 (modules unifier_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  
  (:unifier_test unifier_test.exe))
 (action (run %{unifier_test} )))


(executable
 (name utils_test)
 (modules utils_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  
  (:utils_test utils_test.exe))
 (action (run %{utils_test} )))


(executable
 (name version_test)
 (modules version_test)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  
  (:version_test version_test.exe))
 (action (run %{version_test} )))

