
(rule
  (alias fmt)
  (deps
   audio.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to audio.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./audio.liq)))
      (diff audio.liq audio.liq.prettier))))

(rule
  (alias fmt)
  (deps
   clock.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to clock.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./clock.liq)))
      (diff clock.liq clock.liq.prettier))))

(rule
  (alias fmt)
  (deps
   error.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to error.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./error.liq)))
      (diff error.liq error.liq.prettier))))

(rule
  (alias fmt)
  (deps
   fades.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to fades.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./fades.liq)))
      (diff fades.liq fades.liq.prettier))))

(rule
  (alias fmt)
  (deps
   ffmpeg.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to ffmpeg.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./ffmpeg.liq)))
      (diff ffmpeg.liq ffmpeg.liq.prettier))))

(rule
  (alias fmt)
  (deps
   file.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to file.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./file.liq)))
      (diff file.liq file.liq.prettier))))

(rule
  (alias fmt)
  (deps
   getter.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to getter.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./getter.liq)))
      (diff getter.liq getter.liq.prettier))))

(rule
  (alias fmt)
  (deps
   hls.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to hls.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./hls.liq)))
      (diff hls.liq hls.liq.prettier))))

(rule
  (alias fmt)
  (deps
   http.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to http.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./http.liq)))
      (diff http.liq http.liq.prettier))))

(rule
  (alias fmt)
  (deps
   http_codes.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to http_codes.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./http_codes.liq)))
      (diff http_codes.liq http_codes.liq.prettier))))

(rule
  (alias fmt)
  (deps
   icecast.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to icecast.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./icecast.liq)))
      (diff icecast.liq icecast.liq.prettier))))

(rule
  (alias fmt)
  (deps
   io.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to io.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./io.liq)))
      (diff io.liq io.liq.prettier))))

(rule
  (alias fmt)
  (deps
   liquidsoap.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to liquidsoap.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./liquidsoap.liq)))
      (diff liquidsoap.liq liquidsoap.liq.prettier))))

(rule
  (alias fmt)
  (deps
   list.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to list.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./list.liq)))
      (diff list.liq list.liq.prettier))))

(rule
  (alias fmt)
  (deps
   log.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to log.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./log.liq)))
      (diff log.liq log.liq.prettier))))

(rule
  (alias fmt)
  (deps
   math.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to math.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./math.liq)))
      (diff math.liq math.liq.prettier))))

(rule
  (alias fmt)
  (deps
   medialib.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to medialib.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./medialib.liq)))
      (diff medialib.liq medialib.liq.prettier))))

(rule
  (alias fmt)
  (deps
   metadata.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to metadata.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./metadata.liq)))
      (diff metadata.liq metadata.liq.prettier))))

(rule
  (alias fmt)
  (deps
   null.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to null.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./null.liq)))
      (diff null.liq null.liq.prettier))))

(rule
  (alias fmt)
  (deps
   playlist.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to playlist.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./playlist.liq)))
      (diff playlist.liq playlist.liq.prettier))))

(rule
  (alias fmt)
  (deps
   predicate.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to predicate.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./predicate.liq)))
      (diff predicate.liq predicate.liq.prettier))))

(rule
  (alias fmt)
  (deps
   process.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to process.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./process.liq)))
      (diff process.liq process.liq.prettier))))

(rule
  (alias fmt)
  (deps
   profiler.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to profiler.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./profiler.liq)))
      (diff profiler.liq profiler.liq.prettier))))

(rule
  (alias fmt)
  (deps
   protocols.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to protocols.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./protocols.liq)))
      (diff protocols.liq protocols.liq.prettier))))

(rule
  (alias fmt)
  (deps
   ref.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to ref.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./ref.liq)))
      (diff ref.liq ref.liq.prettier))))

(rule
  (alias fmt)
  (deps
   replaygain.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to replaygain.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./replaygain.liq)))
      (diff replaygain.liq replaygain.liq.prettier))))

(rule
  (alias fmt)
  (deps
   request.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to request.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./request.liq)))
      (diff request.liq request.liq.prettier))))

(rule
  (alias fmt)
  (deps
   resolvers.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to resolvers.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./resolvers.liq)))
      (diff resolvers.liq resolvers.liq.prettier))))

(rule
  (alias fmt)
  (deps
   runtime.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to runtime.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./runtime.liq)))
      (diff runtime.liq runtime.liq.prettier))))

(rule
  (alias fmt)
  (deps
   server.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to server.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./server.liq)))
      (diff server.liq server.liq.prettier))))

(rule
  (alias fmt)
  (deps
   settings.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to settings.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./settings.liq)))
      (diff settings.liq settings.liq.prettier))))

(rule
  (alias fmt)
  (deps
   socket.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to socket.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./socket.liq)))
      (diff socket.liq socket.liq.prettier))))

(rule
  (alias fmt)
  (deps
   source.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to source.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./source.liq)))
      (diff source.liq source.liq.prettier))))

(rule
  (alias fmt)
  (deps
   stdlib.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to stdlib.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./stdlib.liq)))
      (diff stdlib.liq stdlib.liq.prettier))))

(rule
  (alias fmt)
  (deps
   string.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to string.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./string.liq)))
      (diff string.liq string.liq.prettier))))

(rule
  (alias fmt)
  (deps
   switches.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to switches.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./switches.liq)))
      (diff switches.liq switches.liq.prettier))))

(rule
  (alias fmt)
  (deps
   testing.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to testing.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./testing.liq)))
      (diff testing.liq testing.liq.prettier))))

(rule
  (alias fmt)
  (deps
   thread.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to thread.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./thread.liq)))
      (diff thread.liq thread.liq.prettier))))

(rule
  (alias fmt)
  (deps
   tracks.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to tracks.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./tracks.liq)))
      (diff tracks.liq tracks.liq.prettier))))

(rule
  (alias fmt)
  (deps
   utils.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to utils.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./utils.liq)))
      (diff utils.liq utils.liq.prettier))))

(rule
  (alias fmt)
  (deps
   video.liq
   ./../tooling/test-prettier
   ./../tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js
 )
  (action
    (progn
      (with-stdout-to video.liq.prettier
       (chdir ./../tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../libs/./video.liq)))
      (diff video.liq video.liq.prettier))))

(rule
  (alias tree-sitter-parse)
  (deps
   audio.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./audio.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   clock.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./clock.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   error.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./error.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   fades.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./fades.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   ffmpeg.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./ffmpeg.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   file.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./file.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   getter.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./getter.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   hls.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./hls.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   http.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./http.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   http_codes.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./http_codes.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   icecast.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./icecast.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   io.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./io.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   liquidsoap.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./liquidsoap.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   list.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./list.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   log.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./log.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   math.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./math.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   medialib.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./medialib.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   metadata.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./metadata.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   null.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./null.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   playlist.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./playlist.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   predicate.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./predicate.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   process.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./process.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   profiler.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./profiler.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   protocols.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./protocols.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   ref.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./ref.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   replaygain.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./replaygain.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   request.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./request.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   resolvers.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./resolvers.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   runtime.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./runtime.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   server.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./server.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   settings.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./settings.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   socket.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./socket.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   source.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./source.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   stdlib.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./stdlib.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   string.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./string.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   switches.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./switches.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   testing.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./testing.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   thread.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./thread.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   tracks.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./tracks.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   utils.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./utils.liq)))))

(rule
  (alias tree-sitter-parse)
  (deps
   video.liq
   (source_tree ./../tooling/tree-sitter-liquidsoap)
   ./../tooling/test-tree-sitter
 )
  (action
    (chdir ./../tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../libs/./video.liq)))))
