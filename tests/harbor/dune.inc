
(rule
  (alias fmt)
  (deps http.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/prettier/prettier-plugin-liquidsoap/dist/liquidsoap.js ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to http.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/harbor/http.liq)))
      (diff http.liq http.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  http.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} http.liq liquidsoap %{test_liq} http.liq)))

(rule
  (alias fmt)
  (deps http2.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/prettier/prettier-plugin-liquidsoap/dist/liquidsoap.js ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to http2.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/harbor/http2.liq)))
      (diff http2.liq http2.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  http2.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} http2.liq liquidsoap %{test_liq} http2.liq)))

(rule
  (alias fmt)
  (deps http3.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/prettier/prettier-plugin-liquidsoap/dist/liquidsoap.js ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to http3.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/harbor/http3.liq)))
      (diff http3.liq http3.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  http3.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} http3.liq liquidsoap %{test_liq} http3.liq)))

(rule
  (alias fmt)
  (deps post.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/prettier/prettier-plugin-liquidsoap/dist/liquidsoap.js ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to post.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/harbor/post.liq)))
      (diff post.liq post.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  post.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} post.liq liquidsoap %{test_liq} post.liq)))

(rule
  (alias fmt)
  (deps put.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/prettier/prettier-plugin-liquidsoap/dist/liquidsoap.js ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to put.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/harbor/put.liq)))
      (diff put.liq put.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  put.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} put.liq liquidsoap %{test_liq} put.liq)))
