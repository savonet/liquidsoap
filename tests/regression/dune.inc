
(rule
  (alias tree-sitter-parse)
  (deps
   115-1.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/115-1.liq)))))

(rule
  (alias fmt)
  (deps 115-1.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to 115-1.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/115-1.liq)))
      (diff 115-1.liq 115-1.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  115-1.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} 115-1.liq liquidsoap %{test_liq} 115-1.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   115-2.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/115-2.liq)))))

(rule
  (alias fmt)
  (deps 115-2.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to 115-2.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/115-2.liq)))
      (diff 115-2.liq 115-2.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  115-2.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} 115-2.liq liquidsoap %{test_liq} 115-2.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   AC5109.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/AC5109.liq)))))

(rule
  (alias fmt)
  (deps AC5109.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to AC5109.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/AC5109.liq)))
      (diff AC5109.liq AC5109.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  AC5109.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} AC5109.liq liquidsoap %{test_liq} AC5109.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   BUG403.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/BUG403.liq)))))

(rule
  (alias fmt)
  (deps BUG403.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to BUG403.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/BUG403.liq)))
      (diff BUG403.liq BUG403.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  BUG403.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} BUG403.liq liquidsoap %{test_liq} BUG403.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   GH-action-919422659.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/GH-action-919422659.liq)))))

(rule
  (alias fmt)
  (deps GH-action-919422659.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to GH-action-919422659.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/GH-action-919422659.liq)))
      (diff GH-action-919422659.liq GH-action-919422659.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH-action-919422659.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH-action-919422659.liq liquidsoap %{test_liq} GH-action-919422659.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   GH1129.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/GH1129.liq)))))

(rule
  (alias fmt)
  (deps GH1129.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to GH1129.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/GH1129.liq)))
      (diff GH1129.liq GH1129.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH1129.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH1129.liq liquidsoap %{test_liq} GH1129.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   GH1146.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/GH1146.liq)))))

(rule
  (alias fmt)
  (deps GH1146.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to GH1146.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/GH1146.liq)))
      (diff GH1146.liq GH1146.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH1146.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH1146.liq liquidsoap %{test_liq} GH1146.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   GH1151.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/GH1151.liq)))))

(rule
  (alias fmt)
  (deps GH1151.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to GH1151.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/GH1151.liq)))
      (diff GH1151.liq GH1151.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH1151.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH1151.liq liquidsoap %{test_liq} GH1151.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   GH1159.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/GH1159.liq)))))

(rule
  (alias fmt)
  (deps GH1159.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to GH1159.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/GH1159.liq)))
      (diff GH1159.liq GH1159.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH1159.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH1159.liq liquidsoap %{test_liq} GH1159.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   GH1279.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/GH1279.liq)))))

(rule
  (alias fmt)
  (deps GH1279.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to GH1279.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/GH1279.liq)))
      (diff GH1279.liq GH1279.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH1279.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH1279.liq liquidsoap %{test_liq} GH1279.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   GH1327.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/GH1327.liq)))))

(rule
  (alias fmt)
  (deps GH1327.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to GH1327.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/GH1327.liq)))
      (diff GH1327.liq GH1327.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH1327.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH1327.liq liquidsoap %{test_liq} GH1327.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   GH2585.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/GH2585.liq)))))

(rule
  (alias fmt)
  (deps GH2585.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to GH2585.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/GH2585.liq)))
      (diff GH2585.liq GH2585.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH2585.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH2585.liq liquidsoap %{test_liq} GH2585.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   GH2602.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/GH2602.liq)))))

(rule
  (alias fmt)
  (deps GH2602.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to GH2602.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/GH2602.liq)))
      (diff GH2602.liq GH2602.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH2602.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH2602.liq liquidsoap %{test_liq} GH2602.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   GH2756-2.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/GH2756-2.liq)))))

(rule
  (alias fmt)
  (deps GH2756-2.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to GH2756-2.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/GH2756-2.liq)))
      (diff GH2756-2.liq GH2756-2.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH2756-2.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH2756-2.liq liquidsoap %{test_liq} GH2756-2.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   GH2756.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/GH2756.liq)))))

(rule
  (alias fmt)
  (deps GH2756.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to GH2756.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/GH2756.liq)))
      (diff GH2756.liq GH2756.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH2756.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH2756.liq liquidsoap %{test_liq} GH2756.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   GH2758.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/GH2758.liq)))))

(rule
  (alias fmt)
  (deps GH2758.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to GH2758.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/GH2758.liq)))
      (diff GH2758.liq GH2758.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH2758.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH2758.liq liquidsoap %{test_liq} GH2758.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   GH2842.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/GH2842.liq)))))

(rule
  (alias fmt)
  (deps GH2842.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to GH2842.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/GH2842.liq)))
      (diff GH2842.liq GH2842.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH2842.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH2842.liq liquidsoap %{test_liq} GH2842.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   GH2850.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/GH2850.liq)))))

(rule
  (alias fmt)
  (deps GH2850.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to GH2850.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/GH2850.liq)))
      (diff GH2850.liq GH2850.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH2850.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH2850.liq liquidsoap %{test_liq} GH2850.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   GH2867.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/GH2867.liq)))))

(rule
  (alias fmt)
  (deps GH2867.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to GH2867.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/GH2867.liq)))
      (diff GH2867.liq GH2867.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH2867.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH2867.liq liquidsoap %{test_liq} GH2867.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   GH2871.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/GH2871.liq)))))

(rule
  (alias fmt)
  (deps GH2871.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to GH2871.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/GH2871.liq)))
      (diff GH2871.liq GH2871.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH2871.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH2871.liq liquidsoap %{test_liq} GH2871.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   GH2872.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/GH2872.liq)))))

(rule
  (alias fmt)
  (deps GH2872.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to GH2872.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/GH2872.liq)))
      (diff GH2872.liq GH2872.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH2872.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH2872.liq liquidsoap %{test_liq} GH2872.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   GH2897.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/GH2897.liq)))))

(rule
  (alias fmt)
  (deps GH2897.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to GH2897.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/GH2897.liq)))
      (diff GH2897.liq GH2897.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH2897.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH2897.liq liquidsoap %{test_liq} GH2897.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   GH2902.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/GH2902.liq)))))

(rule
  (alias fmt)
  (deps GH2902.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to GH2902.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/GH2902.liq)))
      (diff GH2902.liq GH2902.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH2902.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH2902.liq liquidsoap %{test_liq} GH2902.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   GH2926.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/GH2926.liq)))))

(rule
  (alias fmt)
  (deps GH2926.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to GH2926.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/GH2926.liq)))
      (diff GH2926.liq GH2926.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH2926.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH2926.liq liquidsoap %{test_liq} GH2926.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   GH3093.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/GH3093.liq)))))

(rule
  (alias fmt)
  (deps GH3093.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to GH3093.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/GH3093.liq)))
      (diff GH3093.liq GH3093.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH3093.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH3093.liq liquidsoap %{test_liq} GH3093.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   GH3121.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/GH3121.liq)))))

(rule
  (alias fmt)
  (deps GH3121.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to GH3121.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/GH3121.liq)))
      (diff GH3121.liq GH3121.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH3121.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH3121.liq liquidsoap %{test_liq} GH3121.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   GH3132.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/GH3132.liq)))))

(rule
  (alias fmt)
  (deps GH3132.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to GH3132.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/GH3132.liq)))
      (diff GH3132.liq GH3132.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH3132.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH3132.liq liquidsoap %{test_liq} GH3132.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   GH3224.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/GH3224.liq)))))

(rule
  (alias fmt)
  (deps GH3224.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to GH3224.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/GH3224.liq)))
      (diff GH3224.liq GH3224.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH3224.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH3224.liq liquidsoap %{test_liq} GH3224.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   GH3239-2.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/GH3239-2.liq)))))

(rule
  (alias fmt)
  (deps GH3239-2.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to GH3239-2.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/GH3239-2.liq)))
      (diff GH3239-2.liq GH3239-2.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH3239-2.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH3239-2.liq liquidsoap %{test_liq} GH3239-2.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   GH3239.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/GH3239.liq)))))

(rule
  (alias fmt)
  (deps GH3239.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to GH3239.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/GH3239.liq)))
      (diff GH3239.liq GH3239.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH3239.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH3239.liq liquidsoap %{test_liq} GH3239.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   GH3276.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/GH3276.liq)))))

(rule
  (alias fmt)
  (deps GH3276.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to GH3276.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/GH3276.liq)))
      (diff GH3276.liq GH3276.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH3276.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH3276.liq liquidsoap %{test_liq} GH3276.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   GH3316.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/GH3316.liq)))))

(rule
  (alias fmt)
  (deps GH3316.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to GH3316.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/GH3316.liq)))
      (diff GH3316.liq GH3316.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH3316.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH3316.liq liquidsoap %{test_liq} GH3316.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   LS268.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/LS268.liq)))))

(rule
  (alias fmt)
  (deps LS268.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to LS268.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/LS268.liq)))
      (diff LS268.liq LS268.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  LS268.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} LS268.liq liquidsoap %{test_liq} LS268.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   LS354-1.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/LS354-1.liq)))))

(rule
  (alias fmt)
  (deps LS354-1.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to LS354-1.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/LS354-1.liq)))
      (diff LS354-1.liq LS354-1.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  LS354-1.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} LS354-1.liq liquidsoap %{test_liq} LS354-1.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   LS354-2.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/LS354-2.liq)))))

(rule
  (alias fmt)
  (deps LS354-2.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to LS354-2.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/LS354-2.liq)))
      (diff LS354-2.liq LS354-2.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  LS354-2.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} LS354-2.liq liquidsoap %{test_liq} LS354-2.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   LS460.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/LS460.liq)))))

(rule
  (alias fmt)
  (deps LS460.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to LS460.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/LS460.liq)))
      (diff LS460.liq LS460.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  LS460.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} LS460.liq liquidsoap %{test_liq} LS460.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   LS503.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/LS503.liq)))))

(rule
  (alias fmt)
  (deps LS503.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to LS503.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/LS503.liq)))
      (diff LS503.liq LS503.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  LS503.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} LS503.liq liquidsoap %{test_liq} LS503.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   default_format.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/default_format.liq)))))

(rule
  (alias fmt)
  (deps default_format.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to default_format.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/default_format.liq)))
      (diff default_format.liq default_format.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  default_format.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} default_format.liq liquidsoap %{test_liq} default_format.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   external-encoder.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/external-encoder.liq)))))

(rule
  (alias fmt)
  (deps external-encoder.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to external-encoder.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/external-encoder.liq)))
      (diff external-encoder.liq external-encoder.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  external-encoder.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} external-encoder.liq liquidsoap %{test_liq} external-encoder.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   ffmpeg-copy-encode.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/ffmpeg-copy-encode.liq)))))

(rule
  (alias fmt)
  (deps ffmpeg-copy-encode.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to ffmpeg-copy-encode.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/ffmpeg-copy-encode.liq)))
      (diff ffmpeg-copy-encode.liq ffmpeg-copy-encode.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  ffmpeg-copy-encode.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} ffmpeg-copy-encode.liq liquidsoap %{test_liq} ffmpeg-copy-encode.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   ffmpeg-copy-input-http.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/ffmpeg-copy-input-http.liq)))))

(rule
  (alias fmt)
  (deps ffmpeg-copy-input-http.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to ffmpeg-copy-input-http.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/ffmpeg-copy-input-http.liq)))
      (diff ffmpeg-copy-input-http.liq ffmpeg-copy-input-http.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  ffmpeg-copy-input-http.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} ffmpeg-copy-input-http.liq liquidsoap %{test_liq} ffmpeg-copy-input-http.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   ffmpeg-naming-convention.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/ffmpeg-naming-convention.liq)))))

(rule
  (alias fmt)
  (deps ffmpeg-naming-convention.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to ffmpeg-naming-convention.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/ffmpeg-naming-convention.liq)))
      (diff ffmpeg-naming-convention.liq ffmpeg-naming-convention.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  ffmpeg-naming-convention.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} ffmpeg-naming-convention.liq liquidsoap %{test_liq} ffmpeg-naming-convention.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   infallible-shutdown.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/infallible-shutdown.liq)))))

(rule
  (alias fmt)
  (deps infallible-shutdown.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to infallible-shutdown.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/infallible-shutdown.liq)))
      (diff infallible-shutdown.liq infallible-shutdown.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  infallible-shutdown.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} infallible-shutdown.liq liquidsoap %{test_liq} infallible-shutdown.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   init-error.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/init-error.liq)))))

(rule
  (alias fmt)
  (deps init-error.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to init-error.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/init-error.liq)))
      (diff init-error.liq init-error.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  init-error.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} init-error.liq liquidsoap %{test_liq} init-error.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   initial_request_queue.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/initial_request_queue.liq)))))

(rule
  (alias fmt)
  (deps initial_request_queue.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to initial_request_queue.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/initial_request_queue.liq)))
      (diff initial_request_queue.liq initial_request_queue.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  initial_request_queue.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} initial_request_queue.liq liquidsoap %{test_liq} initial_request_queue.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   input_rtmp.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/input_rtmp.liq)))))

(rule
  (alias fmt)
  (deps input_rtmp.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to input_rtmp.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/input_rtmp.liq)))
      (diff input_rtmp.liq input_rtmp.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  input_rtmp.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} input_rtmp.liq liquidsoap %{test_liq} input_rtmp.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   metadata_cache.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/metadata_cache.liq)))))

(rule
  (alias fmt)
  (deps metadata_cache.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to metadata_cache.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/metadata_cache.liq)))
      (diff metadata_cache.liq metadata_cache.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  metadata_cache.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} metadata_cache.liq liquidsoap %{test_liq} metadata_cache.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   playlist-id.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/playlist-id.liq)))))

(rule
  (alias fmt)
  (deps playlist-id.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to playlist-id.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/playlist-id.liq)))
      (diff playlist-id.liq playlist-id.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  playlist-id.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} playlist-id.liq liquidsoap %{test_liq} playlist-id.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   replaygain.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/replaygain.liq)))))

(rule
  (alias fmt)
  (deps replaygain.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to replaygain.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/replaygain.liq)))
      (diff replaygain.liq replaygain.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  replaygain.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} replaygain.liq liquidsoap %{test_liq} replaygain.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   shoutcast-args.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/shoutcast-args.liq)))))

(rule
  (alias fmt)
  (deps shoutcast-args.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to shoutcast-args.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/shoutcast-args.liq)))
      (diff shoutcast-args.liq shoutcast-args.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  shoutcast-args.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} shoutcast-args.liq liquidsoap %{test_liq} shoutcast-args.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   unified-pcm-types.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/unified-pcm-types.liq)))))

(rule
  (alias fmt)
  (deps unified-pcm-types.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to unified-pcm-types.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/unified-pcm-types.liq)))
      (diff unified-pcm-types.liq unified-pcm-types.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  unified-pcm-types.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} unified-pcm-types.liq liquidsoap %{test_liq} unified-pcm-types.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   video-only.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/regression/video-only.liq)))))

(rule
  (alias fmt)
  (deps video-only.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to video-only.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/video-only.liq)))
      (diff video-only.liq video-only.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  video-only.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} video-only.liq liquidsoap %{test_liq} video-only.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH2573.output
  (:check_output ../check_output.exe)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH2573.output %{check_output})))
