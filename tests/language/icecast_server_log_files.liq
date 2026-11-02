%include "../test.liq"
%include "../../src/libs/extra/icecast_server/settings.liq"
%include "../../src/libs/extra/icecast_server/events.liq"
%include "../../src/libs/extra/icecast_server/stats.liq"
%include "../../src/libs/extra/icecast_server/mounts.liq"
%include "../../src/libs/extra/icecast_server/log_lines.liq"
%include "../../src/libs/extra/icecast_server/log_files.liq"
%include "../../src/libs/extra/icecast_server/logs.liq"

# Log files of icecast.server (observability.md §9, §14) where they fail or
# are filtered, which the streaming test cannot provoke.
checks = ref(0)
log_path = file.temp("icecast-log-files", ".log")
settings.log.file.path := log_path
settings.log.file := true

def check_equal(label, got, expected) =
  checks := checks() + 1
  if
    got != expected
  then
    test.fail(
      "#{label}: got #{got}, expected #{expected}"
    )
  end
end

def lines(path) =
  list.filter(fun (line) -> line != "", file.lines(path))
end

def logged(text) =
  log.flush()
  list.length(
    list.filter(
      fun (line) -> string.contains(substring=text, line),
      file.lines(log_path)
    )
  )
end

# A rotation whose rename fails is tried again only after another
# `size_limit` bytes: a non-empty directory at `.old` makes the rename fail,
# for root too.
def check_failed_rotation(directory) =
  rotated = path.concat(directory, "rotate.log")
  old = "#{rotated}.old"
  file.mkdir(old)
  file.write(data="x", path.concat(old, "keep"))
  f = icecast.server.log_files.file(label="test")
  line = string.make(char_code=97, 29)
  for _ = 1 to 9 do
    f.write(target=rotated, size_limit=100, archive=false, line)

  end
  check_equal(
    "a failed rotation waits for another size_limit bytes",
    logged(
      "Log file rotation failed"
    ),
    2
  )
  check_equal(
    "lines keep going to the file in place",
    list.length(lines(rotated)),
    9
  )
end

# A message filtered out by `log_level` leaves the error log open: after an
# external move, the next line still goes to the moved file.
def check_filtered_keeps_file(directory) =
  error_log = path.concat(directory, "error.log")
  moved = path.concat(directory, "error.log.1")
  logs =
    icecast.server.logs.create(
      label="test",
      compatibility="icecast-2.5",
      fun () ->
        {
          access_log = null,
          playlist_log = null,
          error_log = error_log,
          log_level = 2,
          log_size = 1000000,
          log_archive = false
        }
    )
  logs.error(category="test/test", 2, "first")
  file.move(error_log, moved)
  logs.error(category="test/test", 4, "filtered")
  logs.error(category="test/test", 2, "second")
  check_equal(
    "a filtered message does not close the error log",
    (list.length(lines(moved)), file.exists(error_log)),
    (2, false)
  )
end

def f() =
  directory = file.temp_dir(cleanup=true, "icecast-log-files")
  check_failed_rotation(directory)
  check_filtered_keeps_file(directory)
  if
    checks() < 3
  then
    test.fail(
      "only #{checks()} checks ran"
    )
  end
  test.pass()
end

test.check(f)
