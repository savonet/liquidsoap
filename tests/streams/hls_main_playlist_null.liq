%include "../test.liq"

# playlists.md §3, known-complexity.md KC11: a main playlist writer returning
# `null` leaves no file and no error, and an existing main playlist as it is.
dir = file.temp_dir("hls_main_playlist_null")
on_cleanup({file.rmdir(dir)})
fresh = "#{dir}/fresh"
existing = "#{dir}/existing"
file.mkdir(parents=true, existing)
file.write(data="kept", "#{existing}/stream.m3u8")

s = sine()
clock.assign_new(sync="none", [s])

def no_main_playlist(~extra_tags, ~prefix, ~version, streams) =
  ignore(extra_tags)
  ignore(prefix)
  ignore(version)
  ignore(streams)
  (null : string?)
end

def no_reopen(err) =
  test.fail(
    "reopen_on_error called: #{err}"
  )
  null
end

checks = ref(0)

def check(event) =
  if
    not test.done()
  then
    if
      file.exists("#{fresh}/stream.m3u8")
    then
      test.fail(
        "a null writer left #{fresh}/stream.m3u8"
      )
    end
    if
      file.contents("#{existing}/stream.m3u8") != "kept"
    then
      test.fail(
        "a null writer changed #{existing}/stream.m3u8"
      )
    end
    if
      path.basename(event.path) == "stream.m3u8"
    then
      test.fail(
        "#{event.state} event for #{event.path}"
      )
    end
    checks := checks() + 1
    if
      event.path == "#{existing}/b_10.mp3"
    then
      if
        not file.exists("#{fresh}/b.m3u8")
      then
        test.fail(
          "no media playlist in #{fresh}"
        )
      end
      print(
        "#{checks()} events checked"
      )
      test.pass()
    end
  end
end

list.iter(
  fun (out) ->
    begin
      o =
        output.file.hls(
          main_playlist_writer=no_main_playlist,
          reopen_on_error=no_reopen,
          segment_duration=1.,
          out,
          [("a", %mp3), ("b", %shine(bitrate = 64))],
          s
        )

      o.on_file_change(synchronous=true, check)
    end,
  [fresh, existing]
)
