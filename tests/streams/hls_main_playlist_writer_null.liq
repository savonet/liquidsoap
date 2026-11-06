%include "../test.liq"

# playlists.md §3: `main_playlist_writer=null` writes no main playlist and
# the media playlists as usual.
dir = file.temp_dir("hls_main_playlist_writer_null")
on_cleanup({file.rmdir(dir)})

s = sine()
clock.assign_new(sync="none", [s])

o =
  output.file.hls(
    main_playlist_writer=null,
    segment_duration=1.,
    dir,
    [("a", %mp3), ("b", %shine(bitrate = 64))],
    s
  )

events = ref(0)

def check(e) =
  if
    not test.done()
  then
    ref.incr(events)
    if
      path.basename(e.path) == "stream.m3u8"
      or file.exists("#{dir}/stream.m3u8")
    then
      test.fail(
        "a main playlist was written: #{e.state} #{e.path}"
      )
    end
    if
      path.basename(e.path) == "b_5.mp3"
    then
      if
        not file.exists("#{dir}/a.m3u8") or not file.exists("#{dir}/b.m3u8")
      then
        test.fail(
          "media playlists missing: #{file.ls(dir)}"
        )
      end
      print(
        "#{events()} events checked"
      )
      test.pass()
    end
  end
end

o.on_file_change(synchronous=true, check)
