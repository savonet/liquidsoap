playlist_content =
  '<?xml version="1.0" encoding="UTF-8"?>
<playlist xmlns="http://xspf.org/ns/0/" xmlns:vlc="http://www.videolan.org/vlc/playlist/ns/0/" version="1">
        <title>Playlist</title>
        <trackList>
                <track>
                        <location>file:///C:/Users/user/Music/testing/test-file-1.ogg</location>
                        <title>Very good song</title>
                        <annotation>Other</annotation>
                        <duration>61445</duration>
                </track>
                <track>
                        <location>file:///C:/Users/user/Music/testing/test-file-2.ogg</location>
                        <duration>1240</duration>
                </track>
        </trackList>
</playlist>'

expected_content =
  [
    (
      [
        ("location", "file:///C:/Users/user/Music/testing/test-file-1.ogg"),
        (
          "title",
          "Very good song"
        ),
        ("annotation", "Other"),
        ("duration", "61445")
      ],
      "file:///C:/Users/user/Music/testing/test-file-1.ogg"
    ),
    (
      [
        ("location", "file:///C:/Users/user/Music/testing/test-file-2.ogg"),
        ("duration", "1240")
      ],
      "file:///C:/Users/user/Music/testing/test-file-2.ogg"
    )
  ]

playlist_file = file.temp("playlist", ".xspf")

file.write(data=playlist_content, playlist_file)

on_cleanup({file.remove(playlist_file)})

playlist_content = playlist.parse(playlist_file)

def f() =
  test.equals(playlist_content, expected_content)
  test.pass()
end

test.check(f)
