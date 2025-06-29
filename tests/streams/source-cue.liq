#!../../liquidsoap ../test.liq

append_cuefile = file.temp("append", ".cue")
create_cuefile = file.temp("create", ".cue")

on_cleanup(
  {
    file.remove(append_cuefile)
    file.remove(create_cuefile)
  }
)

s = sine()

track_pos = ref(1)

def f(m) =
  track_pos := track_pos() + 1
  s.insert_metadata(new_track=true, m)
end

s = source.cue(title="tit", performer="perf", file="bla.mp3", append_cuefile, s)

s =
  source.cue(
    title="tit",
    performer="perf",
    file="bla.mp3",
    comment=
      "this is a comment",
    year=2023,
    last_tracks=2,
    map_metadata=
      fun (m) ->
        list.assoc.filter_map(
          fun (x, y) ->
            if
              y == "title2"
            then
              (
                x,
                "title 2"
              )
            else
              (x, y)
            end,
          m
        ),
    create_cuefile,
    s
  )

append_expected =
  'TITLE "tit"
PERFORMER "perf"
FILE "bla.mp3" MP3
  TRACK 01 AUDIO
    TITLE "title1"
    PERFORMER "artist1"
    REM ALBUM "album1"
    REM DATE 2021
    ISRC bla
    INDEX 01 00:00:00
  TRACK 02 AUDIO
    TITLE "title2"
    PERFORMER "artist2"
    REM ALBUM "album2"
    INDEX 01 00:02:00
  TRACK 03 AUDIO
    TITLE "title3"
    PERFORMER "artist3"
    REM ALBUM "album3"
    INDEX 01 00:03:00\n'

create_expected =
  'TITLE "tit"
PERFORMER "perf"
REM COMMENT "this is a comment"
REM DATE 2023
FILE "bla.mp3" MP3
  TRACK 02 AUDIO
    TITLE "title 2"
    PERFORMER "artist2"
    REM ALBUM "album2"
    INDEX 01 00:02:00
  TRACK 03 AUDIO
    TITLE "title3"
    PERFORMER "artist3"
    REM ALBUM "album3"
    INDEX 01 00:03:00\n'

def check() =
  test.equal(file.contents(append_cuefile), append_expected)
  test.equal(file.contents(create_cuefile), create_expected)
  test.pass()
end

s.on_frame(
  synchronous=true,
  fun () ->
    begin
      if
        track_pos() == 1 and source.time(s) == 0.
      then
        f(
          [
            ("artist", "artist1"),
            ("album", "album1"),
            ("title", "title1"),
            ("cue_year", "2021"),
            ("isrc", "bla")
          ]
        )
      elsif
        track_pos() == 2 and source.time(s) == 2.
      then
        f([("artist", "artist2"), ("album", "album2"), ("title", "title2")])
      elsif
        track_pos() == 2 and source.time(s) == 2.1
      then
        # This one should be deduplicated.
        f(
          [
            ("artist", "artist2"),
            ("album", "album2"),
            ("title", "title2"),
            ("foo", "bar")
          ]
        )
      elsif
        track_pos() == 3 and source.time(s) == 3.
      then
        f([("artist", "artist3"), ("album", "album3"), ("title", "title3")])
      elsif track_pos() == 4 then check()
      end
    end
)

clock.assign_new(sync="none", [s])

output.dummy(s)
