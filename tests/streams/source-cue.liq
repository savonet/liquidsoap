#!../../liquidsoap ../test.liq

append_cuefile = file.temp("append", ".cue")
create_cuefile = file.temp("create", ".cue")

on_cleanup(
  {
    file.remove(append_cuefile)
    file.remove(create_cuefile)
  }
)

s = insert_metadata(sine())

track_pos = ref(1)

def f(m) =
  track_pos := track_pos() + 1
  s.insert_metadata(m)
end

s = source.cue(title="tit", performer="perf", file="bla.mp3", append_cuefile, s)

s =
  source.cue(
    title="tit",
    performer="perf",
    file="bla.mp3",
    max_length=2,
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
    INDEX 01 00:00:01
  TRACK 02 AUDIO
    TITLE "title2"
    PERFORMER "artist2"
    REM ALBUM "album2"
    INDEX 01 00:00:02
  TRACK 03 AUDIO
    TITLE "title3"
    PERFORMER "artist3"
    REM ALBUM "album3"
    INDEX 01 00:00:03\n'

create_expected =
  'TITLE "tit"
PERFORMER "perf"
FILE "bla.mp3" MP3
  TRACK 01 AUDIO
    TITLE "title2"
    PERFORMER "artist2"
    REM ALBUM "album2"
    INDEX 01 00:00:02
  TRACK 02 AUDIO
    TITLE "title3"
    PERFORMER "artist3"
    REM ALBUM "album3"
    INDEX 01 00:00:03\n'

def check() =
  test.equals(file.contents(append_cuefile), append_expected)
  test.equals(file.contents(create_cuefile), create_expected)
  test.pass()
end

s =
  source.on_frame(
    s,
    fun () ->
      begin
        if
          track_pos() == 1 and source.time(s) == 0.
        then
          f([("artist", "artist1"), ("album", "album1"), ("title", "title1")])
        elsif
          track_pos() == 2 and source.time(s) == 1.
        then
          f([("artist", "artist2"), ("album", "album2"), ("title", "title2")])
        elsif
          track_pos() == 3 and source.time(s) == 3.
        then
          f([("artist", "artist3"), ("album", "album3"), ("title", "title3")])
        elsif
          track_pos() == 4 and source.time(s) == 6.
        then
          f([("artist", "artist4"), ("album", "album4"), ("title", "title4")])
        elsif track_pos() == 5 then check()
        end
      end
  )

clock.assign_new(sync="none", [s])

output.dummy(s)
