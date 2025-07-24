def f() =
  cue_file =
    'REM GENRE Electronica
REM DATE 2020
PERFORMER "Electronic Mix"
TITLE "Musique a la carte"
FILE "Bronski beat - Smalltown boy (12 extended).mp3" MP3
  TRACK 01 AUDIO
    TITLE "Smalltown boy (12 extended)"
    PERFORMER "Bronski Beat"
    INDEX 01 00:00:00
FILE "Rihanna - Dont Stop The Music (Ed Marquis Remix).mp3" MP3
  TRACK 02 AUDIO
    TITLE "Dont Stop The Music (Ed Marquis Remix)"
    PERFORMER "Rihanna"
    INDEX 01 00:00:00'

  test.equal(
    playlist.parse.cue.full(cue_file),
    {
      tracks=
        [
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=0,
                    minutes=0,
                    file_type="mp3",
                    filename=
                      "Bronski beat - Smalltown boy (12 extended).mp3"
                  }
                )
              ],
            title=
              "Smalltown boy (12 extended)",
            position=1,
            track_type="audio",
            performer=
              "Bronski Beat"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=0,
                    minutes=0,
                    file_type="mp3",
                    filename=
                      "Rihanna - Dont Stop The Music (Ed Marquis Remix).mp3"
                  }
                )
              ],
            title=
              "Dont Stop The Music (Ed Marquis Remix)",
            position=2,
            track_type="audio",
            performer="Rihanna"
          }
        ],
      title=
        "Musique a la carte",
      rem=[("date", "2020"), ("genre", "Electronica")],
      performer=
        "Electronic Mix"
    }
  )

  test.equal(
    playlist.parse.cue(cue_file),
    [
      (
        [
          (
            "album",
            "Musique a la carte"
          ),
          (
            "title",
            "Smalltown boy (12 extended)"
          ),
          (
            "albumartist",
            "Electronic Mix"
          ),
          (
            "artist",
            "Bronski Beat"
          ),
          ("tracknumber", "1"),
          ("year", "2020"),
          ("genre", "Electronica")
        ],
        "Bronski beat - Smalltown boy (12 extended).mp3"
      ),
      (
        [
          (
            "album",
            "Musique a la carte"
          ),
          (
            "title",
            "Dont Stop The Music (Ed Marquis Remix)"
          ),
          (
            "albumartist",
            "Electronic Mix"
          ),
          ("artist", "Rihanna"),
          ("tracknumber", "2"),
          ("year", "2020"),
          ("genre", "Electronica")
        ],
        "Rihanna - Dont Stop The Music (Ed Marquis Remix).mp3"
      )
    ]
  )

  cue_file =
    'PERFORMER "Block Party"
TITLE "Silent Alarm"
FILE "Block Party - Silent Alarm.flac" WAVE
 TRACK 01 AUDIO
   TITLE "Like Eating Glass"
   PERFORMER "Block Party"
   INDEX 00 00:00:00
   INDEX 01 03:22:70
 TRACK 02 AUDIO
   TITLE "Helicopter"
   PERFORMER "Block Party"
   INDEX 00 07:42:69
   INDEX 01 07:44:69'

  test.equal(
    playlist.parse.cue.full(cue_file),
    {
      tracks=
        [
          {
            indexes=
              [
                (
                  0,
                  {
                    frames=0,
                    seconds=0,
                    minutes=0,
                    file_type="wave",
                    filename=
                      "Block Party - Silent Alarm.flac"
                  }
                ),
                (
                  1,
                  {
                    frames=70,
                    seconds=22,
                    minutes=3,
                    file_type="wave",
                    filename=
                      "Block Party - Silent Alarm.flac"
                  }
                )
              ],
            performer=
              "Block Party",
            track_type="audio",
            position=1,
            title=
              "Like Eating Glass"
          },
          {
            indexes=
              [
                (
                  0,
                  {
                    frames=69,
                    seconds=42,
                    minutes=7,
                    file_type="wave",
                    filename=
                      "Block Party - Silent Alarm.flac"
                  }
                ),
                (
                  1,
                  {
                    frames=69,
                    seconds=44,
                    minutes=7,
                    file_type="wave",
                    filename=
                      "Block Party - Silent Alarm.flac"
                  }
                )
              ],
            performer=
              "Block Party",
            track_type="audio",
            position=2,
            title="Helicopter"
          }
        ],
      title=
        "Silent Alarm",
      rem=[],
      performer=
        "Block Party"
    }
  )

  test.equal(
    playlist.parse.cue(cue_file),
    [
      (
        [
          ("liq_cue_in", "202.933333333"),
          ("liq_cue_out", "462.92"),
          (
            "album",
            "Silent Alarm"
          ),
          (
            "title",
            "Like Eating Glass"
          ),
          (
            "albumartist",
            "Block Party"
          ),
          (
            "artist",
            "Block Party"
          ),
          ("tracknumber", "1"),
          (
            "liq_index_zero_filename",
            "Block Party - Silent Alarm.flac"
          ),
          ("liq_index_zero", "0.0")
        ],
        "Block Party - Silent Alarm.flac"
      ),
      (
        [
          ("liq_cue_in", "464.92"),
          (
            "album",
            "Silent Alarm"
          ),
          ("title", "Helicopter"),
          (
            "albumartist",
            "Block Party"
          ),
          (
            "artist",
            "Block Party"
          ),
          ("tracknumber", "2"),
          (
            "liq_index_zero_filename",
            "Block Party - Silent Alarm.flac"
          ),
          ("liq_index_zero", "462.92")
        ],
        "Block Party - Silent Alarm.flac"
      )
    ]
  )

  cue_file =
    'REM GENRE Ska
REM DATE 1991
REM DISCID D00DA810
REM COMMENT "ExactAudioCopy v0.95b4"
PERFORMER "The Specials"
TITLE "Singles"
FILE "The Specials - Singles.wav" WAVE
  TRACK 01 AUDIO
    TITLE "Gangsters"
    PERFORMER "The Specials"
    INDEX 01 00:00:00
  TRACK 02 AUDIO
    TITLE "Rudi, A Message To You"
    PERFORMER "The Specials"
    INDEX 00 02:47:74
    INDEX 01 02:48:27
  TRACK 03 AUDIO
    TITLE "Nite Klub"
    PERFORMER "The Specials"
    INDEX 00 05:41:50
    INDEX 01 05:42:27
  TRACK 04 AUDIO
    TITLE "Too Much Too Young"
    PERFORMER "The Specials"
    INDEX 00 08:53:47
    INDEX 01 08:54:37
  TRACK 05 AUDIO
    TITLE "Guns Of Navarone"
    PERFORMER "The Specials"
    INDEX 00 10:59:20
    INDEX 01 11:00:17
  TRACK 06 AUDIO
    TITLE "Rat Race"
    PERFORMER "The Specials"
    INDEX 00 13:20:55
    INDEX 01 13:20:67
  TRACK 07 AUDIO
    TITLE "Stereotype"
    PERFORMER "The Specials"
    INDEX 00 16:29:67
    INDEX 01 16:30:30
  TRACK 08 AUDIO
    TITLE "International Jet Set"
    PERFORMER "The Specials"
    INDEX 00 20:19:27
    INDEX 01 20:20:20
  TRACK 09 AUDIO
    TITLE "Do Nothing"
    PERFORMER "The Specials"
    INDEX 00 24:30:70
    INDEX 01 24:32:27
  TRACK 10 AUDIO
    TITLE "Ghost Town"
    PERFORMER "The Specials"
    INDEX 00 28:23:30
    INDEX 01 28:23:42
  TRACK 11 AUDIO
    TITLE "Why?"
    PERFORMER "The Specials"
    INDEX 00 34:21:37
    INDEX 01 34:21:47
  TRACK 12 AUDIO
    TITLE "Friday Night, Saturday Morning"
    PERFORMER "The Specials"
    INDEX 00 38:16:50
    INDEX 01 38:16:55
  TRACK 13 AUDIO
    TITLE "War Crimes"
    PERFORMER "The Specials"
    INDEX 00 41:50:07
    INDEX 01 41:51:00
  TRACK 14 AUDIO
    TITLE "Racist Friend"
    PERFORMER "The Specials"
    INDEX 00 45:50:55
    INDEX 01 45:51:72
  TRACK 15 AUDIO
    TITLE "Nelson Mandela"
    PERFORMER "The Specials"
    INDEX 00 49:35:55
    INDEX 01 49:38:22
  TRACK 16 AUDIO
    TITLE "(What I Like Most About You Is Your) Girlfriend"
    PERFORMER "The Specials"
    INDEX 00 54:11:00
    INDEX 01 54:12:40'

  test.equal(
    playlist.parse.cue.full(cue_file),
    {
      tracks=
        [
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=0,
                    minutes=0,
                    file_type="wave",
                    filename=
                      "The Specials - Singles.wav"
                  }
                )
              ],
            title="Gangsters",
            position=1,
            track_type="audio",
            performer=
              "The Specials"
          },
          {
            indexes=
              [
                (
                  0,
                  {
                    frames=74,
                    seconds=47,
                    minutes=2,
                    file_type="wave",
                    filename=
                      "The Specials - Singles.wav"
                  }
                ),
                (
                  1,
                  {
                    frames=27,
                    seconds=48,
                    minutes=2,
                    file_type="wave",
                    filename=
                      "The Specials - Singles.wav"
                  }
                )
              ],
            performer=
              "The Specials",
            track_type="audio",
            position=2,
            title=
              "Rudi, A Message To You"
          },
          {
            indexes=
              [
                (
                  0,
                  {
                    frames=50,
                    seconds=41,
                    minutes=5,
                    file_type="wave",
                    filename=
                      "The Specials - Singles.wav"
                  }
                ),
                (
                  1,
                  {
                    frames=27,
                    seconds=42,
                    minutes=5,
                    file_type="wave",
                    filename=
                      "The Specials - Singles.wav"
                  }
                )
              ],
            performer=
              "The Specials",
            track_type="audio",
            position=3,
            title=
              "Nite Klub"
          },
          {
            indexes=
              [
                (
                  0,
                  {
                    frames=47,
                    seconds=53,
                    minutes=8,
                    file_type="wave",
                    filename=
                      "The Specials - Singles.wav"
                  }
                ),
                (
                  1,
                  {
                    frames=37,
                    seconds=54,
                    minutes=8,
                    file_type="wave",
                    filename=
                      "The Specials - Singles.wav"
                  }
                )
              ],
            performer=
              "The Specials",
            track_type="audio",
            position=4,
            title=
              "Too Much Too Young"
          },
          {
            indexes=
              [
                (
                  0,
                  {
                    frames=20,
                    seconds=59,
                    minutes=10,
                    file_type="wave",
                    filename=
                      "The Specials - Singles.wav"
                  }
                ),
                (
                  1,
                  {
                    frames=17,
                    seconds=0,
                    minutes=11,
                    file_type="wave",
                    filename=
                      "The Specials - Singles.wav"
                  }
                )
              ],
            performer=
              "The Specials",
            track_type="audio",
            position=5,
            title=
              "Guns Of Navarone"
          },
          {
            indexes=
              [
                (
                  0,
                  {
                    frames=55,
                    seconds=20,
                    minutes=13,
                    file_type="wave",
                    filename=
                      "The Specials - Singles.wav"
                  }
                ),
                (
                  1,
                  {
                    frames=67,
                    seconds=20,
                    minutes=13,
                    file_type="wave",
                    filename=
                      "The Specials - Singles.wav"
                  }
                )
              ],
            performer=
              "The Specials",
            track_type="audio",
            position=6,
            title=
              "Rat Race"
          },
          {
            indexes=
              [
                (
                  0,
                  {
                    frames=67,
                    seconds=29,
                    minutes=16,
                    file_type="wave",
                    filename=
                      "The Specials - Singles.wav"
                  }
                ),
                (
                  1,
                  {
                    frames=30,
                    seconds=30,
                    minutes=16,
                    file_type="wave",
                    filename=
                      "The Specials - Singles.wav"
                  }
                )
              ],
            performer=
              "The Specials",
            track_type="audio",
            position=7,
            title="Stereotype"
          },
          {
            indexes=
              [
                (
                  0,
                  {
                    frames=27,
                    seconds=19,
                    minutes=20,
                    file_type="wave",
                    filename=
                      "The Specials - Singles.wav"
                  }
                ),
                (
                  1,
                  {
                    frames=20,
                    seconds=20,
                    minutes=20,
                    file_type="wave",
                    filename=
                      "The Specials - Singles.wav"
                  }
                )
              ],
            performer=
              "The Specials",
            track_type="audio",
            position=8,
            title=
              "International Jet Set"
          },
          {
            indexes=
              [
                (
                  0,
                  {
                    frames=70,
                    seconds=30,
                    minutes=24,
                    file_type="wave",
                    filename=
                      "The Specials - Singles.wav"
                  }
                ),
                (
                  1,
                  {
                    frames=27,
                    seconds=32,
                    minutes=24,
                    file_type="wave",
                    filename=
                      "The Specials - Singles.wav"
                  }
                )
              ],
            performer=
              "The Specials",
            track_type="audio",
            position=9,
            title=
              "Do Nothing"
          },
          {
            indexes=
              [
                (
                  0,
                  {
                    frames=30,
                    seconds=23,
                    minutes=28,
                    file_type="wave",
                    filename=
                      "The Specials - Singles.wav"
                  }
                ),
                (
                  1,
                  {
                    frames=42,
                    seconds=23,
                    minutes=28,
                    file_type="wave",
                    filename=
                      "The Specials - Singles.wav"
                  }
                )
              ],
            performer=
              "The Specials",
            track_type="audio",
            position=10,
            title=
              "Ghost Town"
          },
          {
            indexes=
              [
                (
                  0,
                  {
                    frames=37,
                    seconds=21,
                    minutes=34,
                    file_type="wave",
                    filename=
                      "The Specials - Singles.wav"
                  }
                ),
                (
                  1,
                  {
                    frames=47,
                    seconds=21,
                    minutes=34,
                    file_type="wave",
                    filename=
                      "The Specials - Singles.wav"
                  }
                )
              ],
            performer=
              "The Specials",
            track_type="audio",
            position=11,
            title="Why?"
          },
          {
            indexes=
              [
                (
                  0,
                  {
                    frames=50,
                    seconds=16,
                    minutes=38,
                    file_type="wave",
                    filename=
                      "The Specials - Singles.wav"
                  }
                ),
                (
                  1,
                  {
                    frames=55,
                    seconds=16,
                    minutes=38,
                    file_type="wave",
                    filename=
                      "The Specials - Singles.wav"
                  }
                )
              ],
            performer=
              "The Specials",
            track_type="audio",
            position=12,
            title=
              "Friday Night, Saturday Morning"
          },
          {
            indexes=
              [
                (
                  0,
                  {
                    frames=7,
                    seconds=50,
                    minutes=41,
                    file_type="wave",
                    filename=
                      "The Specials - Singles.wav"
                  }
                ),
                (
                  1,
                  {
                    frames=0,
                    seconds=51,
                    minutes=41,
                    file_type="wave",
                    filename=
                      "The Specials - Singles.wav"
                  }
                )
              ],
            performer=
              "The Specials",
            track_type="audio",
            position=13,
            title=
              "War Crimes"
          },
          {
            indexes=
              [
                (
                  0,
                  {
                    frames=55,
                    seconds=50,
                    minutes=45,
                    file_type="wave",
                    filename=
                      "The Specials - Singles.wav"
                  }
                ),
                (
                  1,
                  {
                    frames=72,
                    seconds=51,
                    minutes=45,
                    file_type="wave",
                    filename=
                      "The Specials - Singles.wav"
                  }
                )
              ],
            performer=
              "The Specials",
            track_type="audio",
            position=14,
            title=
              "Racist Friend"
          },
          {
            indexes=
              [
                (
                  0,
                  {
                    frames=55,
                    seconds=35,
                    minutes=49,
                    file_type="wave",
                    filename=
                      "The Specials - Singles.wav"
                  }
                ),
                (
                  1,
                  {
                    frames=22,
                    seconds=38,
                    minutes=49,
                    file_type="wave",
                    filename=
                      "The Specials - Singles.wav"
                  }
                )
              ],
            performer=
              "The Specials",
            track_type="audio",
            position=15,
            title=
              "Nelson Mandela"
          },
          {
            indexes=
              [
                (
                  0,
                  {
                    frames=0,
                    seconds=11,
                    minutes=54,
                    file_type="wave",
                    filename=
                      "The Specials - Singles.wav"
                  }
                ),
                (
                  1,
                  {
                    frames=40,
                    seconds=12,
                    minutes=54,
                    file_type="wave",
                    filename=
                      "The Specials - Singles.wav"
                  }
                )
              ],
            performer=
              "The Specials",
            track_type="audio",
            position=16,
            title=
              "(What I Like Most About You Is Your) Girlfriend"
          }
        ],
      title="Singles",
      rem=
        [
          (
            "comment",
            "ExactAudioCopy v0.95b4"
          ),
          ("discid", "D00DA810"),
          ("date", "1991"),
          ("genre", "Ska")
        ],
      performer=
        "The Specials"
    }
  )

  test.equal(
    playlist.parse.cue(cue_file),
    [
      (
        [
          ("liq_cue_out", "167.986666667"),
          ("album", "Singles"),
          ("title", "Gangsters"),
          (
            "albumartist",
            "The Specials"
          ),
          (
            "artist",
            "The Specials"
          ),
          ("tracknumber", "1"),
          (
            "comment",
            "ExactAudioCopy v0.95b4"
          ),
          ("discid", "D00DA810"),
          ("year", "1991"),
          ("genre", "Ska")
        ],
        "The Specials - Singles.wav"
      ),
      (
        [
          ("liq_cue_in", "168.36"),
          ("liq_cue_out", "341.666666667"),
          ("album", "Singles"),
          (
            "title",
            "Rudi, A Message To You"
          ),
          (
            "albumartist",
            "The Specials"
          ),
          (
            "artist",
            "The Specials"
          ),
          ("tracknumber", "2"),
          (
            "liq_index_zero_filename",
            "The Specials - Singles.wav"
          ),
          ("liq_index_zero", "167.986666667"),
          (
            "comment",
            "ExactAudioCopy v0.95b4"
          ),
          ("discid", "D00DA810"),
          ("year", "1991"),
          ("genre", "Ska")
        ],
        "The Specials - Singles.wav"
      ),
      (
        [
          ("liq_cue_in", "342.36"),
          ("liq_cue_out", "533.626666667"),
          ("album", "Singles"),
          (
            "title",
            "Nite Klub"
          ),
          (
            "albumartist",
            "The Specials"
          ),
          (
            "artist",
            "The Specials"
          ),
          ("tracknumber", "3"),
          (
            "liq_index_zero_filename",
            "The Specials - Singles.wav"
          ),
          ("liq_index_zero", "341.666666667"),
          (
            "comment",
            "ExactAudioCopy v0.95b4"
          ),
          ("discid", "D00DA810"),
          ("year", "1991"),
          ("genre", "Ska")
        ],
        "The Specials - Singles.wav"
      ),
      (
        [
          ("liq_cue_in", "534.493333333"),
          ("liq_cue_out", "659.266666667"),
          ("album", "Singles"),
          (
            "title",
            "Too Much Too Young"
          ),
          (
            "albumartist",
            "The Specials"
          ),
          (
            "artist",
            "The Specials"
          ),
          ("tracknumber", "4"),
          (
            "liq_index_zero_filename",
            "The Specials - Singles.wav"
          ),
          ("liq_index_zero", "533.626666667"),
          (
            "comment",
            "ExactAudioCopy v0.95b4"
          ),
          ("discid", "D00DA810"),
          ("year", "1991"),
          ("genre", "Ska")
        ],
        "The Specials - Singles.wav"
      ),
      (
        [
          ("liq_cue_in", "660.226666667"),
          ("liq_cue_out", "800.733333333"),
          ("album", "Singles"),
          (
            "title",
            "Guns Of Navarone"
          ),
          (
            "albumartist",
            "The Specials"
          ),
          (
            "artist",
            "The Specials"
          ),
          ("tracknumber", "5"),
          (
            "liq_index_zero_filename",
            "The Specials - Singles.wav"
          ),
          ("liq_index_zero", "659.266666667"),
          (
            "comment",
            "ExactAudioCopy v0.95b4"
          ),
          ("discid", "D00DA810"),
          ("year", "1991"),
          ("genre", "Ska")
        ],
        "The Specials - Singles.wav"
      ),
      (
        [
          ("liq_cue_in", "800.893333333"),
          ("liq_cue_out", "989.893333333"),
          ("album", "Singles"),
          (
            "title",
            "Rat Race"
          ),
          (
            "albumartist",
            "The Specials"
          ),
          (
            "artist",
            "The Specials"
          ),
          ("tracknumber", "6"),
          (
            "liq_index_zero_filename",
            "The Specials - Singles.wav"
          ),
          ("liq_index_zero", "800.733333333"),
          (
            "comment",
            "ExactAudioCopy v0.95b4"
          ),
          ("discid", "D00DA810"),
          ("year", "1991"),
          ("genre", "Ska")
        ],
        "The Specials - Singles.wav"
      ),
      (
        [
          ("liq_cue_in", "990.4"),
          ("liq_cue_out", "1219.36"),
          ("album", "Singles"),
          ("title", "Stereotype"),
          (
            "albumartist",
            "The Specials"
          ),
          (
            "artist",
            "The Specials"
          ),
          ("tracknumber", "7"),
          (
            "liq_index_zero_filename",
            "The Specials - Singles.wav"
          ),
          ("liq_index_zero", "989.893333333"),
          (
            "comment",
            "ExactAudioCopy v0.95b4"
          ),
          ("discid", "D00DA810"),
          ("year", "1991"),
          ("genre", "Ska")
        ],
        "The Specials - Singles.wav"
      ),
      (
        [
          ("liq_cue_in", "1220.26666667"),
          ("liq_cue_out", "1470.93333333"),
          ("album", "Singles"),
          (
            "title",
            "International Jet Set"
          ),
          (
            "albumartist",
            "The Specials"
          ),
          (
            "artist",
            "The Specials"
          ),
          ("tracknumber", "8"),
          (
            "liq_index_zero_filename",
            "The Specials - Singles.wav"
          ),
          ("liq_index_zero", "1219.36"),
          (
            "comment",
            "ExactAudioCopy v0.95b4"
          ),
          ("discid", "D00DA810"),
          ("year", "1991"),
          ("genre", "Ska")
        ],
        "The Specials - Singles.wav"
      ),
      (
        [
          ("liq_cue_in", "1472.36"),
          ("liq_cue_out", "1703.4"),
          ("album", "Singles"),
          (
            "title",
            "Do Nothing"
          ),
          (
            "albumartist",
            "The Specials"
          ),
          (
            "artist",
            "The Specials"
          ),
          ("tracknumber", "9"),
          (
            "liq_index_zero_filename",
            "The Specials - Singles.wav"
          ),
          ("liq_index_zero", "1470.93333333"),
          (
            "comment",
            "ExactAudioCopy v0.95b4"
          ),
          ("discid", "D00DA810"),
          ("year", "1991"),
          ("genre", "Ska")
        ],
        "The Specials - Singles.wav"
      ),
      (
        [
          ("liq_cue_in", "1703.56"),
          ("liq_cue_out", "2061.49333333"),
          ("album", "Singles"),
          (
            "title",
            "Ghost Town"
          ),
          (
            "albumartist",
            "The Specials"
          ),
          (
            "artist",
            "The Specials"
          ),
          ("tracknumber", "10"),
          (
            "liq_index_zero_filename",
            "The Specials - Singles.wav"
          ),
          ("liq_index_zero", "1703.4"),
          (
            "comment",
            "ExactAudioCopy v0.95b4"
          ),
          ("discid", "D00DA810"),
          ("year", "1991"),
          ("genre", "Ska")
        ],
        "The Specials - Singles.wav"
      ),
      (
        [
          ("liq_cue_in", "2061.62666667"),
          ("liq_cue_out", "2296.66666667"),
          ("album", "Singles"),
          ("title", "Why?"),
          (
            "albumartist",
            "The Specials"
          ),
          (
            "artist",
            "The Specials"
          ),
          ("tracknumber", "11"),
          (
            "liq_index_zero_filename",
            "The Specials - Singles.wav"
          ),
          ("liq_index_zero", "2061.49333333"),
          (
            "comment",
            "ExactAudioCopy v0.95b4"
          ),
          ("discid", "D00DA810"),
          ("year", "1991"),
          ("genre", "Ska")
        ],
        "The Specials - Singles.wav"
      ),
      (
        [
          ("liq_cue_in", "2296.73333333"),
          ("liq_cue_out", "2510.09333333"),
          ("album", "Singles"),
          (
            "title",
            "Friday Night, Saturday Morning"
          ),
          (
            "albumartist",
            "The Specials"
          ),
          (
            "artist",
            "The Specials"
          ),
          ("tracknumber", "12"),
          (
            "liq_index_zero_filename",
            "The Specials - Singles.wav"
          ),
          ("liq_index_zero", "2296.66666667"),
          (
            "comment",
            "ExactAudioCopy v0.95b4"
          ),
          ("discid", "D00DA810"),
          ("year", "1991"),
          ("genre", "Ska")
        ],
        "The Specials - Singles.wav"
      ),
      (
        [
          ("liq_cue_in", "2511.0"),
          ("liq_cue_out", "2750.73333333"),
          ("album", "Singles"),
          (
            "title",
            "War Crimes"
          ),
          (
            "albumartist",
            "The Specials"
          ),
          (
            "artist",
            "The Specials"
          ),
          ("tracknumber", "13"),
          (
            "liq_index_zero_filename",
            "The Specials - Singles.wav"
          ),
          ("liq_index_zero", "2510.09333333"),
          (
            "comment",
            "ExactAudioCopy v0.95b4"
          ),
          ("discid", "D00DA810"),
          ("year", "1991"),
          ("genre", "Ska")
        ],
        "The Specials - Singles.wav"
      ),
      (
        [
          ("liq_cue_in", "2751.96"),
          ("liq_cue_out", "2975.73333333"),
          ("album", "Singles"),
          (
            "title",
            "Racist Friend"
          ),
          (
            "albumartist",
            "The Specials"
          ),
          (
            "artist",
            "The Specials"
          ),
          ("tracknumber", "14"),
          (
            "liq_index_zero_filename",
            "The Specials - Singles.wav"
          ),
          ("liq_index_zero", "2750.73333333"),
          (
            "comment",
            "ExactAudioCopy v0.95b4"
          ),
          ("discid", "D00DA810"),
          ("year", "1991"),
          ("genre", "Ska")
        ],
        "The Specials - Singles.wav"
      ),
      (
        [
          ("liq_cue_in", "2978.29333333"),
          ("liq_cue_out", "3251.0"),
          ("album", "Singles"),
          (
            "title",
            "Nelson Mandela"
          ),
          (
            "albumartist",
            "The Specials"
          ),
          (
            "artist",
            "The Specials"
          ),
          ("tracknumber", "15"),
          (
            "liq_index_zero_filename",
            "The Specials - Singles.wav"
          ),
          ("liq_index_zero", "2975.73333333"),
          (
            "comment",
            "ExactAudioCopy v0.95b4"
          ),
          ("discid", "D00DA810"),
          ("year", "1991"),
          ("genre", "Ska")
        ],
        "The Specials - Singles.wav"
      ),
      (
        [
          ("liq_cue_in", "3252.53333333"),
          ("album", "Singles"),
          (
            "title",
            "(What I Like Most About You Is Your) Girlfriend"
          ),
          (
            "albumartist",
            "The Specials"
          ),
          (
            "artist",
            "The Specials"
          ),
          ("tracknumber", "16"),
          (
            "liq_index_zero_filename",
            "The Specials - Singles.wav"
          ),
          ("liq_index_zero", "3251.0"),
          (
            "comment",
            "ExactAudioCopy v0.95b4"
          ),
          ("discid", "D00DA810"),
          ("year", "1991"),
          ("genre", "Ska")
        ],
        "The Specials - Singles.wav"
      )
    ]
  )

  cue_file =
    'FILE "The Specials - Singles - 01 - Gangsters.wav" WAVE
  TRACK 01 AUDIO
    TITLE "Gangsters"
    PERFORMER "The Specials"
    INDEX 01 00:00:00
FILE "The Specials - Singles - 02 - Rudi, A Message To You.wav" WAVE
  TRACK 02 AUDIO
    TITLE "Rudi, A Message To You"
    PERFORMER "The Specials"
    PREGAP 00:00:28
    INDEX 01 00:00:00'

  test.equal(
    playlist.parse.cue.full(cue_file),
    {
      tracks=
        [
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=0,
                    minutes=0,
                    file_type="wave",
                    filename=
                      "The Specials - Singles - 01 - Gangsters.wav"
                  }
                )
              ],
            title="Gangsters",
            position=1,
            track_type="audio",
            performer=
              "The Specials"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=0,
                    minutes=0,
                    file_type="wave",
                    filename=
                      "The Specials - Singles - 02 - Rudi, A Message To You.wav"
                  }
                )
              ],
            performer=
              "The Specials",
            track_type="audio",
            position=2,
            title=
              "Rudi, A Message To You",
            pregap={minutes=0, seconds=0, frames=28}
          }
        ],
      rem=[]
    }
  )

  test.equal(
    playlist.parse.cue(cue_file),
    [
      (
        [
          ("title", "Gangsters"),
          (
            "artist",
            "The Specials"
          ),
          ("tracknumber", "1")
        ],
        "The Specials - Singles - 01 - Gangsters.wav"
      ),
      (
        [
          (
            "title",
            "Rudi, A Message To You"
          ),
          (
            "artist",
            "The Specials"
          ),
          ("tracknumber", "2"),
          ("liq_pregap", "0.373333333333")
        ],
        "The Specials - Singles - 02 - Rudi, A Message To You.wav"
      )
    ]
  )

  cue_file =
    'REM GENRE Rap/Metal
REM DATE 1996
REM DISCID A30AEF0B
REM COMMENT "ExactAudioCopy v1.0b3"
PERFORMER "Rage Against The Machine"
TITLE "Evil Empire"
FILE "01 People Of The Sun.flac" WAVE
  TRACK 01 AUDIO
    TITLE "People Of The Sun"
    PERFORMER "Rage Against The Machine"
    ISRC USSM19502587
    INDEX 01 00:00:00
FILE "02 Bulls On Parade.flac" WAVE
  TRACK 02 AUDIO
    TITLE "Bulls On Parade"
    PERFORMER "Rage Against The Machine"
    ISRC USSM19502563
    INDEX 01 00:00:00
  TRACK 03 AUDIO
    TITLE "Vietnow"
    PERFORMER "Rage Against The Machine"
    ISRC USSM19502588
    INDEX 00 03:49:52
FILE "03 Vietnow.flac" WAVE
    INDEX 01 00:00:00
  TRACK 04 AUDIO
    TITLE "Revolver"
    PERFORMER "Rage Against The Machine"
    ISRC USSM19502589
    INDEX 00 04:37:42
FILE "04 Revolver.flac" WAVE
    INDEX 01 00:00:00
  TRACK 05 AUDIO
    TITLE "Snakecharmer"
    PERFORMER "Rage Against The Machine"
    ISRC USSM19502590
    INDEX 00 05:29:18
FILE "05 Snakecharmer.flac" WAVE
    INDEX 01 00:00:00
  TRACK 06 AUDIO
    TITLE "Tire Me"
    PERFORMER "Rage Against The Machine"
    ISRC USSM19502591
    INDEX 00 03:54:43
FILE "06 Tire Me.flac" WAVE
    INDEX 01 00:00:00
  TRACK 07 AUDIO
    TITLE "Down Rodeo"
    PERFORMER "Rage Against The Machine"
    ISRC USSM19502592
    INDEX 00 02:57:73
FILE "07 Down Rodeo.flac" WAVE
    INDEX 01 00:00:00
FILE "08 Without A Face.flac" WAVE
  TRACK 08 AUDIO
    TITLE "Without A Face"
    PERFORMER "Rage Against The Machine"
    ISRC USSM19502594
    INDEX 01 00:00:00
FILE "09 Wind Below.flac" WAVE
  TRACK 09 AUDIO
    TITLE "Wind Below"
    PERFORMER "Rage Against The Machine"
    ISRC USSM19502593
    INDEX 01 00:00:00
FILE "10 Roll Right.flac" WAVE
  TRACK 10 AUDIO
    TITLE "Roll Right"
    PERFORMER "Rage Against The Machine"
    ISRC USSM19502595
    INDEX 01 00:00:00
  TRACK 11 AUDIO
    TITLE "Year Of Tha Boomerang"
    PERFORMER "Rage Against The Machine"
    ISRC USSM19502596
    INDEX 00 04:20:38
FILE "11 Year Of Tha Boomerang.flac" WAVE
    INDEX 01 00:00:00'

  test.equal(
    playlist.parse.cue.full(cue_file),
    {
      tracks=
        [
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=0,
                    minutes=0,
                    file_type="wave",
                    filename=
                      "01 People Of The Sun.flac"
                  }
                )
              ],
            performer=
              "Rage Against The Machine",
            track_type="audio",
            position=1,
            title=
              "People Of The Sun",
            isrc="USSM19502587"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=0,
                    minutes=0,
                    file_type="wave",
                    filename=
                      "02 Bulls On Parade.flac"
                  }
                )
              ],
            performer=
              "Rage Against The Machine",
            track_type="audio",
            position=2,
            title=
              "Bulls On Parade",
            isrc="USSM19502563"
          },
          {
            indexes=
              [
                (
                  0,
                  {
                    frames=52,
                    seconds=49,
                    minutes=3,
                    file_type="wave",
                    filename=
                      "02 Bulls On Parade.flac"
                  }
                ),
                (
                  1,
                  {
                    frames=0,
                    seconds=0,
                    minutes=0,
                    file_type="wave",
                    filename=
                      "03 Vietnow.flac"
                  }
                )
              ],
            isrc="USSM19502588",
            title="Vietnow",
            position=3,
            track_type="audio",
            performer=
              "Rage Against The Machine"
          },
          {
            indexes=
              [
                (
                  0,
                  {
                    frames=42,
                    seconds=37,
                    minutes=4,
                    file_type="wave",
                    filename=
                      "03 Vietnow.flac"
                  }
                ),
                (
                  1,
                  {
                    frames=0,
                    seconds=0,
                    minutes=0,
                    file_type="wave",
                    filename=
                      "04 Revolver.flac"
                  }
                )
              ],
            isrc="USSM19502589",
            title="Revolver",
            position=4,
            track_type="audio",
            performer=
              "Rage Against The Machine"
          },
          {
            indexes=
              [
                (
                  0,
                  {
                    frames=18,
                    seconds=29,
                    minutes=5,
                    file_type="wave",
                    filename=
                      "04 Revolver.flac"
                  }
                ),
                (
                  1,
                  {
                    frames=0,
                    seconds=0,
                    minutes=0,
                    file_type="wave",
                    filename=
                      "05 Snakecharmer.flac"
                  }
                )
              ],
            isrc="USSM19502590",
            title="Snakecharmer",
            position=5,
            track_type="audio",
            performer=
              "Rage Against The Machine"
          },
          {
            indexes=
              [
                (
                  0,
                  {
                    frames=43,
                    seconds=54,
                    minutes=3,
                    file_type="wave",
                    filename=
                      "05 Snakecharmer.flac"
                  }
                ),
                (
                  1,
                  {
                    frames=0,
                    seconds=0,
                    minutes=0,
                    file_type="wave",
                    filename=
                      "06 Tire Me.flac"
                  }
                )
              ],
            isrc="USSM19502591",
            title=
              "Tire Me",
            position=6,
            track_type="audio",
            performer=
              "Rage Against The Machine"
          },
          {
            indexes=
              [
                (
                  0,
                  {
                    frames=73,
                    seconds=57,
                    minutes=2,
                    file_type="wave",
                    filename=
                      "06 Tire Me.flac"
                  }
                ),
                (
                  1,
                  {
                    frames=0,
                    seconds=0,
                    minutes=0,
                    file_type="wave",
                    filename=
                      "07 Down Rodeo.flac"
                  }
                )
              ],
            isrc="USSM19502592",
            title=
              "Down Rodeo",
            position=7,
            track_type="audio",
            performer=
              "Rage Against The Machine"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=0,
                    minutes=0,
                    file_type="wave",
                    filename=
                      "08 Without A Face.flac"
                  }
                )
              ],
            performer=
              "Rage Against The Machine",
            track_type="audio",
            position=8,
            title=
              "Without A Face",
            isrc="USSM19502594"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=0,
                    minutes=0,
                    file_type="wave",
                    filename=
                      "09 Wind Below.flac"
                  }
                )
              ],
            performer=
              "Rage Against The Machine",
            track_type="audio",
            position=9,
            title=
              "Wind Below",
            isrc="USSM19502593"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=0,
                    minutes=0,
                    file_type="wave",
                    filename=
                      "10 Roll Right.flac"
                  }
                )
              ],
            performer=
              "Rage Against The Machine",
            track_type="audio",
            position=10,
            title=
              "Roll Right",
            isrc="USSM19502595"
          },
          {
            indexes=
              [
                (
                  0,
                  {
                    frames=38,
                    seconds=20,
                    minutes=4,
                    file_type="wave",
                    filename=
                      "10 Roll Right.flac"
                  }
                ),
                (
                  1,
                  {
                    frames=0,
                    seconds=0,
                    minutes=0,
                    file_type="wave",
                    filename=
                      "11 Year Of Tha Boomerang.flac"
                  }
                )
              ],
            isrc="USSM19502596",
            title=
              "Year Of Tha Boomerang",
            position=11,
            track_type="audio",
            performer=
              "Rage Against The Machine"
          }
        ],
      performer=
        "Rage Against The Machine",
      rem=
        [
          (
            "comment",
            "ExactAudioCopy v1.0b3"
          ),
          ("discid", "A30AEF0B"),
          ("date", "1996"),
          ("genre", "Rap/Metal")
        ],
      title=
        "Evil Empire"
    }
  )

  test.equal(
    playlist.parse.cue(cue_file),
    [
      (
        [
          (
            "album",
            "Evil Empire"
          ),
          ("isrc", "USSM19502587"),
          (
            "title",
            "People Of The Sun"
          ),
          (
            "albumartist",
            "Rage Against The Machine"
          ),
          (
            "artist",
            "Rage Against The Machine"
          ),
          ("tracknumber", "1"),
          (
            "comment",
            "ExactAudioCopy v1.0b3"
          ),
          ("discid", "A30AEF0B"),
          ("year", "1996"),
          ("genre", "Rap/Metal")
        ],
        "01 People Of The Sun.flac"
      ),
      (
        [
          (
            "album",
            "Evil Empire"
          ),
          ("isrc", "USSM19502563"),
          (
            "title",
            "Bulls On Parade"
          ),
          (
            "albumartist",
            "Rage Against The Machine"
          ),
          (
            "artist",
            "Rage Against The Machine"
          ),
          ("tracknumber", "2"),
          (
            "comment",
            "ExactAudioCopy v1.0b3"
          ),
          ("discid", "A30AEF0B"),
          ("year", "1996"),
          ("genre", "Rap/Metal")
        ],
        "02 Bulls On Parade.flac"
      ),
      (
        [
          (
            "album",
            "Evil Empire"
          ),
          ("isrc", "USSM19502588"),
          ("title", "Vietnow"),
          (
            "albumartist",
            "Rage Against The Machine"
          ),
          (
            "artist",
            "Rage Against The Machine"
          ),
          ("tracknumber", "3"),
          (
            "liq_index_zero_filename",
            "02 Bulls On Parade.flac"
          ),
          ("liq_index_zero", "229.693333333"),
          (
            "comment",
            "ExactAudioCopy v1.0b3"
          ),
          ("discid", "A30AEF0B"),
          ("year", "1996"),
          ("genre", "Rap/Metal")
        ],
        "03 Vietnow.flac"
      ),
      (
        [
          (
            "album",
            "Evil Empire"
          ),
          ("isrc", "USSM19502589"),
          ("title", "Revolver"),
          (
            "albumartist",
            "Rage Against The Machine"
          ),
          (
            "artist",
            "Rage Against The Machine"
          ),
          ("tracknumber", "4"),
          (
            "liq_index_zero_filename",
            "03 Vietnow.flac"
          ),
          ("liq_index_zero", "277.56"),
          (
            "comment",
            "ExactAudioCopy v1.0b3"
          ),
          ("discid", "A30AEF0B"),
          ("year", "1996"),
          ("genre", "Rap/Metal")
        ],
        "04 Revolver.flac"
      ),
      (
        [
          (
            "album",
            "Evil Empire"
          ),
          ("isrc", "USSM19502590"),
          ("title", "Snakecharmer"),
          (
            "albumartist",
            "Rage Against The Machine"
          ),
          (
            "artist",
            "Rage Against The Machine"
          ),
          ("tracknumber", "5"),
          (
            "liq_index_zero_filename",
            "04 Revolver.flac"
          ),
          ("liq_index_zero", "329.24"),
          (
            "comment",
            "ExactAudioCopy v1.0b3"
          ),
          ("discid", "A30AEF0B"),
          ("year", "1996"),
          ("genre", "Rap/Metal")
        ],
        "05 Snakecharmer.flac"
      ),
      (
        [
          (
            "album",
            "Evil Empire"
          ),
          ("isrc", "USSM19502591"),
          (
            "title",
            "Tire Me"
          ),
          (
            "albumartist",
            "Rage Against The Machine"
          ),
          (
            "artist",
            "Rage Against The Machine"
          ),
          ("tracknumber", "6"),
          (
            "liq_index_zero_filename",
            "05 Snakecharmer.flac"
          ),
          ("liq_index_zero", "234.573333333"),
          (
            "comment",
            "ExactAudioCopy v1.0b3"
          ),
          ("discid", "A30AEF0B"),
          ("year", "1996"),
          ("genre", "Rap/Metal")
        ],
        "06 Tire Me.flac"
      ),
      (
        [
          (
            "album",
            "Evil Empire"
          ),
          ("isrc", "USSM19502592"),
          (
            "title",
            "Down Rodeo"
          ),
          (
            "albumartist",
            "Rage Against The Machine"
          ),
          (
            "artist",
            "Rage Against The Machine"
          ),
          ("tracknumber", "7"),
          (
            "liq_index_zero_filename",
            "06 Tire Me.flac"
          ),
          ("liq_index_zero", "177.973333333"),
          (
            "comment",
            "ExactAudioCopy v1.0b3"
          ),
          ("discid", "A30AEF0B"),
          ("year", "1996"),
          ("genre", "Rap/Metal")
        ],
        "07 Down Rodeo.flac"
      ),
      (
        [
          (
            "album",
            "Evil Empire"
          ),
          ("isrc", "USSM19502594"),
          (
            "title",
            "Without A Face"
          ),
          (
            "albumartist",
            "Rage Against The Machine"
          ),
          (
            "artist",
            "Rage Against The Machine"
          ),
          ("tracknumber", "8"),
          (
            "comment",
            "ExactAudioCopy v1.0b3"
          ),
          ("discid", "A30AEF0B"),
          ("year", "1996"),
          ("genre", "Rap/Metal")
        ],
        "08 Without A Face.flac"
      ),
      (
        [
          (
            "album",
            "Evil Empire"
          ),
          ("isrc", "USSM19502593"),
          (
            "title",
            "Wind Below"
          ),
          (
            "albumartist",
            "Rage Against The Machine"
          ),
          (
            "artist",
            "Rage Against The Machine"
          ),
          ("tracknumber", "9"),
          (
            "comment",
            "ExactAudioCopy v1.0b3"
          ),
          ("discid", "A30AEF0B"),
          ("year", "1996"),
          ("genre", "Rap/Metal")
        ],
        "09 Wind Below.flac"
      ),
      (
        [
          (
            "album",
            "Evil Empire"
          ),
          ("isrc", "USSM19502595"),
          (
            "title",
            "Roll Right"
          ),
          (
            "albumartist",
            "Rage Against The Machine"
          ),
          (
            "artist",
            "Rage Against The Machine"
          ),
          ("tracknumber", "10"),
          (
            "comment",
            "ExactAudioCopy v1.0b3"
          ),
          ("discid", "A30AEF0B"),
          ("year", "1996"),
          ("genre", "Rap/Metal")
        ],
        "10 Roll Right.flac"
      ),
      (
        [
          (
            "album",
            "Evil Empire"
          ),
          ("isrc", "USSM19502596"),
          (
            "title",
            "Year Of Tha Boomerang"
          ),
          (
            "albumartist",
            "Rage Against The Machine"
          ),
          (
            "artist",
            "Rage Against The Machine"
          ),
          ("tracknumber", "11"),
          (
            "liq_index_zero_filename",
            "10 Roll Right.flac"
          ),
          ("liq_index_zero", "260.506666667"),
          (
            "comment",
            "ExactAudioCopy v1.0b3"
          ),
          ("discid", "A30AEF0B"),
          ("year", "1996"),
          ("genre", "Rap/Metal")
        ],
        "11 Year Of Tha Boomerang.flac"
      )
    ]
  )

  cue_file =
    'FILE "/home/radio/firepit/StandardTime.mp3" MP3
TRACK 01 AUDIO
TITLE "Moonlight Serenade"
PERFORMER "Big Bands In Hi-Fi"
REM ALBUM "Volume 1 : Let\'s Dance - (Disc2"
INDEX 01 00:00:00
TRACK 02 AUDIO
TITLE "The Laughs are on me from Frank Sinatra Balids"
PERFORMER "frank sinatra"
INDEX 01 03:42:00
TRACK 03 AUDIO
TITLE "It Was A Very Good Year"
PERFORMER "Frank Sinatra"
REM ALBUM "Anthology - Frank Sinatra: Collection"
INDEX 01 07:09:00
TRACK 04 AUDIO
TITLE "It Was A Very Good Year"
PERFORMER ""
REM ALBUM "The Capitol Years (Disc 2)"
INDEX 01 12:07:00
TRACK 05 AUDIO
TITLE "I\'m Old Fashioned (Kern-Mercer)"
PERFORMER "Marian McPartland"
REM ALBUM "Personal Choice"
INDEX 01 15:42:00
TRACK 06 AUDIO
TITLE "You Took Advantage Of Me"
PERFORMER "Rosemary Clooney"
REM ALBUM "Rosie Solves The Swingin\' Riddle! (Vinyl)"
INDEX 01 19:16:00
TRACK 07 AUDIO
TITLE "Pennies from Heaven"
PERFORMER "Rosemary Clooney"
REM ALBUM "From Bing to Billie"
INDEX 01 22:12:00
TRACK 08 AUDIO
TITLE "Getting Some Fun Out Of Life"
PERFORMER "Billie Holiday"
REM ALBUM "Live - Billie Holiday: 1936-1949 - Just Jazz with Ed Beach"
INDEX 01 26:42:00
TRACK 09 AUDIO
TITLE "It\'s Been A Long, Long Time"
PERFORMER "Bing Crosby"
REM ALBUM "The Definitive Collection"
INDEX 01 29:47:00
TRACK 10 AUDIO
TITLE "Gone Fishin\' (Duet With Bing Crosby)"
PERFORMER "Louis Armstrong"
REM ALBUM "The Definitive Collection"
INDEX 01 32:43:00
TRACK 11 AUDIO
TITLE "Moonglow"
PERFORMER "Diana Krall"
REM ALBUM "Turn Up The Quiet"
INDEX 01 35:53:00
TRACK 12 AUDIO
TITLE "Hello, Young Lovers"
PERFORMER "Perry Como"
INDEX 01 41:39:00
TRACK 13 AUDIO
TITLE "I\'d be but a slave to you perry como"
PERFORMER "Perry Como"
INDEX 01 44:16:00
TRACK 14 AUDIO
TITLE "It Isn\'t Fair"
PERFORMER "Dinah Washington"
REM ALBUM "The Dinah Washington Story"
INDEX 01 47:36:00
TRACK 15 AUDIO
TITLE "Mona Lisa"
PERFORMER "Tommy Emmanuel"
REM ALBUM "Endless Road"
INDEX 01 51:07:00
TRACK 16 AUDIO
TITLE "Don\'t Explain"
PERFORMER "June Christy"
REM ALBUM "Anthology - VA: Lady Sings The Blues"
INDEX 01 55:04:00
TRACK 17 AUDIO
TITLE "Milord"
PERFORMER "Bobby Darin"
REM ALBUM "The Bobby Darin Collection (Disc 2) - The Pop Years - Part One"
INDEX 01 57:48:00
TRACK 18 AUDIO
TITLE "A Fine Romance"
PERFORMER "Fred Astaire"
REM ALBUM "Steppin\'Out: Astaire Sings"
INDEX 01 59:49:00
TRACK 19 AUDIO
TITLE "Dreamer"
PERFORMER "Stacey Kent"
REM ALBUM "Dreamer In Concert"
INDEX 01 63:58:00
TRACK 20 AUDIO
TITLE "Jardin D\'hiver"
PERFORMER "Stacey Kent"
REM ALBUM "Dreamer In Concert"
INDEX 01 69:04:00
TRACK 21 AUDIO
TITLE "Maybe This Time [From Cabaret]"
PERFORMER "Tony Bennett"
REM ALBUM "Forty Years: The Artistry of Tony Bennett Disc 4"
INDEX 01 73:30:00
TRACK 22 AUDIO
TITLE "If I Ruled The World 3:00"
PERFORMER "Tony Bennett"
REM ALBUM "Very Best of"
INDEX 01 77:33:00
TRACK 23 AUDIO
TITLE "Be Careful It\'s My Heart"
PERFORMER "Tommy Dorsey"
REM ALBUM "Sentimental Memories With Vocals By Frank Sinatra"
INDEX 01 80:49:00
TRACK 24 AUDIO
TITLE "You Made Me Love You (I Didn\'t Want To Do It)"
PERFORMER "Day, Doris"
REM ALBUM "The Essential Love Songs"
INDEX 01 83:55:00
TRACK 25 AUDIO
TITLE "Li\'l Darlin\'"
PERFORMER "Frank Capp"
REM ALBUM "In a Hefti Bag"
INDEX 01 86:41:00
TRACK 26 AUDIO
TITLE "Standard Time"
PERFORMER ""
INDEX 01 91:37:00
TRACK 27 AUDIO
TITLE "Soundtrack Of Your Life"
PERFORMER ""
INDEX 01 91:41:00
TRACK 28 AUDIO
TITLE "Misty"
PERFORMER "Johnny Mathis"
REM ALBUM "The Essential Johnny Mathis Disc 1"
INDEX 01 91:46:00
TRACK 29 AUDIO
TITLE "Soundtrack Of Your Life"
PERFORMER ""
INDEX 01 95:19:00
TRACK 30 AUDIO
TITLE "Standard Time"
PERFORMER ""
INDEX 01 95:25:00
TRACK 31 AUDIO
TITLE "Don\'t worry bout me"
PERFORMER "Tierny Sutton"
INDEX 01 95:39:00
TRACK 32 AUDIO
TITLE "What I Did For Love"
PERFORMER "Steve Lawrence, Eydie Gorme"
REM ALBUM "The Hits of Steve Lawrence & Eydie Gorme"
INDEX 01 100:23:00
TRACK 33 AUDIO
TITLE "Woodchopper\'s Ball"
PERFORMER "Woody Herman"
REM ALBUM "Woodchopper\'s Ball"
INDEX 01 104:47:00
TRACK 34 AUDIO
TITLE "There\'s No You"
PERFORMER "Jo Stafford"
REM ALBUM "Fools Rush In"
INDEX 01 108:12:00
TRACK 35 AUDIO
TITLE "It Looks Like Love"
PERFORMER "Dean Martin"
REM ALBUM "Essential Love Songs [+digital booklet]"
INDEX 01 111:33:00
TRACK 36 AUDIO
TITLE "12-sammy_davis_jr-too_close_for_comfort"
PERFORMER ""
REM ALBUM "The Reprise Years"
INDEX 01 113:56:00
TRACK 37 AUDIO
TITLE "La Vie En Rose"
PERFORMER "Pete Fountain"
REM ALBUM "A Touch Of Class"
INDEX 01 117:19:00'

  test.equal(
    playlist.parse.cue.full(cue_file),
    {
      tracks=
        [
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=0,
                    minutes=0,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            performer=
              "Big Bands In Hi-Fi",
            track_type="audio",
            position=1,
            title=
              "Moonlight Serenade",
            album=
              "Volume 1 : Let's Dance - (Disc2"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=42,
                    minutes=3,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            title=
              "The Laughs are on me from Frank Sinatra Balids",
            position=2,
            track_type="audio",
            performer=
              "frank sinatra"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=9,
                    minutes=7,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            performer=
              "Frank Sinatra",
            track_type="audio",
            position=3,
            title=
              "It Was A Very Good Year",
            album=
              "Anthology - Frank Sinatra: Collection"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=7,
                    minutes=12,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            performer="",
            track_type="audio",
            position=4,
            title=
              "It Was A Very Good Year",
            album=
              "The Capitol Years (Disc 2)"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=42,
                    minutes=15,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            performer=
              "Marian McPartland",
            track_type="audio",
            position=5,
            title=
              "I'm Old Fashioned (Kern-Mercer)",
            album=
              "Personal Choice"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=16,
                    minutes=19,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            performer=
              "Rosemary Clooney",
            track_type="audio",
            position=6,
            title=
              "You Took Advantage Of Me",
            album=
              "Rosie Solves The Swingin' Riddle! (Vinyl)"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=12,
                    minutes=22,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            performer=
              "Rosemary Clooney",
            track_type="audio",
            position=7,
            title=
              "Pennies from Heaven",
            album=
              "From Bing to Billie"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=42,
                    minutes=26,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            performer=
              "Billie Holiday",
            track_type="audio",
            position=8,
            title=
              "Getting Some Fun Out Of Life",
            album=
              "Live - Billie Holiday: 1936-1949 - Just Jazz with Ed Beach"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=47,
                    minutes=29,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            performer=
              "Bing Crosby",
            track_type="audio",
            position=9,
            title=
              "It's Been A Long, Long Time",
            album=
              "The Definitive Collection"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=43,
                    minutes=32,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            performer=
              "Louis Armstrong",
            track_type="audio",
            position=10,
            title=
              "Gone Fishin' (Duet With Bing Crosby)",
            album=
              "The Definitive Collection"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=53,
                    minutes=35,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            performer=
              "Diana Krall",
            track_type="audio",
            position=11,
            title="Moonglow",
            album=
              "Turn Up The Quiet"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=39,
                    minutes=41,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            title=
              "Hello, Young Lovers",
            position=12,
            track_type="audio",
            performer=
              "Perry Como"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=16,
                    minutes=44,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            title=
              "I'd be but a slave to you perry como",
            position=13,
            track_type="audio",
            performer=
              "Perry Como"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=36,
                    minutes=47,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            performer=
              "Dinah Washington",
            track_type="audio",
            position=14,
            title=
              "It Isn't Fair",
            album=
              "The Dinah Washington Story"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=7,
                    minutes=51,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            performer=
              "Tommy Emmanuel",
            track_type="audio",
            position=15,
            title=
              "Mona Lisa",
            album=
              "Endless Road"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=4,
                    minutes=55,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            performer=
              "June Christy",
            track_type="audio",
            position=16,
            title=
              "Don't Explain",
            album=
              "Anthology - VA: Lady Sings The Blues"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=48,
                    minutes=57,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            performer=
              "Bobby Darin",
            track_type="audio",
            position=17,
            title="Milord",
            album=
              "The Bobby Darin Collection (Disc 2) - The Pop Years - Part One"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=49,
                    minutes=59,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            performer=
              "Fred Astaire",
            track_type="audio",
            position=18,
            title=
              "A Fine Romance",
            album=
              "Steppin'Out: Astaire Sings"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=58,
                    minutes=63,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            performer=
              "Stacey Kent",
            track_type="audio",
            position=19,
            title="Dreamer",
            album=
              "Dreamer In Concert"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=4,
                    minutes=69,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            performer=
              "Stacey Kent",
            track_type="audio",
            position=20,
            title=
              "Jardin D'hiver",
            album=
              "Dreamer In Concert"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=30,
                    minutes=73,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            performer=
              "Tony Bennett",
            track_type="audio",
            position=21,
            title=
              "Maybe This Time [From Cabaret]",
            album=
              "Forty Years: The Artistry of Tony Bennett Disc 4"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=33,
                    minutes=77,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            performer=
              "Tony Bennett",
            track_type="audio",
            position=22,
            title=
              "If I Ruled The World 3:00",
            album=
              "Very Best of"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=49,
                    minutes=80,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            performer=
              "Tommy Dorsey",
            track_type="audio",
            position=23,
            title=
              "Be Careful It's My Heart",
            album=
              "Sentimental Memories With Vocals By Frank Sinatra"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=55,
                    minutes=83,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            performer=
              "Day, Doris",
            track_type="audio",
            position=24,
            title=
              "You Made Me Love You (I Didn't Want To Do It)",
            album=
              "The Essential Love Songs"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=41,
                    minutes=86,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            performer=
              "Frank Capp",
            track_type="audio",
            position=25,
            title=
              "Li'l Darlin'",
            album=
              "In a Hefti Bag"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=37,
                    minutes=91,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            title=
              "Standard Time",
            position=26,
            track_type="audio",
            performer=""
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=41,
                    minutes=91,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            title=
              "Soundtrack Of Your Life",
            position=27,
            track_type="audio",
            performer=""
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=46,
                    minutes=91,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            performer=
              "Johnny Mathis",
            track_type="audio",
            position=28,
            title="Misty",
            album=
              "The Essential Johnny Mathis Disc 1"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=19,
                    minutes=95,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            title=
              "Soundtrack Of Your Life",
            position=29,
            track_type="audio",
            performer=""
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=25,
                    minutes=95,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            title=
              "Standard Time",
            position=30,
            track_type="audio",
            performer=""
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=39,
                    minutes=95,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            title=
              "Don't worry bout me",
            position=31,
            track_type="audio",
            performer=
              "Tierny Sutton"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=23,
                    minutes=100,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            performer=
              "Steve Lawrence, Eydie Gorme",
            track_type="audio",
            position=32,
            title=
              "What I Did For Love",
            album=
              "The Hits of Steve Lawrence & Eydie Gorme"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=47,
                    minutes=104,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            performer=
              "Woody Herman",
            track_type="audio",
            position=33,
            title=
              "Woodchopper's Ball",
            album=
              "Woodchopper's Ball"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=12,
                    minutes=108,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            performer=
              "Jo Stafford",
            track_type="audio",
            position=34,
            title=
              "There's No You",
            album=
              "Fools Rush In"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=33,
                    minutes=111,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            performer=
              "Dean Martin",
            track_type="audio",
            position=35,
            title=
              "It Looks Like Love",
            album=
              "Essential Love Songs [+digital booklet]"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=56,
                    minutes=113,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            performer="",
            track_type="audio",
            position=36,
            title="12-sammy_davis_jr-too_close_for_comfort",
            album=
              "The Reprise Years"
          },
          {
            indexes=
              [
                (
                  1,
                  {
                    frames=0,
                    seconds=19,
                    minutes=117,
                    file_type="mp3",
                    filename="/home/radio/firepit/StandardTime.mp3"
                  }
                )
              ],
            performer=
              "Pete Fountain",
            track_type="audio",
            position=37,
            title=
              "La Vie En Rose",
            album=
              "A Touch Of Class"
          }
        ],
      rem=[]
    }
  )

  test.equal(
    playlist.parse.cue(cue_file),
    [
      (
        [
          ("liq_cue_out", "222.0"),
          (
            "album",
            "Volume 1 : Let's Dance - (Disc2"
          ),
          (
            "title",
            "Moonlight Serenade"
          ),
          (
            "artist",
            "Big Bands In Hi-Fi"
          ),
          ("tracknumber", "1")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "222.0"),
          ("liq_cue_out", "429.0"),
          (
            "title",
            "The Laughs are on me from Frank Sinatra Balids"
          ),
          (
            "artist",
            "frank sinatra"
          ),
          ("tracknumber", "2")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "429.0"),
          ("liq_cue_out", "727.0"),
          (
            "album",
            "Anthology - Frank Sinatra: Collection"
          ),
          (
            "title",
            "It Was A Very Good Year"
          ),
          (
            "artist",
            "Frank Sinatra"
          ),
          ("tracknumber", "3")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "727.0"),
          ("liq_cue_out", "942.0"),
          (
            "album",
            "The Capitol Years (Disc 2)"
          ),
          (
            "title",
            "It Was A Very Good Year"
          ),
          ("artist", ""),
          ("tracknumber", "4")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "942.0"),
          ("liq_cue_out", "1156.0"),
          (
            "album",
            "Personal Choice"
          ),
          (
            "title",
            "I'm Old Fashioned (Kern-Mercer)"
          ),
          (
            "artist",
            "Marian McPartland"
          ),
          ("tracknumber", "5")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "1156.0"),
          ("liq_cue_out", "1332.0"),
          (
            "album",
            "Rosie Solves The Swingin' Riddle! (Vinyl)"
          ),
          (
            "title",
            "You Took Advantage Of Me"
          ),
          (
            "artist",
            "Rosemary Clooney"
          ),
          ("tracknumber", "6")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "1332.0"),
          ("liq_cue_out", "1602.0"),
          (
            "album",
            "From Bing to Billie"
          ),
          (
            "title",
            "Pennies from Heaven"
          ),
          (
            "artist",
            "Rosemary Clooney"
          ),
          ("tracknumber", "7")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "1602.0"),
          ("liq_cue_out", "1787.0"),
          (
            "album",
            "Live - Billie Holiday: 1936-1949 - Just Jazz with Ed Beach"
          ),
          (
            "title",
            "Getting Some Fun Out Of Life"
          ),
          (
            "artist",
            "Billie Holiday"
          ),
          ("tracknumber", "8")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "1787.0"),
          ("liq_cue_out", "1963.0"),
          (
            "album",
            "The Definitive Collection"
          ),
          (
            "title",
            "It's Been A Long, Long Time"
          ),
          (
            "artist",
            "Bing Crosby"
          ),
          ("tracknumber", "9")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "1963.0"),
          ("liq_cue_out", "2153.0"),
          (
            "album",
            "The Definitive Collection"
          ),
          (
            "title",
            "Gone Fishin' (Duet With Bing Crosby)"
          ),
          (
            "artist",
            "Louis Armstrong"
          ),
          ("tracknumber", "10")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "2153.0"),
          ("liq_cue_out", "2499.0"),
          (
            "album",
            "Turn Up The Quiet"
          ),
          ("title", "Moonglow"),
          (
            "artist",
            "Diana Krall"
          ),
          ("tracknumber", "11")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "2499.0"),
          ("liq_cue_out", "2656.0"),
          (
            "title",
            "Hello, Young Lovers"
          ),
          (
            "artist",
            "Perry Como"
          ),
          ("tracknumber", "12")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "2656.0"),
          ("liq_cue_out", "2856.0"),
          (
            "title",
            "I'd be but a slave to you perry como"
          ),
          (
            "artist",
            "Perry Como"
          ),
          ("tracknumber", "13")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "2856.0"),
          ("liq_cue_out", "3067.0"),
          (
            "album",
            "The Dinah Washington Story"
          ),
          (
            "title",
            "It Isn't Fair"
          ),
          (
            "artist",
            "Dinah Washington"
          ),
          ("tracknumber", "14")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "3067.0"),
          ("liq_cue_out", "3304.0"),
          (
            "album",
            "Endless Road"
          ),
          (
            "title",
            "Mona Lisa"
          ),
          (
            "artist",
            "Tommy Emmanuel"
          ),
          ("tracknumber", "15")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "3304.0"),
          ("liq_cue_out", "3468.0"),
          (
            "album",
            "Anthology - VA: Lady Sings The Blues"
          ),
          (
            "title",
            "Don't Explain"
          ),
          (
            "artist",
            "June Christy"
          ),
          ("tracknumber", "16")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "3468.0"),
          ("liq_cue_out", "3589.0"),
          (
            "album",
            "The Bobby Darin Collection (Disc 2) - The Pop Years - Part One"
          ),
          ("title", "Milord"),
          (
            "artist",
            "Bobby Darin"
          ),
          ("tracknumber", "17")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "3589.0"),
          ("liq_cue_out", "3838.0"),
          (
            "album",
            "Steppin'Out: Astaire Sings"
          ),
          (
            "title",
            "A Fine Romance"
          ),
          (
            "artist",
            "Fred Astaire"
          ),
          ("tracknumber", "18")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "3838.0"),
          ("liq_cue_out", "4144.0"),
          (
            "album",
            "Dreamer In Concert"
          ),
          ("title", "Dreamer"),
          (
            "artist",
            "Stacey Kent"
          ),
          ("tracknumber", "19")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "4144.0"),
          ("liq_cue_out", "4410.0"),
          (
            "album",
            "Dreamer In Concert"
          ),
          (
            "title",
            "Jardin D'hiver"
          ),
          (
            "artist",
            "Stacey Kent"
          ),
          ("tracknumber", "20")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "4410.0"),
          ("liq_cue_out", "4653.0"),
          (
            "album",
            "Forty Years: The Artistry of Tony Bennett Disc 4"
          ),
          (
            "title",
            "Maybe This Time [From Cabaret]"
          ),
          (
            "artist",
            "Tony Bennett"
          ),
          ("tracknumber", "21")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "4653.0"),
          ("liq_cue_out", "4849.0"),
          (
            "album",
            "Very Best of"
          ),
          (
            "title",
            "If I Ruled The World 3:00"
          ),
          (
            "artist",
            "Tony Bennett"
          ),
          ("tracknumber", "22")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "4849.0"),
          ("liq_cue_out", "5035.0"),
          (
            "album",
            "Sentimental Memories With Vocals By Frank Sinatra"
          ),
          (
            "title",
            "Be Careful It's My Heart"
          ),
          (
            "artist",
            "Tommy Dorsey"
          ),
          ("tracknumber", "23")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "5035.0"),
          ("liq_cue_out", "5201.0"),
          (
            "album",
            "The Essential Love Songs"
          ),
          (
            "title",
            "You Made Me Love You (I Didn't Want To Do It)"
          ),
          (
            "artist",
            "Day, Doris"
          ),
          ("tracknumber", "24")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "5201.0"),
          ("liq_cue_out", "5497.0"),
          (
            "album",
            "In a Hefti Bag"
          ),
          (
            "title",
            "Li'l Darlin'"
          ),
          (
            "artist",
            "Frank Capp"
          ),
          ("tracknumber", "25")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "5497.0"),
          ("liq_cue_out", "5501.0"),
          (
            "title",
            "Standard Time"
          ),
          ("artist", ""),
          ("tracknumber", "26")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "5501.0"),
          ("liq_cue_out", "5506.0"),
          (
            "title",
            "Soundtrack Of Your Life"
          ),
          ("artist", ""),
          ("tracknumber", "27")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "5506.0"),
          ("liq_cue_out", "5719.0"),
          (
            "album",
            "The Essential Johnny Mathis Disc 1"
          ),
          ("title", "Misty"),
          (
            "artist",
            "Johnny Mathis"
          ),
          ("tracknumber", "28")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "5719.0"),
          ("liq_cue_out", "5725.0"),
          (
            "title",
            "Soundtrack Of Your Life"
          ),
          ("artist", ""),
          ("tracknumber", "29")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "5725.0"),
          ("liq_cue_out", "5739.0"),
          (
            "title",
            "Standard Time"
          ),
          ("artist", ""),
          ("tracknumber", "30")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "5739.0"),
          ("liq_cue_out", "6023.0"),
          (
            "title",
            "Don't worry bout me"
          ),
          (
            "artist",
            "Tierny Sutton"
          ),
          ("tracknumber", "31")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "6023.0"),
          ("liq_cue_out", "6287.0"),
          (
            "album",
            "The Hits of Steve Lawrence & Eydie Gorme"
          ),
          (
            "title",
            "What I Did For Love"
          ),
          (
            "artist",
            "Steve Lawrence, Eydie Gorme"
          ),
          ("tracknumber", "32")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "6287.0"),
          ("liq_cue_out", "6492.0"),
          (
            "album",
            "Woodchopper's Ball"
          ),
          (
            "title",
            "Woodchopper's Ball"
          ),
          (
            "artist",
            "Woody Herman"
          ),
          ("tracknumber", "33")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "6492.0"),
          ("liq_cue_out", "6693.0"),
          (
            "album",
            "Fools Rush In"
          ),
          (
            "title",
            "There's No You"
          ),
          (
            "artist",
            "Jo Stafford"
          ),
          ("tracknumber", "34")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "6693.0"),
          ("liq_cue_out", "6836.0"),
          (
            "album",
            "Essential Love Songs [+digital booklet]"
          ),
          (
            "title",
            "It Looks Like Love"
          ),
          (
            "artist",
            "Dean Martin"
          ),
          ("tracknumber", "35")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "6836.0"),
          ("liq_cue_out", "7039.0"),
          (
            "album",
            "The Reprise Years"
          ),
          ("title", "12-sammy_davis_jr-too_close_for_comfort"),
          ("artist", ""),
          ("tracknumber", "36")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      ),
      (
        [
          ("liq_cue_in", "7039.0"),
          (
            "album",
            "A Touch Of Class"
          ),
          (
            "title",
            "La Vie En Rose"
          ),
          (
            "artist",
            "Pete Fountain"
          ),
          ("tracknumber", "37")
        ],
        "/home/radio/firepit/StandardTime.mp3"
      )
    ]
  )

  test.pass()
end

test.check(f)
