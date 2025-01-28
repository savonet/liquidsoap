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
            indexes=[(1, {minutes=0, seconds=0, frames=0})],
            title=
              "Smalltown boy (12 extended)",
            position=1,
            track_type="audio",
            filename=
              "Bronski beat - Smalltown boy (12 extended).mp3",
            file_type="mp3",
            performer=
              "Bronski Beat"
          },
          {
            indexes=[(1, {minutes=0, seconds=0, frames=0})],
            title=
              "Dont Stop The Music (Ed Marquis Remix)",
            position=2,
            track_type="audio",
            filename=
              "Rihanna - Dont Stop The Music (Ed Marquis Remix).mp3",
            file_type="mp3",
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
        "Bronski beat - Smalltown boy (12 extended).mp3",
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
            "artist",
            "Bronski Beat"
          ),
          ("track", "1"),
          ("date", "2020"),
          ("genre", "Electronica")
        ]
      ),
      (
        "Rihanna - Dont Stop The Music (Ed Marquis Remix).mp3",
        [
          (
            "album",
            "Musique a la carte"
          ),
          (
            "title",
            "Dont Stop The Music (Ed Marquis Remix)"
          ),
          ("artist", "Rihanna"),
          ("track", "2"),
          ("date", "2020"),
          ("genre", "Electronica")
        ]
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
                (0, {minutes=0, seconds=0, frames=0}),
                (1, {minutes=3, seconds=22, frames=70})
              ],
            performer=
              "Block Party",
            file_type="wave",
            filename=
              "Block Party - Silent Alarm.flac",
            track_type="audio",
            position=1,
            title=
              "Like Eating Glass"
          },
          {
            indexes=
              [
                (0, {minutes=7, seconds=42, frames=69}),
                (1, {minutes=7, seconds=44, frames=69})
              ],
            performer=
              "Block Party",
            file_type="wave",
            filename=
              "Block Party - Silent Alarm.flac",
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
        "Block Party - Silent Alarm.flac",
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
            "artist",
            "Block Party"
          ),
          ("track", "1"),
          ("liq_index_zero", "0.0")
        ]
      ),
      (
        "Block Party - Silent Alarm.flac",
        [
          ("liq_cue_in", "464.92"),
          (
            "album",
            "Silent Alarm"
          ),
          ("title", "Helicopter"),
          (
            "artist",
            "Block Party"
          ),
          ("track", "2"),
          ("liq_index_zero", "462.92")
        ]
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
            indexes=[(1, {minutes=0, seconds=0, frames=0})],
            title="Gangsters",
            position=1,
            track_type="audio",
            filename=
              "The Specials - Singles.wav",
            file_type="wave",
            performer=
              "The Specials"
          },
          {
            indexes=
              [
                (0, {minutes=2, seconds=47, frames=74}),
                (1, {minutes=2, seconds=48, frames=27})
              ],
            performer=
              "The Specials",
            file_type="wave",
            filename=
              "The Specials - Singles.wav",
            track_type="audio",
            position=2,
            title=
              "Rudi, A Message To You"
          },
          {
            indexes=
              [
                (0, {minutes=5, seconds=41, frames=50}),
                (1, {minutes=5, seconds=42, frames=27})
              ],
            performer=
              "The Specials",
            file_type="wave",
            filename=
              "The Specials - Singles.wav",
            track_type="audio",
            position=3,
            title=
              "Nite Klub"
          },
          {
            indexes=
              [
                (0, {minutes=8, seconds=53, frames=47}),
                (1, {minutes=8, seconds=54, frames=37})
              ],
            performer=
              "The Specials",
            file_type="wave",
            filename=
              "The Specials - Singles.wav",
            track_type="audio",
            position=4,
            title=
              "Too Much Too Young"
          },
          {
            indexes=
              [
                (0, {minutes=10, seconds=59, frames=20}),
                (1, {minutes=11, seconds=0, frames=17})
              ],
            performer=
              "The Specials",
            file_type="wave",
            filename=
              "The Specials - Singles.wav",
            track_type="audio",
            position=5,
            title=
              "Guns Of Navarone"
          },
          {
            indexes=
              [
                (0, {minutes=13, seconds=20, frames=55}),
                (1, {minutes=13, seconds=20, frames=67})
              ],
            performer=
              "The Specials",
            file_type="wave",
            filename=
              "The Specials - Singles.wav",
            track_type="audio",
            position=6,
            title=
              "Rat Race"
          },
          {
            indexes=
              [
                (0, {minutes=16, seconds=29, frames=67}),
                (1, {minutes=16, seconds=30, frames=30})
              ],
            performer=
              "The Specials",
            file_type="wave",
            filename=
              "The Specials - Singles.wav",
            track_type="audio",
            position=7,
            title="Stereotype"
          },
          {
            indexes=
              [
                (0, {minutes=20, seconds=19, frames=27}),
                (1, {minutes=20, seconds=20, frames=20})
              ],
            performer=
              "The Specials",
            file_type="wave",
            filename=
              "The Specials - Singles.wav",
            track_type="audio",
            position=8,
            title=
              "International Jet Set"
          },
          {
            indexes=
              [
                (0, {minutes=24, seconds=30, frames=70}),
                (1, {minutes=24, seconds=32, frames=27})
              ],
            performer=
              "The Specials",
            file_type="wave",
            filename=
              "The Specials - Singles.wav",
            track_type="audio",
            position=9,
            title=
              "Do Nothing"
          },
          {
            indexes=
              [
                (0, {minutes=28, seconds=23, frames=30}),
                (1, {minutes=28, seconds=23, frames=42})
              ],
            performer=
              "The Specials",
            file_type="wave",
            filename=
              "The Specials - Singles.wav",
            track_type="audio",
            position=10,
            title=
              "Ghost Town"
          },
          {
            indexes=
              [
                (0, {minutes=34, seconds=21, frames=37}),
                (1, {minutes=34, seconds=21, frames=47})
              ],
            performer=
              "The Specials",
            file_type="wave",
            filename=
              "The Specials - Singles.wav",
            track_type="audio",
            position=11,
            title="Why?"
          },
          {
            indexes=
              [
                (0, {minutes=38, seconds=16, frames=50}),
                (1, {minutes=38, seconds=16, frames=55})
              ],
            performer=
              "The Specials",
            file_type="wave",
            filename=
              "The Specials - Singles.wav",
            track_type="audio",
            position=12,
            title=
              "Friday Night, Saturday Morning"
          },
          {
            indexes=
              [
                (0, {minutes=41, seconds=50, frames=7}),
                (1, {minutes=41, seconds=51, frames=0})
              ],
            performer=
              "The Specials",
            file_type="wave",
            filename=
              "The Specials - Singles.wav",
            track_type="audio",
            position=13,
            title=
              "War Crimes"
          },
          {
            indexes=
              [
                (0, {minutes=45, seconds=50, frames=55}),
                (1, {minutes=45, seconds=51, frames=72})
              ],
            performer=
              "The Specials",
            file_type="wave",
            filename=
              "The Specials - Singles.wav",
            track_type="audio",
            position=14,
            title=
              "Racist Friend"
          },
          {
            indexes=
              [
                (0, {minutes=49, seconds=35, frames=55}),
                (1, {minutes=49, seconds=38, frames=22})
              ],
            performer=
              "The Specials",
            file_type="wave",
            filename=
              "The Specials - Singles.wav",
            track_type="audio",
            position=15,
            title=
              "Nelson Mandela"
          },
          {
            indexes=
              [
                (0, {minutes=54, seconds=11, frames=0}),
                (1, {minutes=54, seconds=12, frames=40})
              ],
            performer=
              "The Specials",
            file_type="wave",
            filename=
              "The Specials - Singles.wav",
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
        "The Specials - Singles.wav",
        [
          ("liq_cue_out", "167.986666667"),
          ("album", "Singles"),
          ("title", "Gangsters"),
          (
            "artist",
            "The Specials"
          ),
          ("track", "1"),
          (
            "comment",
            "ExactAudioCopy v0.95b4"
          ),
          ("discid", "D00DA810"),
          ("date", "1991"),
          ("genre", "Ska")
        ]
      ),
      (
        "The Specials - Singles.wav",
        [
          ("liq_cue_in", "168.36"),
          ("liq_cue_out", "341.666666667"),
          ("album", "Singles"),
          (
            "title",
            "Rudi, A Message To You"
          ),
          (
            "artist",
            "The Specials"
          ),
          ("track", "2"),
          ("liq_index_zero", "167.986666667"),
          (
            "comment",
            "ExactAudioCopy v0.95b4"
          ),
          ("discid", "D00DA810"),
          ("date", "1991"),
          ("genre", "Ska")
        ]
      ),
      (
        "The Specials - Singles.wav",
        [
          ("liq_cue_in", "342.36"),
          ("liq_cue_out", "533.626666667"),
          ("album", "Singles"),
          (
            "title",
            "Nite Klub"
          ),
          (
            "artist",
            "The Specials"
          ),
          ("track", "3"),
          ("liq_index_zero", "341.666666667"),
          (
            "comment",
            "ExactAudioCopy v0.95b4"
          ),
          ("discid", "D00DA810"),
          ("date", "1991"),
          ("genre", "Ska")
        ]
      ),
      (
        "The Specials - Singles.wav",
        [
          ("liq_cue_in", "534.493333333"),
          ("liq_cue_out", "659.266666667"),
          ("album", "Singles"),
          (
            "title",
            "Too Much Too Young"
          ),
          (
            "artist",
            "The Specials"
          ),
          ("track", "4"),
          ("liq_index_zero", "533.626666667"),
          (
            "comment",
            "ExactAudioCopy v0.95b4"
          ),
          ("discid", "D00DA810"),
          ("date", "1991"),
          ("genre", "Ska")
        ]
      ),
      (
        "The Specials - Singles.wav",
        [
          ("liq_cue_in", "660.226666667"),
          ("liq_cue_out", "800.733333333"),
          ("album", "Singles"),
          (
            "title",
            "Guns Of Navarone"
          ),
          (
            "artist",
            "The Specials"
          ),
          ("track", "5"),
          ("liq_index_zero", "659.266666667"),
          (
            "comment",
            "ExactAudioCopy v0.95b4"
          ),
          ("discid", "D00DA810"),
          ("date", "1991"),
          ("genre", "Ska")
        ]
      ),
      (
        "The Specials - Singles.wav",
        [
          ("liq_cue_in", "800.893333333"),
          ("liq_cue_out", "989.893333333"),
          ("album", "Singles"),
          (
            "title",
            "Rat Race"
          ),
          (
            "artist",
            "The Specials"
          ),
          ("track", "6"),
          ("liq_index_zero", "800.733333333"),
          (
            "comment",
            "ExactAudioCopy v0.95b4"
          ),
          ("discid", "D00DA810"),
          ("date", "1991"),
          ("genre", "Ska")
        ]
      ),
      (
        "The Specials - Singles.wav",
        [
          ("liq_cue_in", "990.4"),
          ("liq_cue_out", "1219.36"),
          ("album", "Singles"),
          ("title", "Stereotype"),
          (
            "artist",
            "The Specials"
          ),
          ("track", "7"),
          ("liq_index_zero", "989.893333333"),
          (
            "comment",
            "ExactAudioCopy v0.95b4"
          ),
          ("discid", "D00DA810"),
          ("date", "1991"),
          ("genre", "Ska")
        ]
      ),
      (
        "The Specials - Singles.wav",
        [
          ("liq_cue_in", "1220.26666667"),
          ("liq_cue_out", "1470.93333333"),
          ("album", "Singles"),
          (
            "title",
            "International Jet Set"
          ),
          (
            "artist",
            "The Specials"
          ),
          ("track", "8"),
          ("liq_index_zero", "1219.36"),
          (
            "comment",
            "ExactAudioCopy v0.95b4"
          ),
          ("discid", "D00DA810"),
          ("date", "1991"),
          ("genre", "Ska")
        ]
      ),
      (
        "The Specials - Singles.wav",
        [
          ("liq_cue_in", "1472.36"),
          ("liq_cue_out", "1703.4"),
          ("album", "Singles"),
          (
            "title",
            "Do Nothing"
          ),
          (
            "artist",
            "The Specials"
          ),
          ("track", "9"),
          ("liq_index_zero", "1470.93333333"),
          (
            "comment",
            "ExactAudioCopy v0.95b4"
          ),
          ("discid", "D00DA810"),
          ("date", "1991"),
          ("genre", "Ska")
        ]
      ),
      (
        "The Specials - Singles.wav",
        [
          ("liq_cue_in", "1703.56"),
          ("liq_cue_out", "2061.49333333"),
          ("album", "Singles"),
          (
            "title",
            "Ghost Town"
          ),
          (
            "artist",
            "The Specials"
          ),
          ("track", "10"),
          ("liq_index_zero", "1703.4"),
          (
            "comment",
            "ExactAudioCopy v0.95b4"
          ),
          ("discid", "D00DA810"),
          ("date", "1991"),
          ("genre", "Ska")
        ]
      ),
      (
        "The Specials - Singles.wav",
        [
          ("liq_cue_in", "2061.62666667"),
          ("liq_cue_out", "2296.66666667"),
          ("album", "Singles"),
          ("title", "Why?"),
          (
            "artist",
            "The Specials"
          ),
          ("track", "11"),
          ("liq_index_zero", "2061.49333333"),
          (
            "comment",
            "ExactAudioCopy v0.95b4"
          ),
          ("discid", "D00DA810"),
          ("date", "1991"),
          ("genre", "Ska")
        ]
      ),
      (
        "The Specials - Singles.wav",
        [
          ("liq_cue_in", "2296.73333333"),
          ("liq_cue_out", "2510.09333333"),
          ("album", "Singles"),
          (
            "title",
            "Friday Night, Saturday Morning"
          ),
          (
            "artist",
            "The Specials"
          ),
          ("track", "12"),
          ("liq_index_zero", "2296.66666667"),
          (
            "comment",
            "ExactAudioCopy v0.95b4"
          ),
          ("discid", "D00DA810"),
          ("date", "1991"),
          ("genre", "Ska")
        ]
      ),
      (
        "The Specials - Singles.wav",
        [
          ("liq_cue_in", "2511.0"),
          ("liq_cue_out", "2750.73333333"),
          ("album", "Singles"),
          (
            "title",
            "War Crimes"
          ),
          (
            "artist",
            "The Specials"
          ),
          ("track", "13"),
          ("liq_index_zero", "2510.09333333"),
          (
            "comment",
            "ExactAudioCopy v0.95b4"
          ),
          ("discid", "D00DA810"),
          ("date", "1991"),
          ("genre", "Ska")
        ]
      ),
      (
        "The Specials - Singles.wav",
        [
          ("liq_cue_in", "2751.96"),
          ("liq_cue_out", "2975.73333333"),
          ("album", "Singles"),
          (
            "title",
            "Racist Friend"
          ),
          (
            "artist",
            "The Specials"
          ),
          ("track", "14"),
          ("liq_index_zero", "2750.73333333"),
          (
            "comment",
            "ExactAudioCopy v0.95b4"
          ),
          ("discid", "D00DA810"),
          ("date", "1991"),
          ("genre", "Ska")
        ]
      ),
      (
        "The Specials - Singles.wav",
        [
          ("liq_cue_in", "2978.29333333"),
          ("liq_cue_out", "3251.0"),
          ("album", "Singles"),
          (
            "title",
            "Nelson Mandela"
          ),
          (
            "artist",
            "The Specials"
          ),
          ("track", "15"),
          ("liq_index_zero", "2975.73333333"),
          (
            "comment",
            "ExactAudioCopy v0.95b4"
          ),
          ("discid", "D00DA810"),
          ("date", "1991"),
          ("genre", "Ska")
        ]
      ),
      (
        "The Specials - Singles.wav",
        [
          ("liq_cue_in", "3252.53333333"),
          ("album", "Singles"),
          (
            "title",
            "(What I Like Most About You Is Your) Girlfriend"
          ),
          (
            "artist",
            "The Specials"
          ),
          ("track", "16"),
          ("liq_index_zero", "3251.0"),
          (
            "comment",
            "ExactAudioCopy v0.95b4"
          ),
          ("discid", "D00DA810"),
          ("date", "1991"),
          ("genre", "Ska")
        ]
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
            indexes=[(1, {minutes=0, seconds=0, frames=0})],
            title="Gangsters",
            position=1,
            track_type="audio",
            filename=
              "The Specials - Singles - 01 - Gangsters.wav",
            file_type="wave",
            performer=
              "The Specials"
          },
          {
            indexes=[(1, {minutes=0, seconds=0, frames=0})],
            performer=
              "The Specials",
            file_type="wave",
            filename=
              "The Specials - Singles - 02 - Rudi, A Message To You.wav",
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
        "The Specials - Singles - 01 - Gangsters.wav",
        [
          ("title", "Gangsters"),
          (
            "artist",
            "The Specials"
          ),
          ("track", "1")
        ]
      ),
      (
        "The Specials - Singles - 02 - Rudi, A Message To You.wav",
        [
          (
            "title",
            "Rudi, A Message To You"
          ),
          (
            "artist",
            "The Specials"
          ),
          ("track", "2"),
          ("liq_pregap", "0.373333333333")
        ]
      )
    ]
  )

  test.pass()
end

test.check(f)
