#!/usr/bin/liquidsoap

# Standard settings
log.file := true
log.file.path := "/var/log/liquidsoap/pi.log"
log.stdout := false
init.daemon := true
init.daemon.pidfile.path := "/var/run/liquidsoap/pi.pid"

# Enable telnet server
settings.server.telnet.set(true)

# Enable harbor for any external connection
settings.harbor.bind_addrs.set(["0.0.0.0"])

# Verbose logs
log.level.set(4)

# === Settings ===

# The host to request files
stream = "XXXxXXXx"

# The command to request files
scripts =
  "ssh XXxxxXXX@#{stream} '/path/to/scripts/"

# A substitution on the returned path
sed =
  " | sed -e s#/path/to/files/#ftp://user:password@#{stream}/#'"

# Enable replay gain
enable_replaygain_metadata()

pass = "XXxXXXXx"
ice_host = "localhost"

descr = "RadioPi"
url = "http://radiopi.org"

# === Live ===

# A live source, on which we strip blank (make the source unavailable when
# streaming blank).
live =
  blank.strip(
    input.harbor(
      id="live", port=8000, password=pass, buffer=8., max=20., "live.ogg"
    ),
    max_blank=10.,
    threshold=-50.
  )

# This source relays the live data, when available, to the other streamer, in
# uncompressed format (WAV).
output.icecast(
  %wav,
  host=stream,
  port=8005,
  password=pass,
  mount="live.ogg",
  fallible=true,
  live
)

# This source relays the live source to "live.ogg". This is used for debugging
# purposes, to see what is sent to the harbor source.
output.icecast(
  %vorbis,
  host="127.0.0.1",
  port=8080,
  password=pass,
  mount="live.ogg",
  fallible=true,
  live
)

# This source starts an archive of the live stream when available
title =
  '$(if $(title),"$(title)","Emission inconnue")$(if $(artist), " par  \
   $(artist)") - %m-%d-%Y, %H:%M:%S'
output.file(
  %vorbis,
  reopen_on_metadata=fun (_) -> true,
  fallible=true,
  "/data/archives/brutes/" ^ title ^ ".ogg",
  live
)

# === Channels ===

# Specialize the output functions
def out(fmt, ~id, ~mount, ~name, ~genre, s) =
  output.icecast(
    fmt,
    id=id,
    description=descr,
    url=url,
    host=ice_host,
    port=8080,
    password=pass,
    fallible=true,
    mount=mount,
    name=name,
    genre=genre,
    s
  )
end
def out_aac32(~id, ~mount, ~name, ~genre, s) =
  out(%fdkaac(bitrate = 32), id=id, mount=mount, name=name, genre=genre, s)
end
def out_aac(~id, ~mount, ~name, ~genre, s) =
  out(%fdkaac(bitrate = 64), id=id, mount=mount, name=name, genre=genre, s)
end
def out(~id, ~mount, ~name, ~genre, s) =
  out(%mp3, id=id, mount=mount, name=name, genre=genre, s)
end

# A file for playing during failures
interlude = single("/home/radiopi/fallback.mp3")

# Lastfm submission
def lastfm(m) =
  if
    (m["type"] == "chansons")
  then
    if
      (
        m["canal"] == "reggae"
      or
        m["canal"] == "Jazz"
      or
        m["canal"] == "That70Sound"
      )
    then
      canal =
        if (m["canal"] == "That70Sound") then "70sound" else m["canal"] end
      user = "radiopi-" ^ canal
      lastfm.submit(user=user, password="xXXxx", m)
    end
  end
end

# === Basic sources ===

# Custom crossfade to deal with jingles.
def crossfade(
  ~start_next=5.,
  ~fade_in=3.,
  ~fade_out=3.,
  ~default=(fun (a, b) -> sequence([a, b])),
  ~high=-15.,
  ~medium=-32.,
  ~margin=4.,
  ~width=2.,
  s
) =
  fade_out = fun (s) -> fade.out(type="sin", duration=fade_out, s)
  fade_in = fun (s) -> fade.in(type="sin", duration=fade_in, s)
  add = fun (a, b) -> add(normalize=false, [b, a])
  log = fun (~level=3, x) -> log(label="crossfade", level=level, x)

  def transition(a, b) =
    list.iter(
      fun (x) ->
        log(
          level=4,
          "Before: #{x}"
        ),
      a.metadata
    )
    list.iter(
      fun (x) ->
        log(
          level=4,
          "After : #{x}"
        ),
      b.metadata
    )

    if
      a.metadata["type"] == "jingles" or b.metadata["type"] == "jingles"
    then
      log(
        "Old or new file is a jingle: sequenced transition."
      )
      sequence([a.source, b.source])
    elsif
      # If A and B are not too loud and close, fully cross-fade them.
      a.db_level <= medium
    and
      b.db_level <= medium
    and
      abs(a.db_level - b.db_level) <= margin
    then
      log(
        "Old <= medium, new <= medium and |old-new| <= margin."
      )
      log(
        "Old and new source are not too loud and close."
      )
      log(
        "Transition: crossed, fade-in, fade-out."
      )
      add(fade_out(a.source), fade_in(b.source))
    elsif
      # If B is significantly louder than A, only fade-out A.
      # We don't want to fade almost silent things, ask for >medium.
      b.db_level >= a.db_level + margin
    and
      a.db_level >= medium
    and
      b.db_level <= high
    then
      log(
        "new >= old + margin, old >= medium and new <= high."
      )
      log(
        "New source is significantly louder than old one."
      )
      log(
        "Transition: crossed, fade-out."
      )
      add(fade_out(a.source), b.source)
    elsif
      # Opposite as the previous one.
      a.db_level >= b.db_level + margin
    and
      b.db_level >= medium
    and
      a.db_level <= high
    then
      log(
        "old >= new + margin, new >= medium and old <= high"
      )
      log(
        "Old source is significantly louder than new one."
      )
      log(
        "Transition: crossed, fade-in."
      )
      add(a.source, fade_in(b.source))
    elsif
      # Do not fade if it's already very low.
      b.db_level >= a.db_level + margin
    and
      a.db_level <= medium
    and
      b.db_level <= high
    then
      log(
        "new >= old + margin, old <= medium and new <= high."
      )
      log(
        "Do not fade if it's already very low."
      )
      log(
        "Transition: crossed, no fade."
      )
      add(a.source, b.source)

      # What to do with a loud end and a quiet beginning?
      # A good idea is to use a jingle to separate the two tracks,
      # but that's another story.
    else
      # Otherwise, A and B are just too loud to overlap nicely, or the
      # difference between them is too large and overlapping would completely
      # mask one of them.
      log(
        "No transition: using default."
      )
      default(a.source, b.source)
    end
  end

  cross(width=width, duration=start_next, transition, s)
end

# Create a radiopilote-driven source
def channel_radiopilote(~skip=true, name) =
  log(
    "Creating canal #{name}"
  )

  # Request function
  def req() =
    log(
      "Request for #{name}"
    )
    ret =
      list.hd(
        process.read.lines(
          scripts ^
            "radiopilote-getnext " ^
            process.quote(name) ^
            sed
        )
      )
    log(
      "Got answer: #{ret} for #{name}"
    )
    request.create(ret)
  end

  # Create the dynamic source.
  s = request.dynamic(id="dyn_" ^ name, req, timeout=60.)

  # Apply normalization using replaygain information.
  s = amplify(1., override="replay_gain", s)

  # Skip blank when asked to
  s = if skip then blank.skip(s, max_blank=10., threshold=-40.) else s end

  # Submit new tracks on lastfm
  s = source.on_metadata(s, lastfm)

  # Tell the system when a new track is played
  s =
    source.on_metadata(
      s,
      fun (meta) ->
        process.run(
          scripts ^
            "radiopilote-feedback " ^
            process.quote(meta["canal"]) ^
            " " ^
            process.quote(meta["file_id"]) ^
            "'"
        )
    )

  # Finally apply a smart crossfading
  crossfade(s)
end

# Basic source
jazz = channel_radiopilote("jazz")
discoqueen = channel_radiopilote("discoqueen")

# Avoid skipping blank with classic music !!
classique = channel_radiopilote(skip=false, "classique")
That70Sound = channel_radiopilote("That70Sound")
metal = channel_radiopilote("metal")
reggae = channel_radiopilote("reggae")
Rock = channel_radiopilote("Rock")

# Group those sources in a separate
# clock (good for multithreading/multicore)
clock.assign_new([jazz, That70Sound, metal, reggae])

# === Mixing live ===

# To create a channel from a basic source, add:
# - a new-track notification for radiopilote
# - metadata rewriting
# - the live shows
# - the failsafe 'interlude' source to channels
# - blank detection
def mklive(s) =
  # Transition function: if transitioning to the live, fade out the old source
  # if transitioning from live, fade.in the new source. NOTE: We cannot skip the
  # current song because reloading new songs for all the sources when live
  # starts costs too much CPU.
  def trans(old, new) =
    if
      source.id(new) == source.id(live)
    then
      log(
        "Transition to live!"
      )
      add([new, fade.out(old)])
    elsif
      source.id(old) == source.id(live)
    then
      log(
        "Transitioning from live!"
      )
      add([fade.in(new), old])
    else
      log(
        "Dummy transition"
      )
      new
    end
  end
  fallback(
    track_sensitive=false,
    transitions=[trans, trans, trans],
    [live, (s : source), interlude]
  )
end

# Create a channel using mklive(), encode and output it to icecast.
def mkoutput(~out=out, mount, source, name, genre) =
  out(id=(mount : string), mount=mount, name=name, genre=genre, mklive(source))
end

# === Outputs ===

mkoutput(
  "jazz",
  jazz,
  "RadioPi - Canal Jazz",
  "jazz"
)
mkoutput(
  "discoqueen",
  discoqueen,
  "RadioPi - Canal DiscoQueen",
  "discoqueen"
)
mkoutput(
  "classique",
  classique,
  "RadioPi - Canal Classique",
  "classique"
)
mkoutput(
  "That70Sound",
  That70Sound,
  "RadioPi - Canal That70Sound",
  "That70Sound"
)
mkoutput(
  "metal",
  metal,
  "RadioPi - Canal Metal",
  "metal"
)
mkoutput(
  "reggae",
  reggae,
  "RadioPi - Canal Reggae",
  "reggae"
)
mkoutput(
  "Rock",
  Rock,
  "RadioPi - Canal Rock",
  "Rock"
)

# Test outouts
mkoutput(
  out=out_aac,
  "reggae.aacp",
  reggae,
  "RadioPi - Canal Reggae (64 kbits AAC+ test stream)",
  "reggae"
)
mkoutput(
  out=out_aac32,
  "reggae.aacp32",
  reggae,
  "RadioPi - Canal Reggae (32 kbits AAC+ test stream)",
  "reggae"
)
