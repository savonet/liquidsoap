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

# We use the scheduler intensively,
# therefore we create many queues.
settings.scheduler.generic_queues.set(5)
settings.scheduler.fast_queues.set(3)
settings.scheduler.non_blocking_queues.set(3)

# === Settings ===

# The host to request files
stream = "XXXxXXXx"
# The command to request files
scripts = "ssh XXxxxXXX@#{stream} '/path/to/scripts/"
# A substitution on the returned path
sed = " | sed -e s#/path/to/files/#ftp://user:password@#{stream}/#'"

# Enable replay gain
enable_replaygain_metadata ()

pass = "XXxXXXXx"
ice_host = "localhost"

descr = "RadioPi"
url = "http://radiopi.org"

# === Live ===

# A live source, on which we strip blank (make the source unavailable when
# streaming blank).
live =
  blank.strip(
    input.harbor(id="live", port=8000, password=pass,
                 buffer=8.,max=20.,"live.ogg"),
    max_blank=10., threshold=-50.)

# This source relays the live data, when available, to the other streamer, in
# uncompressed format (WAV).
output.icecast(%wav, host=stream,
               port=8005, password=pass,
               mount="live.ogg", fallible=true,
               live)

# This source relays the live source to "live.ogg". This is used for debugging
# purposes, to see what is sent to the harbor source.
output.icecast(%vorbis, host="127.0.0.1",
               port=8080, password=pass,
               mount="live.ogg", fallible=true,
               live)

# This source starts an archive of the live stream when available
title = '$(if $(title),"$(title)",\
         "Emission inconnue")$(if $(artist), \
         " par  $(artist)") - %m-%d-%Y, %H:%M:%S'
output.file(%vorbis, reopen_on_metadata=fun(_)->true,
            fallible=true,
            "/data/archives/brutes/" ^ title ^ ".ogg",
            live)

# === Channels ===

# Specialize the output functions
def out(fmt,~mount,s) = output.icecast(fmt, description=descr, url=url, host=ice_host, port=8080, password=pass, fallible=true, mount=mount, s) end
def out_aac32(~mount,s) = out(%fdkaac(bitrate=32), mount=mount, s) end
def out_aac(~mount,s) = out(%fdkaac(bitrate=64), mount=mount, s) end
def out(~mount,s) = out(%mp3, mount=mount, s) end

# A file for playing during failures
interlude = single("/home/radiopi/fallback.mp3")

# Lastfm submission
def lastfm (m) =
  if (m["type"] == "chansons") then
    if (m["canal"] == "reggae" or m["canal"] == "Jazz" or m["canal"] == "That70Sound") then
      canal =
        if (m["canal"] == "That70Sound") then
       "70sound"
    else
       m["canal"]
    end
      user = "radiopi-" ^ canal
      lastfm.submit(user=user,password="xXXxx",m)
    end
  end
end

# === Basic sources ===

# Custom crossfade to deal with jingles.
def crossfade (~start_next=5.,~fade_in=3.,~fade_out=3.,
               ~default=(fun (a,b) -> sequence([a, b])),
               ~high=-15., ~medium=-32., ~margin=4.,
               ~width=2.,~conservative=false,s)
  fade_out = fun(s) -> fade.out(type="sin",duration=fade_out,s)
  fade_in  = fun(s) -> fade.in(type="sin",duration=fade_in,s)
  add = fun (a,b) -> add(normalize=false,[b, a])
  log = fun (~level=3, x) -> log(label="crossfade", level=level, x)

  def transition(a,b,ma,mb,sa,sb)

    list.iter(fun(x)-> log(level=4,"Before: #{x}"),ma)
    list.iter(fun(x)-> log(level=4,"After : #{x}"),mb)

    if ma["type"] == "jingles" or mb["type"] == "jingles" then
      log("Old or new file is a jingle: sequenced transition.")
      sequence([sa, sb])
    elsif
      # If A and B are not too loud and close, fully cross-fade them.
      a <= medium and b <= medium and abs(a - b) <= margin
    then
      log("Old <= medium, new <= medium and |old-new| <= margin.")
      log("Old and new source are not too loud and close.")
      log("Transition: crossed, fade-in, fade-out.")
      add(fade_out(sa),fade_in(sb))

    elsif
      # If B is significantly louder than A, only fade-out A.
      # We don't want to fade almost silent things, ask for >medium.
      b >= a + margin and a >= medium and b <= high
    then
      log("new >= old + margin, old >= medium and new <= high.")
      log("New source is significantly louder than old one.")
      log("Transition: crossed, fade-out.")
      add(fade_out(sa),sb)

    elsif
      # Opposite as the previous one.
      a >= b + margin and b >= medium and a <= high
    then
      log("old >= new + margin, new >= medium and old <= high")
      log("Old source is significantly louder than new one.")
      log("Transition: crossed, fade-in.")
      add(sa,fade_in(sb))

    elsif
      # Do not fade if it's already very low.
      b >= a + margin and a <= medium and b <= high
    then
      log("new >= old + margin, old <= medium and new <= high.")
      log("Do not fade if it's already very low.")
      log("Transition: crossed, no fade.")
      add(sa,sb)

    # What to do with a loud end and a quiet beginning?
    # A good idea is to use a jingle to separate the two tracks,
    # but that's another story.

    else
      # Otherwise, A and B are just too loud to overlap nicely, or the
      # difference between them is too large and overlapping would completely
      # mask one of them.
      log("No transition: using default.")
      default(sa, sb)
    end
  end

  cross(width=width, duration=start_next, conservative=conservative, transition, s)
end

# Create a radiopilote-driven source
def channel_radiopilote(~skip=true,name)
  log("Creating canal #{name}")

  # Request function
  def request () =
    log("Request for #{name}")
    ret = list.hd(process.read.lines(scripts^"radiopilote-getnext "^quote(name)^sed))
    log("Got answer: #{ret} for #{name}")
    [request.create(ret)]
  end

  # Create the request.dynamic.list source. Set conservative to true to queue
  # several songs in advance
  source =
    request.dynamic.list(conservative=true, length=400.,
                    id="dyn_"^name,request,
                    timeout=60.)

  # Apply normalization using replaygain information.
  source =   amplify(1.,override="replay_gain", source)

  # Skip blank when asked to
  source =
    if skip then
      blank.skip(source, length=10., threshold=-40.)
    else
      source
    end

  # Submit new tracks on lastfm
  source = on_metadata(lastfm,source)

  # Tell the system when a new track
  # is played
  source = on_metadata(fun (meta) ->
                    system(scripts ^ "radiopilote-feedback "
                           ^quote(meta["canal"])^" "
                           ^quote(meta["file_id"]) ^ "'"), source)

  # Finally apply a smart crossfading
  crossfade(source)
end

# Basic source
jazz = channel_radiopilote("jazz")
discoqueen = channel_radiopilote("discoqueen")
# Avoid skipping blank with classic music !!
classique = channel_radiopilote(skip=false,"classique")
That70Sound = channel_radiopilote("That70Sound")
metal = channel_radiopilote("metal")
reggae = channel_radiopilote("reggae")
Rock = channel_radiopilote("Rock")

# Group those sources in a separate
# clock (good for multithreading/multicore)
clock.assign_new([jazz,That70Sound,metal,reggae])

# === Mixing live ===

# To create a channel from a basic source, add:
# - a new-track notification for radiopilote
# - metadata rewriting
# - the live shows
# - the failsafe 'interlude' source to channels
# - blank detection
def mklive(source) =
  # Transition function: if transitioning to the live, fade out the old source
  # if transitioning from live, fade.in the new source. NOTE: We cannot skip the
  # current song because reloading new songs for all the sources when live
  # starts costs too much CPU.
  def trans(old,new) =
    if source.id(new) == source.id(live) then
      log("Transition to live!")
      add([new,fade.out(old)])
    elsif source.id(old) == source.id(live) then
      log("Transitioning from live!")
      add([fade.in(new),old])
    else
      log("Dummy transition")
      new
    end
  end
  fallback(track_sensitive=false,
    transitions=[trans,trans,trans],
    [live,source,interlude])
end

# Create a channel using mklive(), encode and output it to icecast.
def mkoutput(~out=out,mount,source,name,genre)
  out(id=mount,mount=mount,name=name,genre=genre,
     mklive(source)
     )
end

# === Outputs ===

mkoutput("jazz", jazz, "RadioPi - Canal Jazz","jazz")
mkoutput("discoqueen", discoqueen, "RadioPi - Canal DiscoQueen","discoqueen")
mkoutput("classique", classique, "RadioPi - Canal Classique","classique")
mkoutput("That70Sound", That70Sound,
                       "RadioPi - Canal That70Sound","That70Sound")
mkoutput("metal", metal, "RadioPi - Canal Metal","metal")
mkoutput("reggae", reggae, "RadioPi - Canal Reggae","reggae")
mkoutput("Rock", Rock, "RadioPi - Canal Rock","Rock")

# Test outouts
mkoutput(out=out_aac,"reggae.aacp", reggae, "RadioPi - Canal Reggae \
                                     (64 kbits AAC+ test stream)","reggae")
mkoutput(out=out_aac32,"reggae.aacp32", reggae, "RadioPi - Canal Reggae \
                                      (32 kbits AAC+ test stream)","reggae")
