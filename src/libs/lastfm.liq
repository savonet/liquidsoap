%ifdef audioscrobbler.submit

librefm = ()
lastfm = ()

# Submit metadata to libre.fm using the audioscrobbler protocol.
# @category Interaction
# @flag extra
# @param ~source Source for tracks. Should be one of: "broadcast", "user", "recommendation" or "unknown". Since liquidsoap is intended for radio broadcasting, this is the default. Sources other than user don't need duration to be set.
# @param ~length Try to submit length information. This operation can be CPU intensive. Value forced to true when used with the "user" source type.
def librefm.submit(~user,~password,~source="broadcast",~length=false,m) =
  audioscrobbler.submit(user=user,password=password,
                source=source,length=length,
                host="turtle.libre.fm",port=80,
                m)
end

# Submit metadata to lastfm.fm using the audioscrobbler protocol.
# @category Interaction
# @flag extra
# @param ~source Source for tracks. Should be one of: "broadcast", "user", "recommendation" or "unknown". Since liquidsoap is intended for radio broadcasting, this is the default. Sources other than user don't need duration to be set.
# @param ~length Try to submit length information. This operation can be CPU intensive. Value forced to true when used with the "user" source type.
def lastfm.submit(~user,~password,~source="broadcast",~length=false,m) =
  audioscrobbler.submit(user=user,password=password,
                source=source,length=length,
                host="post.audioscrobbler.com",port=80,
                m)
end

# Submit metadata to libre.fm using the audioscrobbler protocol (nowplaying mode).
# @category Interaction
# @flag extra
# @param ~length Try to submit length information. This operation can be CPU intensive. Value forced to true when used with the "user" source type.
def librefm.nowplaying(~user,~password,~length=false,m) =
  audioscrobbler.nowplaying(user=user,password=password,length=length,
                host="turtle.libre.fm",port=80,
                m)
end

# Submit metadata to lastfm.fm using the audioscrobbler protocol (nowplaying mode).
# @category Interaction
# @flag extra
# @param ~length Try to submit length information. This operation can be CPU intensive. Value forced to true when used with the "user" source type.
def lastfm.nowplaying(~user,~password,~length=false,m) =
  audioscrobbler.nowplaying(user=user,password=password,length=length,
                host="post.audioscrobbler.com",port=80,
                m)
end

let source_on_end = source.on_end
let source_on_metadata = source.on_metadata

# Submit songs using audioscrobbler, respecting the full protocol:
# First signal song as now playing when starting, and
# then submit song when it ends.
# @category Interaction
# @flag extra
# @param ~source Source for tracks. Should be one of: "broadcast", "user", "recommendation" or "unknown". Since liquidsoap is intended for radio broadcasting, this is the default. Sources other than user don't need duration to be set.
# @param ~length Try to submit length information. This operation can be CPU intensive. Value forced to true when used with the "user" source type.
# @param ~delay Submit song when there is only this delay left, in seconds.
# @param ~force If remaining time is null, the song will be assumed to be skipped or cut, and not submitted. Set to zero to disable this behaviour.
def audioscrobbler.submit.full(
     ~user,~password,
     ~host="post.audioscrobbler.com",~port=80,
     ~source="broadcast",~length=false,
     ~delay=10.,~force=false,s) =
  def f(m)
    audioscrobbler.nowplaying(
        user=user,password=password,
        host=host,port=port,length=length,m)
  end
  s = source_on_metadata(s, f)
  f = fun (rem,m) ->
      # Avoid skipped songs
      if rem > 0. or force then
        audioscrobbler.submit(
          user=user,password=password,
          host=host,port=port,length=length,
          source=source,m)
      else
        log(label="audioscrobbler.submit.full",
            level=4,"Remaining time null: \
                     will not submit song (song skipped ?)")
      end
  source_on_end(s,delay=delay,f)
end

# Submit songs to librefm using audioscrobbler, respecting the full protocol:
# First signal song as now playing when starting, and
# then submit song when it ends.
# @category Interaction
# @flag extra
# @param ~source Source for tracks. Should be one of: "broadcast", "user", "recommendation" or "unknown". Since liquidsoap is intended for radio broadcasting, this is the default. Sources other than user don't need duration to be set.
# @param ~length Try to submit length information. This operation can be CPU intensive. Value forced to true when used with the "user" source type.
# @param ~delay Submit song when there is only this delay left, in seconds. If remaining time is less than this value, the song will be assumed to be skipped or cut, and not submitted. Set to zero to disable this behaviour.
# @param ~force If remaining time is null, the song will be assumed to be skipped or cut, and not submitted. Set to zero to disable this behaviour.
def librefm.submit.full(
     ~user,~password,
     ~source="broadcast",~length=false,
     ~delay=10.,~force=false,s) =
    audioscrobbler.submit.full(
      user=user,password=password,
      source=source,length=length,
      host="turtle.libre.fm",port=80,
      delay=delay,force=force,s)
end

# Submit songs to lastfm using audioscrobbler, respecting the full protocol:
# First signal song as now playing when starting, and
# then submit song when it ends.
# @category Interaction
# @flag extra
# @param ~source Source for tracks. Should be one of: "broadcast", "user", "recommendation" or "unknown". Since liquidsoap is intended for radio broadcasting, this is the default. Sources other than user don't need duration to be set.
# @param ~length Try to submit length information. This operation can be CPU intensive. Value forced to true when used with the "user" source type.
# @param ~delay Submit song when there is only this delay left, in seconds. If remaining time is less than this value, the song will be assumed to be skipped or cut, and not submitted. Set to zero to disable this behaviour.
# @param ~force If remaining time is null, the song will be assumed to be skipped or cut, and not submitted. Set to zero to disable this behaviour.
def lastfm.submit.full(
     ~user,~password,
     ~source="broadcast",~length=false,
     ~delay=10.,~force=false,s) =
    audioscrobbler.submit.full(
      user=user,password=password,
      source=source,length=length,
      host="post.audioscrobbler.com",port=80,
      delay=delay,force=force,s)
end
%endif
