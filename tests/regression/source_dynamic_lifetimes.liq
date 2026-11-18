%include "../test.liq"

# Sources handed out by `source.dynamic` do not pile up: after hundreds of
# joins no more are awake than after one, each one replaced is put to sleep,
# and the function is not asked for more sources than are played. A source
# returned again and again is woken once and never put to sleep.

woken = ref(0)
asleep = ref(0)
most_awake = ref(0)
asked = ref(0)

def fresh() =
  ref.incr(asked)
  s = sine(duration=0.05)
  s.on_wake_up(
    synchronous=true,
    fun () ->
      begin
        ref.incr(woken)
        awake = woken() - asleep()
        if most_awake() < awake then most_awake := awake end
      end
  )
  s.on_shutdown(synchronous=true, fun () -> ref.incr(asleep))
  s
end

replaced = source.dynamic(track_sensitive=true, fresh)

# A silence of fixed duration starts a new track each time it ends.
kept = blank(duration=0.05)
kept_woken = ref(0)
kept_asleep = ref(0)
kept.on_wake_up(synchronous=true, fun () -> ref.incr(kept_woken))
kept.on_shutdown(synchronous=true, fun () -> ref.incr(kept_asleep))
same = source.dynamic(track_sensitive=true, {kept})

tracks_wanted = 300
still_playing = ref(2)

def finished() =
  still_playing := still_playing() - 1
  if still_playing() == 0 then test.pass() end
end

replaced_tracks = ref(0)
replaced.on_track(
  synchronous=true,
  fun (_) ->
    begin
      ref.incr(replaced_tracks)
      if
        replaced_tracks() == tracks_wanted
      then
        # The one playing, and at most one prepared for the next track.
        if
          2 < most_awake() or 2 < woken() - asleep()
        then
          test.fail(
            "#{most_awake()} sources awake at once, #{woken() - asleep()} now"
          )
        end

        if
          tracks_wanted + 1 < asked()
        then
          test.fail(
            "#{asked()} sources asked for #{tracks_wanted} tracks"
          )
        end

        finished()
      end
    end
)

same_tracks = ref(0)
same.on_track(
  synchronous=true,
  fun (_) ->
    begin
      ref.incr(same_tracks)
      if
        same_tracks() == tracks_wanted
      then
        if
          kept_woken() != 1 or kept_asleep() != 0
        then
          test.fail(
            "kept source woken #{kept_woken()} times, put to sleep #{
              kept_asleep()
            } times"
          )
        end

        finished()
      end
    end
)

def play(s) =
  clock.assign_new(sync="none", [s])
  output.dummy(fallible=true, s)
end

play(replaced)
play(same)
