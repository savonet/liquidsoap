%include "../test.liq"

# The default transition only mixes PCM audio: a source that also carries
# video is switched without a fade.

def with_video(s) =
  source.mux.video(video=blank(), s)
end

music = with_video(sine(440.))

live_on = ref(false)
live = source.available(with_video(sine(880.)), live_on)

s = fallback([live.{track_sensitive = false}, music])

frames = ref(0)
s.on_frame(
  synchronous=true,
  fun () ->
    begin
      ref.incr(frames)
      if frames() == 10 then live_on := true end
      if
        frames() == 30
      then
        if s.selected() == live then test.pass() else test.fail() end
      end
    end
)

clock.assign_new(sync="none", [s])
output.dummy(fallible=true, s)
