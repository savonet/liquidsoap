%include "error.liq"
%include "null.liq"
%include "ref.liq"
%include "list.liq"
%include "math.liq"
%include "getter.liq"
%include "predicate.liq"
%include "settings.liq"
%include "log.liq"
%include "thread.liq"
%include "process.liq"
%include "string.liq"
%include "socket.liq"
%include "cron.liq"
%include "file.liq"
%include "switches.liq"
%include "utils.liq"
%include "clock.liq"
%include "metadata.liq"
%include "nfo.liq"
%include "playlist.liq"
%include "source.liq"
%include "tracks.liq"
%include "request.liq"
%include "audio.liq"
%include "fades.liq"
%include "autocue.liq"
%include "replaygain.liq"
%include "lufs.liq"
%include "http_codes.liq"
%include "http.liq"
%include "protocols.liq"
%include "resolvers.liq"
%include "icecast.liq"
%include "hls.liq"
%include "video.liq"
%include "ffmpeg.liq"
%include "profiler.liq"
%include "io.liq"
%include "sqlite.liq"
%include "medialib.liq"
%include "runtime.liq"
%include "server.liq"
%include "sqlite.liq"
%include "testing.liq"
%include "liquidsoap.liq"

%include_extra "extra/file.liq"
%include_extra "extra/native.liq"
%include_extra "extra/audio.liq"
%include_extra "extra/source.liq"
%include_extra "extra/http.liq"
%include_extra "extra/externals.liq"
%include_extra "extra/audioscrobbler.liq"
%include_extra "extra/server.liq"
%include_extra "extra/telnet.liq"
%include_extra "extra/interactive.liq"
%include_extra "extra/visualization.liq"
%include_extra "extra/openai.liq"
%include_extra "extra/spinitron.liq"
%include_extra "extra/metadata.liq"
%include_extra "extra/fades.liq"
%include_extra "extra/video.liq"

set_settings_ref(settings)

# Update both composition profiles: skip the source when it is preempted
# mid-track (track_sensitive=false), so it starts fresh on next selection.
# When switching at a track boundary (track_sensitive=true), the source has
# finished naturally and no skip is needed.
def _on_leave({source = s, track_sensitive}) =
  if not track_sensitive then source.skip(s) end
end

source.composition.file := {...source.composition.file(), on_leave=_on_leave}

source.composition.live := {...source.composition.live(), on_leave=_on_leave}

# Update the live composition profile: use a fade-out transition when switching
# away from a live source mid-track (ending is non-null).
source.composition.live :=
  {
    ...source.composition.live(),
    on_select=
      fun (x) ->
        begin
          starting = x.starting
          if
            null.defined(x?.ending)
          then
            begin
              leaving = null.get(x?.ending)
              max_fade = 1.
              rem = source.remaining(leaving)
              effective_fade =
                if rem >= 0. then min(rem, max_fade) else max_fade end
              (
                sequence(
                  [
                    (
                      fade.out(
                        duration=effective_fade,
                        max_duration(
                          effective_fade,
                          sequence([leaving, switch([])])
                        )
                      )
                    :
                      source
                    ),
                    starting
                  ]
                )
              :
                source
              )
            end
          else
            starting
          end
        end
  }
