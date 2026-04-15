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

# Default on_select for both file and live composition profiles. When `ending`
# is non-null (the ending source was preempted mid-track), fades it out (up to
# 1s or remaining time). When `replay_metadata` is true, replays the latest
# metadata on the starting source.
def source.composition.default_on_select(
  (
  {starting, ending, replay_metadata}:
  {starting: source, ending: source?, replay_metadata: bool}
  )
) =
  let {id, insert_metadata, last_metadata} = source.methods(starting)

  # starting can be an active source to insert metadata when the
  # source is about to be used.
  def starting() =
    if
      replay_metadata
    then
      last_metadata = last_metadata()
      if
        null.defined(last_metadata)
      then
        log.important(
          label=id(),
          "Switching to #{id()}: inserting latest metadata"
        )
        insert_metadata(null.get(last_metadata))
      else
        log.important(
          label=id(),
          "Switching to #{id()}: no latest metadata to insert"
        )
      end
    end
    starting
  end
  s =
    if
      null.defined(ending)
    then
      leaving = null.get(ending)

      let {id} = source.methods(leaving)

      has_pcm_audio =
        list.map(
          fun ((field, _)) -> field,
          list.filter(
            fun ((_, fmt)) -> null.defined(format.description(fmt)?.pcm),
            source.content(leaving)
          )
        ) ==
          ["audio"]

      max_fade = 1.
      rem = source.remaining(leaving)
      if
        rem == 0. or not has_pcm_audio
      then
        starting()
      else
        effective_fade = if rem >= 0. then min(rem, max_fade) else max_fade end
        log.important(
          label=id(),
          "Ending #{id()} with a effective_fade #{effective_fade} fade out.."
        )

        leaving =
          fade.out(
            duration=effective_fade,
            max_duration(effective_fade, sequence([leaving, switch([])]))
          )

        count = ref(0)
        def next() =
          c = count()
          ref.incr(count)
          if c == 0 then leaving elsif c == 1 then starting() else null end
        end

        source.dynamic(track_sensitive=true, next)
      end
    else
      starting()
    end

  (s : source)
end

source.composition.file(
  {
    on_leave =
      # skip the source when it is preempted mid-track (track_sensitive=false),
      # so it starts fresh on next selection. When switching at a track
      # boundary (track_sensitive=true), the source has finished naturally and
      # no skip is needed.

      fun ({source = s, track_sensitive}) ->
        if
          not track_sensitive
        then
          log.important(
            label=source.id(s),
            "Source left in the middle of a track: skipping current track"
          )
          let {skip, clear_last_metadata} = source.methods(s)
          clear_last_metadata()
          skip()
        end,
    on_select = source.composition.default_on_select,
    track_sensitive = true,
    replay_metadata = true
  }
)

source.composition.live(
  {
    on_leave = fun (_) -> (),
    on_select = source.composition.default_on_select,
    track_sensitive = false,
    replay_metadata = true
  }
)
