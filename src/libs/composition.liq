let settings.source =
  settings.make.void(
    "Source settings"
  )

let settings.source.composition =
  settings.make.void(
    "Settings for source composition"
  )

let settings.source.composition.max_fade =
  settings.make(
    description="Maximum fade duration (in seconds) applied when switching away \
     from a source mid-track. The actual fade is capped to the remaining time if \
     shorter.",
    1.
  )

# Default on_select for both file and live composition profiles. When `ending`
# is non-null (the ending source was preempted mid-track) and has only PCM audio
# content: the starting source plays right away and the ending one is mixed
# underneath it. If the ending source has at most
# `settings.source.composition.max_fade` seconds remaining, it finishes
# naturally; otherwise it fades out over `max_fade` seconds. Any other content
# switches immediately: the body is typed for PCM audio, and the runtime type checks
# reject any source it builds from other content. When `replay_metadata` is
# true, replays the latest metadata on the starting source.
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
        insert_metadata(override=false, null.get(last_metadata))
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

      max_fade = settings.source.composition.max_fade()
      rem = source.remaining(leaving)
      if
        not has_pcm_audio
      then
        starting()
      else
        overlap =
          if
            rem > 0. and rem <= max_fade
          then
            log.important(
              label=id(),
              "Letting #{id()} finish under the starting source.."
            )
            rem
          else
            log.important(
              label=id(),
              "Ending #{id()} with a #{max_fade}s fade out.."
            )
            max_fade
          end

        tail = max_duration(overlap, leaving)
        tail =
          overlap < rem or rem < 0. ? fade.out(duration=overlap, tail) : tail

        # A live source keeps producing whether it is read or not, so the
        # starting source is read from the first frame on. The mix keeps its
        # track marks and metadata; the tail only contributes audio.
        starting = starting()
        tracks = source.tracks(starting)
        mix =
          source(
            tracks.{
              audio =
                track.audio.add(
                  normalize=false,
                  [tracks.audio, source.tracks(tail).audio]
                )
            }
          )

        # Dropping the mix once the tail is over releases the leaving source.
        (sequence(merge=true, [max_duration(overlap, mix), starting]) : source)
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
