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
        fade_duration = rem > 0. and rem <= max_fade ? rem : max_fade
        log.important(
          label=id(),
          "Ending #{id()} with a #{fade_duration}s fade out.."
        )

        # The tail stops at the end of the leaving track.
        tail =
          fade.out(
            duration=fade_duration,
            max_duration(max_fade, once(leaving))
          )

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
        mix = source.available(mix, {source.is_ready(tail)})

        # Once the tail is over, hand over to the starting source itself at a
        # frame boundary, which releases the leaving source. That boundary is
        # not a new track: merge it, and only it.
        # `next` is consulted at every boundary, so the handover is flagged
        # once, when the mix is first found over.
        handed_over = ref(false)
        handing_over = ref(false)

        # A handover that falls where the starting source begins a track of
        # its own is that track's start: nothing is left to merge, and a flag
        # left up would merge its next real track end.
        let {on_track} = source.methods(starting)
        ignore(on_track(synchronous=true, fun (_) -> handing_over := false))
        def next() =
          if
            source.is_ready(mix)
          then
            mix
          else
            if
              not handed_over()
            then
              handed_over := true
              handing_over := true
            end

            starting
          end
        end

        (
          source.dynamic(
            track_sensitive=true,
            merge={handing_over.exchange(false)},
            next
          )
        :
          source
        )
      end
    else
      starting()
    end

  (s : source)
end

source.composition.file(
  {
    on_leave =
      # skip the source when it is preempted mid-track, so it starts fresh on
      # next selection. Left at a track boundary, it has finished naturally
      # and no skip is needed.

      fun ({source = s, preempted}) ->
        if
          preempted
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
