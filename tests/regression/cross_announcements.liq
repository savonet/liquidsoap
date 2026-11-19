%include "../test.liq"

# Each track of the child is announced once, in order, and no earlier than
# its audio, whatever the transition keeps of its two sources: both mixed,
# both in sequence, the ending one only, the starting one only, neither. The
# third track repeats the metadata of the second: it is announced all the
# same. A crossing puts one mark, on the first sample of the transition.

duration = 1.
length = 3.

def tagged(title) =
  metadata.map(
    insert_missing=true,
    fun (_) -> [("title", title)],
    sine(duration=length)
  )
end

pending = ref(0)

# Crosses three tracks of 3 s with a duration of 1 s and checks where tracks
# begin and where each track of the child is announced, as (title, time).
def check(~label, ~deduplicate, ~tracks_expected, ~announced_at, transition) =
  ref.incr(pending)
  s =
    cross(
      duration=duration,
      deduplicate=deduplicate,
      transition,
      sequence([tagged("a"), tagged("b"), tagged("b")])
    )

  tracks = ref([])
  announcements = ref([])
  s.on_track(
    synchronous=true,
    fun (m) -> tracks := [...tracks(), (m["title"], source.time(s))]
  )
  s.on_metadata(
    synchronous=true,
    fun (m) ->
      announcements := [...announcements(), (m["title"], source.time(s))]
  )

  clock.assign_new(sync="none", [s])
  o = output.dummy(fallible=true, s)
  o.on_stop(
    synchronous=true,
    fun () ->
      begin
        def near(x, y) =
          abs(x - y) <= 0.021
        end

        def same(got, expected) =
          list.length(got) == list.length(expected)
          and list.for_all(
            fun (pair) ->
              begin
                let ((title, at), (title_expected, at_expected)) = pair
                title == title_expected and near(at, at_expected)
              end,
            list.mapi(fun (i, x) -> (x, list.nth(expected, i)), got)
          )
        end

        if
          not same(announcements(), announced_at)
        then
          test.fail(
            "#{label}: announced #{announcements()}"
          )
        end

        if
          not same(tracks(), tracks_expected)
        then
          test.fail(
            "#{label}: tracks #{tracks()}"
          )
        end

        pending := pending() - 1
        if pending() == 0 then test.pass() end
      end
  )
end

def mix(ending, starting) =
  add(normalize=false, [ending.source, starting.source])
end

def in_sequence(ending, starting) =
  sequence([ending.source, starting.source])
end

def ending_only(ending, _) =
  ending.source
end

def starting_only(_, starting) =
  starting.source
end

def neither(_, _) =
  sine(duration=0.5)
end

list.iter(
  fun (deduplicate) ->
    begin
      # Mixed: the first crossing starts at 2 s, lasts 1 s; the second at 4 s.
      check(
        label="mix, deduplicate=#{deduplicate}",
        deduplicate=deduplicate,
        tracks_expected=[("a", 0.), ("b", 2.), ("b", 4.)],
        announced_at=[("a", 0.), ("b", 2.), ("b", 4.)],
        mix
      )

      # In sequence nothing overlaps: 2 s of the transition, and the next
      # track is heard, and marked by the sequence, 1 s into it.
      check(
        label="sequence, deduplicate=#{deduplicate}",
        deduplicate=deduplicate,
        tracks_expected=[("a", 0.), ("", 2.), ("b", 3.), ("", 5.), ("b", 6.)],
        announced_at=[("a", 0.), ("b", 3.), ("b", 6.)],
        in_sequence
      )

      # Only the ending source is played, for 1 s: the next track is first
      # heard, and announced, when the transition is over. Its first second
      # was not played, so it has 2 s left, and the next crossing is at 4 s.
      check(
        label="ending only, deduplicate=#{deduplicate}",
        deduplicate=deduplicate,
        tracks_expected=[("a", 0.), ("", 2.), ("", 4.)],
        announced_at=[("a", 0.), ("b", 3.), ("b", 5.)],
        ending_only
      )

      # Only the starting source is played: the next track is heard, marked
      # and announced on the first sample of the transition.
      check(
        label="starting only, deduplicate=#{deduplicate}",
        deduplicate=deduplicate,
        tracks_expected=[("a", 0.), ("b", 2.), ("b", 4.)],
        announced_at=[("a", 0.), ("b", 2.), ("b", 4.)],
        starting_only
      )

      # Half a second of something else: the next track is announced when
      # that is over.
      check(
        label="neither, deduplicate=#{deduplicate}",
        deduplicate=deduplicate,
        tracks_expected=[("a", 0.), ("", 2.), ("", 3.5)],
        announced_at=[("a", 0.), ("b", 2.5), ("b", 4.)],
        neither
      )
    end,
  [true, false]
)
