%include "../test.liq"

# Asking a source whether it is ready changes nothing: a rotation that is
# probed ten times in every cycle plays the same tracks, and asks its
# children for the same number of sources, as one that is never probed.

tracks_wanted = 24
finished = ref([])

def rotation(~probed) =
  asked = ref(0)
  def tracks(title) =
    def next() =
      ref.incr(asked)
      metadata.map(
        insert_missing=true,
        fun (_) -> [("title", title)],
        sine(duration=0.07)
      )
    end

    source.dynamic(track_sensitive=true, next)
  end

  s = rotate([tracks("a").{weight = 1}, tracks("b").{weight = 2}])
  titles = ref([])

  def on_track(m) =
    titles := [...titles(), m["title"]]
    if
      list.length(titles()) == tracks_wanted
    then
      finished := [...finished(), (titles(), asked())]
      if
        list.length(finished()) == 2
      then
        let (unprobed_titles, unprobed_asked) = list.nth(finished(), 0)
        let (probed_titles, probed_asked) = list.nth(finished(), 1)
        test.equal(unprobed_titles, probed_titles)
        test.equal(unprobed_asked, probed_asked)
        test.equal(
          list.prefix(6, probed_titles),
          ["a", "b", "b", "a", "b", "b"]
        )
        test.pass()
      end
    end
  end

  s.on_track(synchronous=true, on_track)

  if
    probed
  then
    s.on_frame(
      synchronous=true,
      fun () ->
        list.iter(fun (_) -> ignore(s.is_ready()), list.init(10, fun (i) -> i))
    )
  end

  clock.assign_new(sync="none", [s])
  output.dummy(fallible=true, s)
end

rotation(probed=false)
rotation(probed=true)
