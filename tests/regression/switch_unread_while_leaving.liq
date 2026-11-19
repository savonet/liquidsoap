%include "../test.liq"

# A switch that stops being read while one of its children is still leaving
# must be able to play again. The leave only completes by being read, so a
# switch answering "nothing" until it completes would never come back.

def tagged(title, s) =
  metadata.map(insert_missing=true, fun (_) -> [("title", title)], s)
end

use_a = ref(true)
busy = ref(false)

inner =
  switch(
    id="inner",
    [(use_a, tagged("a", sine(440.))), ({not use_a()}, tagged("b", sine(880.)))]
  )

# Cuts in at once, without reading what it replaces.
other =
  tagged("other", sine(660.)).{on_select = source.composition.legacy_on_select}

radio = switch(id="radio", [(busy, other), ({true}, inner)])

frames = ref(0)
returned_at = ref(-1)
titles = ref([])

def on_frame() =
  ref.incr(frames)

  # `inner` cuts from "a" to "b": "a" is leaving for the length of the fade.
  if frames() == 20 then use_a := false end

  # Two frames later `radio` stops reading `inner`.
  if frames() == 22 then busy := true end

  # While unread, `inner` is asked for "a" again, then `radio` returns to it.
  if frames() == 40 then use_a := true end
  if
    frames() == 60
  then
    busy := false
    returned_at := list.length(titles())
  end

  if
    frames() == 120
  then
    heard =
      list.prefix(
        100,
        list.rev(
          list.prefix(list.length(titles()) - returned_at(), list.rev(titles()))
        )
      )
    if
      list.mem("a", heard)
    then
      test.pass()
    else
      test.fail(
        "after returning to inner: #{heard}"
      )
    end
  end
end

radio.on_frame(synchronous=true, on_frame)
radio.on_metadata(
  synchronous=true,
  fun (m) -> titles := [...titles(), m["title"]]
)

clock.assign_new(sync="none", [radio])
output.dummy(fallible=true, radio)
