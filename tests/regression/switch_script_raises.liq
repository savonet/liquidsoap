%include "../test.liq"

# Script code that raises inside a switch is logged and the stream goes on:
# a predicate counts as false, a getter as its default, a raising
# `on_select` plays the child as it is, a raising `on_leave` does nothing.

# Tracks of one second titled `title`, as many as the test needs.
def tagged(title) =
  sequence(
    list.init(
      30,
      fun (_) ->
        metadata.map(
          insert_missing=true,
          fun (_) -> [("title", title)],
          sine(duration=1.)
        )
    )
  )
end

calls = ref(0)

def predicate() =
  ref.incr(calls)
  if
    calls() mod 3 == 0
  then
    error.raise(
      error.failure,
      "predicate failed"
    )
  end
  true
end

a = tagged("a")
a =
  a.{
    on_select =
      fun (_) ->
        error.raise(
          error.failure,
          "on_select failed"
        ),
    on_leave =
      fun (_) ->
        error.raise(
          error.failure,
          "on_leave failed"
        ),
    track_sensitive =
      fun () ->
        error.raise(
          error.failure,
          "track_sensitive failed"
        )
  }

b = tagged("b")
s = switch([(predicate, a), ({true}, b)])

titles = ref([])
s.on_track(synchronous=true, fun (m) -> titles := [...titles(), m["title"]])

clock.assign_new(sync="none", [s])
o = output.dummy(fallible=true, s)
s.on_frame(
  synchronous=true,
  fun () ->
    if
      10. <= source.time(s)
    then
      if
        list.length(titles()) < 8
      then
        test.fail(
          "only #{list.length(titles())} tracks: #{titles()}"
        )
      end

      if
        not list.exists(fun (t) -> t == "a", titles())
        or not list.exists(fun (t) -> t == "b", titles())
      then
        test.fail(
          "both children should have played: #{titles()}"
        )
      end

      test.pass()
    end
)
