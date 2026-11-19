%include "../test.liq"

# A rotation of one jingle then three songs sits under a switch. A one-track
# insert becomes due during the third song. The rotation is left exactly where
# that song ends, which is also where its next turn starts.
#
# The track after the insert must be the jingle, played in full: the rotation
# must not have started, or counted, a track nobody heard.

def tracks(title, duration) =
  metadata.map(
    insert_missing=true,
    fun (_) -> [("title", title)],
    source.dynamic(track_sensitive=true, {sine(duration=duration)})
  )
end

jingle_duration = 0.5
jingle = tracks("jingle", jingle_duration)
song = tracks("song", 0.31)
rotation = rotate([jingle.{weight = 1}, song.{weight = 3}])

insert_due = ref(false)
insert =
  tracks("insert", 0.4).{
    on_select =
      fun (x) ->
        begin
          insert_due := false
          source.composition.default_on_select(x)
        end
  }

radio = switch([(insert_due, insert), ({true}, rotation)])

expected = ["jingle", "song", "song", "song", "insert", "jingle", "song"]
titles = ref([])
starts = ref([])

def on_track(m) =
  titles := [...titles(), m["title"]]
  starts := [...starts(), source.time(radio)]
  if list.length(titles()) == 4 then insert_due := true end
  if
    list.length(titles()) == list.length(expected)
  then
    test.equal(titles(), expected)
    jingle_length = list.nth(starts(), 6) - list.nth(starts(), 5)
    if
      jingle_length < jingle_duration - 0.05
    then
      test.fail(
        "jingle cut short: #{jingle_length}s"
      )
    end

    test.pass()
  end
end

radio.on_track(synchronous=true, on_track)

clock.assign_new(sync="none", [radio])
output.dummy(fallible=true, radio)
