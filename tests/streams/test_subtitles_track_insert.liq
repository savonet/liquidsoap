t = track.subtitles.insert()

thread.run(
  delay=1.,
  {
    t.insert_subtitle(
      {duration=2.0, text="Inserted subtitle", format="ass", forced=true}
    )
  }
)

def check_subtitle(sub) =
  print("Received subtitle: text=#{sub.text}")
  if sub.text == "Inserted subtitle" then
    if sub.format == "ass" and sub.forced then
      test.pass()
    else
      test.fail()
    end
  end
end

t = track.on_subtitle(check_subtitle, t)

s = source({subtitles=t})

clock.assign_new(sync='none', [s])
output.dummy(fallible=true, s)
