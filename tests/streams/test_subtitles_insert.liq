s = sine()
s = subtitles.insert(s)

thread.run(
  delay=1.,
  {
    s.insert_subtitle(
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

s = on_subtitle(check_subtitle, s)

clock.assign_new(sync='none', [s])
output.dummy(fallible=true, s)
