settings.decoder.decoders := ["srt"]

s = single("../media/test-subtitle.srt")

count = ref(0)

def map_subtitle(sub) =
  c = count()
  ref.incr(count)
  if
    c == 0
  then
    # Change text only
    {
      text=
        "Modified first"
    }
  elsif
    c == 1
  then
    # Change format only
    {format="ass"}
  elsif
    c == 2
  then
    # Change forced only
    {forced=true}
  elsif
    c == 3
  then
    # Remove subtitle
    null
  else
    # Pass through unchanged (empty record)
    {}
  end
end

s = subtitles.map(map_subtitle, s)

check_count = ref(0)

def check_subtitle(sub) =
  c = check_count()
  ref.incr(check_count)
  print(
    "Received subtitle #{c}: text=#{sub.text}, format=#{sub.format}, forced=#{
      sub.forced
    }"
  )
  if
    c == 0
  then
    # Should have modified text
    if
      sub.text !=
        "Modified first"
    then
      test.fail()
    end
  elsif
    c == 1
  then
    # Should have ass format
    if sub.format != "ass" then test.fail() end
  elsif
    c == 2
  then
    # Should be forced
    if not sub.forced then test.fail() end
  elsif
    c == 3
  then
    # Subtitle 4 was removed, so this should be the 5th subtitle
    if
      sub.text !=
        "Fifth subtitle line"
    then
      test.fail()
    end
    test.pass()
  end
end

s = on_subtitle(check_subtitle, s)

clock.assign_new(sync='none', [s])
output.dummy(fallible=true, s)
