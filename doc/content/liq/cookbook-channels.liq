# BEGIN
enable_replaygain_metadata()

live =
  blank.strip(
    max_blank=10.,
    threshold=-50.,
    input.harbor("live", port=8005, password="hackme")
  )

interlude = single("/path/to/interlude.mp3")

# Build the programmed content of a channel.
def channel(~skip_silence=true, name) =
  def next() =
    uri =
      list.hd(
        default="",
        process.read.lines(
          "next-song #{process.quote(name)}"
        )
      )
    if uri == "" then null else request.create(uri) end
  end

  s = request.dynamic(id="#{name}_requests", next)
  s = normalize_track_gain(s)
  s = if skip_silence then blank.skip(max_blank=10., s) else s end
  crossfade(s)
end

# Add the live show and the interlude, then send the channel to Icecast.
def output_channel(~genre, (name:string), s) =
  radio = fallback(id="#{name}_radio", [(live : source), s, interlude])
  output.icecast(
    %mp3,
    id=name,
    host="localhost",
    port=8000,
    password="hackme",
    mount=name,
    name="My radio - #{name}",
    genre=genre,
    radio
  )
end

output_channel(genre="jazz", "jazz", channel("jazz"))
output_channel(genre="reggae", "reggae", channel("reggae"))

# Classical music has quiet passages that should not be skipped.
output_channel(
  genre="classical",
  "classical",
  channel(skip_silence=false, "classical")
)
