BEET = "/usr/bin/beet"

# BEGIN
def beets(id, query) =
  beets_src =
    blank.eat(
      id="#{id}_",
      start_blank=true,
      max_blank=1.0,
      threshold=-45.0,
      amplify(
        override="replaygain_track_gain",
        1.0,
        request.dynamic(
          id=id,
          retry_delay=1.,
          {
            request.create(
              string.trim(
                process.read(
                  "#{BEET} random -f '$path' #{query}"
                )
              )
            )
          }
        )
      )
    )
  (beets_src : source)
end
