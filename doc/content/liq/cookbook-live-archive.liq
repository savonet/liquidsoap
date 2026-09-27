live = input.harbor("live", port=8000, password="hackme")

# BEGIN
filename =
  {
    time.string(
      '/archives/$(if $(title),"$(title)","Unknown show")$(if $(artist)," by \
       $(artist)") - %Y-%m-%d, %H-%M-%S.ogg'
    )
  }

output.file(
  %vorbis,
  fallible=true,
  reopen_on_metadata=fun (_) -> true,
  filename,
  live
)
