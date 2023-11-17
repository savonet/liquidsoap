s = sine()

# BEGIN
filename =
  {
    time.string(
      '/archive/$(if $(title),"$(title)","Unknown \
       archive")-%Y-%m-%d/%Y-%m-%d-%H_%M_%S.mp3'
    )
  }
output.file(%mp3, filename, s, reopen_on_metadata=fun (_) -> true)
