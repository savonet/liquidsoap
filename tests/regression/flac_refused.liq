%include "../test.liq"

# A configuration libFLAC refuses fails the output. Its statuses were never
# checked, so the output wrote a 0-byte file and reported nothing.

def on_error(_) =
  print(
    "error raised"
  )
  test.pass()
  null
end

%ifencoder %flac
output.file(
  reopen_on_error=on_error,
  %flac(samplerate = 1000000),
  file.temp(cleanup=true, "flac_refused", ".flac"),
  sine()
)
%else
test.skip()
%endif
