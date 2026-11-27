%include "../test.liq"

# A configuration libFLAC would refuse is refused with the format: a literal
# when the script is checked, a computed value when it is evaluated. It used to
# reach the library, which wrote a 0-byte file and reported nothing.

%ifencoder %flac
samplerate = int_of_string("1000000")
try
  ignore(%flac(samplerate = samplerate))
  test.fail()
catch err do
  print(
    "refused: #{err.message}"
  )
  if err.kind == "encoder" then test.pass() else test.fail() end
end
%else
test.skip()
%endif
