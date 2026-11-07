# Routing libsrt's debug logs does not slow the stream down (spec/library.md §2).
%include "../test.liq"
%include "helpers.inc"

settings.srt.log.verbosity := "debug"
settings.srt.log.level := 5
port = 19592
o = output.srt(mode="listener", port=port, %wav, sine())

def run() =
  code =
    run_peer(
      "receive --port #{port} --bytes flood.wav --duration 5"
    )
  if
    code != 0
  then
    test.fail(
      "no connection"
    )
  end
  received = file_size("flood.wav")
  expected = int_of_float(0.8 * 4. * 176400.)
  if
    received < expected
  then
    test.fail(
      "#{received} bytes in 5s, expected at least #{expected}"
    )
  end
  ignore(o)
  test.pass()
end

thread.run(delay=0.1, run)
