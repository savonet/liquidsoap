%include "../test.liq"

# A source read by several readers advances at most one frame per tick, and
# every reader gets its data at its own position in the frame. Here `s` is
# read whole by one output, and by a fallback that only reaches it in a
# later streaming cycle of the same tick, after a track ending mid-frame. The
# first output must still hear `s` unbroken: its file is the same, sample for
# sample, as that of a sine nobody else reads.

length = 3.

s = sine()

# A track ending inside a frame, then nothing: the fallback moves to `s` in a
# second streaming cycle.
p = once(sine(duration=1.2345))

a_file = file.temp(cleanup=true, "shared", ".wav")
reference_file = file.temp(cleanup=true, "reference", ".wav")

clock.assign_new(sync="none", [s, p])
a = output.file(fallible=true, %wav, a_file, max_duration(length, s))
output.dummy(fallible=true, fallback([p, s]))

reference = sine()
clock.assign_new(sync="none", [reference])
r =
  output.file(
    fallible=true,
    %wav,
    reference_file,
    max_duration(length, reference)
  )

stopped = ref(0)

def check() =
  ref.incr(stopped)
  if
    stopped() == 2
  then
    if
      file.size(a_file) != file.size(reference_file)
    then
      test.fail(
        "#{file.size(a_file)} bytes written, #{file.size(reference_file)} \
         expected"
      )
    elsif
      file.digest(a_file) != file.digest(reference_file)
    then
      test.fail(
        "the shared sine was not heard unbroken"
      )
    else
      test.pass()
    end
  end
end

a.on_stop(synchronous=true, check)
r.on_stop(synchronous=true, check)
