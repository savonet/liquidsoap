tmp = file.temp("foo", "mp3")
on_cleanup({file.remove(tmp)})
a = sine(duration=2.)
b = sine(duration=2.)

# This function will be type-checked as a generic
# fun ({..., starting: source('a), ...}) -> source('a). We need to make sure
# that it returns a mono source when used as transition
# for a mono fallback.
def transition({starting, ..._}) =
  fade.in(starting)
end

s = fallback([a.{on_select=transition}, b.{on_select=transition}])

clock.assign_new(sync='none', [s])

o = output.file(fallible=true, %mp3(mono), tmp, s)
o.on_stop(synchronous=true, test.pass)
