tmp = file.temp("foo", "mp3")
on_cleanup({file.remove(tmp)})

a = sine(duration=2.)
b = sine(duration=2.)

# This function will be type-checked as a generic
# fun (source('a), source('a)) -> source('a). We
# need to make sure that it returns a mono source
# when used as transition for a mono fallback.
def transition(a, b) =
  add([fade.out(a), fade.in(b)])
end

s = fallback(transitions=[transition, transition], [a, b])

clock.assign_new(sync='none',[s])

output.file(fallible=true, on_stop=test.pass, %mp3(mono), tmp, s)
