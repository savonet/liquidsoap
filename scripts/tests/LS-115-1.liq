s = add(normalize=true,[
  sine(),
  sequence([sine(amplitude=0.3,duration=1.),
            once(sine(duration=1.))])
])

output.dummy(s)

add_timeout(3.,{
  print("if this terminates, then TEST PASSED")
  shutdown()
  (-1.)
})
