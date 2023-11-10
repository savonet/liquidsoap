promotions = sine()
other_source = sine()

# (1200 sec = 20 min)
timed_promotions = delay(1200., promotions)
main_source = fallback([timed_promotions, other_source])
output.dummy(main_source)
