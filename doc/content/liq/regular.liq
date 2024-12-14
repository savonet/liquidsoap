promotions = sine()
other_source = sine()

# BEGIN
# (1200 sec = 20 min)
timed_promotions = delay(1200., promotions)
main_source = fallback([timed_promotions, other_source])

# END
output.dummy(main_source)
