# A command cancelled by `process.run`'s timeout must be terminated, not left
# running in the background.
marker = file.temp("liq-gh5630", ".txt")
file.remove(marker)

def check() =
  if
    file.exists(marker)
  then
    file.remove(marker)
    test.fail()
  else
    test.pass()
  end
end

def f() =
  p =
    process.run(
      timeout=0.5,
      "sleep 2 && touch #{process.quote(marker)}"
    )
  if p.status != "timeout" then test.fail() end
  thread.run(delay=3., check)
end

output.dummy(blank())
thread.run(delay=0.5, f)
