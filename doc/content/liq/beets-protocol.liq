BEET = "/usr/bin/beets"

# BEGIN
def beets_protocol(~rlog, ~maxtime, arg) =
  timeout = maxtime - time()
  command =
    "#{BEET} random -f '$path' #{arg}"
  p = process.run(timeout=timeout, command)
  if
    p.status == "exit" and p.status.code == 0
  then
    string.trim(p.stdout)
  else
    rlog(
      "Failed to execute #{command}: #{p.status} (#{p.status.code}) #{p.stderr}"
    )
    null.make()
  end
end
protocol.add(
  "beets",
  beets_protocol,
  syntax=
    "same arguments as beet's random module, see \
     https://beets.readthedocs.io/en/stable/reference/query.html"
)
