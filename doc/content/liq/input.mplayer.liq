# Stream data from mplayer
# @category Source / Input
# @param s data URI.
# @param ~restart restart on exit.
# @param ~restart_on_error restart on exit with error.
# @param ~buffer Duration of the pre-buffered data.
# @param ~max Maximum duration of the buffered data.
def input.mplayer(
  ~id="input.mplayer",
  ~restart=true,
  ~restart_on_error=false,
  ~buffer=0.2,
  ~max=10.,
  s
) =
  input.external.rawaudio(
    id=id,
    restart=restart,
    restart_on_error=restart_on_error,
    buffer=buffer,
    max=max,
    "mplayer -really-quiet -ao pcm:file=/dev/stdout -vc null -vo null #{
      process.quote(s)
    } 2>/dev/null"
  )
end
