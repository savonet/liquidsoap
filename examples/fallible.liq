#
# This is an example of script which is fallible,
# because the request queue and the afternoon playlist are not trusted.
#

# Try running: liquidsoap -c fallible.liq

morning = playlist.safe("playlist.pls")
afternoon = playlist("ftp://foo/pub/afternoon.pls")

radio = fallback([ request.queue(),
                   switch([ ({0h-12h}, morning),
                            ({12h-24h}, afternoon) ]) ])

output.file(%vorbis,"/tmp/radio.ogg",radio)
