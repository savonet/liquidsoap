
# This is an example of script which is fallible,
# because telnet and the playlist in the afternoon are not trusted.

# Try > liquidsoap -c -a fallible.liq

let morning = local_playlist "playlist.pls" in
let afternoon = playlist "ftp://foo/pub/afternoon.pls" in
  fallback
    [ telnet ;
      ( date_switch [ ("0-11h", morning) ;
		      ("12-23h", afternoon) ] ) ]
