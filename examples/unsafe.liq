
# Example of *not really* unsafe script.
# The problem is that *it seems* that babasse can be accessed
# by two different way at the same time.
# Actually, we know that the switch never asks frames on both sides.
# But the type system doesn't.

# Try > liquidsoap -c -s unsafe.liq

let libre = telnet in
let babasse = playlist "./babasse.techno.pls" in
let defaut = local_playlist "./pls.pls" in
  add [
    libre ;
    ( date_switch [ ("1-7h", babasse) ; ("0h || 7-23h", babasse) ] ) ;
    defaut ]
