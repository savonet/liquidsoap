
open Dtools

let _ =
  Conf.set_string "log.file" "/dev/null" ;
  Conf.set_bool "log.stdout" true

let log = Log.log ~label:"log" 3

let stop = ref false
let lock = Tutils.Mutex.create "foo"

let p s () =
  log (s^" runs") ;
  if s = "2" then Mutex.unlock lock ;
  while not !stop do
    Unix.sleep 1
  done ;
  log (s^" quits")

let main () =
  log "Hi!" ;
  ignore (Tutils.create (p "1") () "boule") ;
  ignore (Tutils.create (p "2") () "bill") ;
  Mutex.lock lock ;
  Unix.sleep 1 ;
  stop := true ;
  log "Waiting..." ;
  Tutils.join_all_no_timeout () ;
  log "Finished"

let _ =
  Init.init main
