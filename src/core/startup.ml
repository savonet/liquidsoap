include Liquidsoap_lang.Startup

let log = Log.make ["startup"]

let () =
  Lifecycle.before_start (fun () ->
      List.iter (log#important "%s") (messages ()))
