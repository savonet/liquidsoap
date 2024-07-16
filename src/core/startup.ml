include Liquidsoap_lang.Startup

let log = Log.make ["startup"]

let () =
  Lifecycle.before_start ~name:"core startup" (fun () ->
      List.iter (log#important "%s") (messages ()))
