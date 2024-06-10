let cache_filename ~toplevel term =
  match Cache.dir () with
    | None -> None
    | Some dir ->
        Utils.recmkdir dir;
        let hash = Parsed_term.hash term in
        let fname =
          Printf.sprintf "%s%s.liq-cache" hash
            (if toplevel then "-toplevel" else "")
        in
        Some (Filename.concat dir fname)

let retrieve ~toplevel parsed_term : Term.t option =
  Startup.time "Cache retrieval" (fun () ->
      try
        match cache_filename ~toplevel parsed_term with
          | None -> None
          | Some filename ->
              if Sys.file_exists filename then (
                let ic = open_in_bin filename in
                Fun.protect
                  ~finally:(fun () -> close_in ic)
                  (fun () ->
                    let term = Marshal.from_channel ic in
                    Startup.message "Loading script from cache!";
                    Some term))
              else None
      with
        | Failure msg
          when String.starts_with ~prefix:"input_value: unknown code module" msg
          ->
            Startup.message "Liquidsoap binary changed: cache invalidated!";
            None
        | exn ->
            let bt = Printexc.get_backtrace () in
            let exn = Printexc.to_string exn in
            if Sys.getenv_opt "LIQ_DEBUG_CACHE" <> None then
              Startup.message "Error while loading cache: %s\n%s" exn bt
            else Startup.message "Error while loading cache: %s" exn;
            None)

let cache ~toplevel ~parsed_term term =
  try
    match cache_filename ~toplevel parsed_term with
      | None -> ()
      | Some filename ->
          let oc = open_out filename in
          Fun.protect
            ~finally:(fun () -> close_out oc)
            (fun () ->
              let term = Marshal.to_channel oc term [Marshal.Closures] in
              let fn = !Hooks.cache_maintenance in
              fn ();
              term)
  with exn ->
    Startup.message "Error while saving cache: %s" (Printexc.to_string exn)
