include Mutex

let mutexify m f x =
  lock m;
  match f x with
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        unlock m;
        Printexc.raise_with_backtrace exn bt
    | v ->
        unlock m;
        v
  [@@inline always]
