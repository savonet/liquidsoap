type 'a t = { m : Mutex.t; mutable v : 'a }

let mutexify m f x =
  Mutex.lock m;
  match f x with
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        Mutex.unlock m;
        Printexc.raise_with_backtrace exn bt
    | v ->
        Mutex.unlock m;
        v
[@@inline always]

let make v = { m = Mutex.create (); v }
let get r = mutexify r.m (fun () -> r.v) ()
let set r = mutexify r.m (fun v -> r.v <- v)

let[@inline never] exchange r =
  mutexify r.m (fun v ->
      let cur = r.v in
      r.v <- v;
      cur)

let[@inline never] compare_and_set r seen =
  mutexify r.m (fun v ->
      let cur = r.v in
      if cur == seen then (
        r.v <- v;
        true)
      else false)

let[@inline never] fetch_and_add r =
  mutexify r.m (fun n ->
      let cur = r.v in
      r.v <- cur + n;
      cur)

let incr r = ignore (fetch_and_add r 1)
let decr r = ignore (fetch_and_add r (-1))
