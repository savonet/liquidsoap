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
let get r = mutexify r.m (fun [@inline] () -> r.v) ()
let set r v = mutexify r.m (fun [@inline] v -> r.v <- v) v

let exchange r v =
  mutexify r.m
    (fun [@inline] v ->
      let cur = r.v in
      r.v <- v;
      cur)
    v

let compare_and_set r seen v =
  mutexify r.m
    (fun [@inline] v ->
      let cur = r.v in
      if cur == seen then (
        r.v <- v;
        true)
      else false)
    v

let fetch_and_add r n =
  mutexify r.m
    (fun [@inline] n ->
      let cur = r.v in
      r.v <- cur + n;
      cur)
    n

let incr r = ignore ((fetch_and_add [@inlined]) r 1)
let decr r = ignore ((fetch_and_add [@inlined]) r (-1))
