type 'a t = 'a Lazy.t * Mutex.t

let[@inline] from_lazy v : 'a t = (v, Mutex.create ())
let[@inline] from_val v : 'a t = from_lazy (Lazy.from_val v)
let[@inline] from_fun f : 'a t = from_lazy (Lazy.from_fun f)

let force ((v, m) : 'a t) =
  Mutex.lock m;
  Fun.protect ~finally:(fun () -> Mutex.unlock m) @@ fun () -> Lazy.force v

let to_fun v () = force v

let map f ((v, m) : 'a t) =
  Mutex.lock m;
  Fun.protect ~finally:(fun () -> Mutex.unlock m) @@ fun () -> (Lazy.map f v, m)

let is_val ((v, m) : 'a t) =
  Mutex.lock m;
  Fun.protect ~finally:(fun () -> Mutex.unlock m) @@ fun () -> Lazy.is_val v
