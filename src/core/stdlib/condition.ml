type t = unit Atomic.Awaitable.t

let create () = Atomic.Awaitable.make ()
let signal c = Atomic.Awaitable.set c ()

let wait c m =
  Mutex.unlock m;
  Future.await (Atomic.Awaitable.get c);
  Mutex.lock m
