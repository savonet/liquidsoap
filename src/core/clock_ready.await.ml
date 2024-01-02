module Future = Moonpool.Fut
module Pool = Moonpool.Ws_pool

type 'a t = 'a Future.t

let clock_pool = Pool.create ()
let make fn = Future.spawn ~on:clock_pool fn
let process fut = Future.await fut
