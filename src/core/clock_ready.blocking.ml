type 'a t = unit -> 'a

let clock_pool = Moonpool.Ws_pool.create ()
let make fn = fn
let process fn = fn ()
