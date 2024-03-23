include Atomic

module Awaitable = struct
  type 'a pending = ('a -> unit) list
  type 'a t = ('a * 'a pending) Atomic.t

  let make v = Atomic.make (v, [])

  let get x =
    let release, future = Future.make_promise () in
    let rec f () =
      let ((v, l) as cur) = Atomic.get x in
      match Atomic.compare_and_set x cur (v, release :: l) with
        | true -> future
        | false -> f ()
    in
    f ()

  let set x v =
    let _, l = Atomic.exchange x (v, []) in
    List.iter (fun fn -> fn v) l
end
