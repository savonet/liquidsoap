module Queue = Liquidsoap_lang.Queues.Queue
module Future = Moonpool.Fut
module Pool = Moonpool.Ws_pool
module Evaluation = Liquidsoap_lang.Evaluation

type 'a t = {
  future : 'a Future.t;
  promise : unit Future.promise;
  after_eval : (unit -> unit) Queue.t;
}

type after_eval = (unit -> unit) Queue.t

let pool = Pool.create ()
let default_after_eval = function Some q -> q | None -> Queue.create ()

let make ?after_eval fn =
  let wait, promise = Future.make () in
  let after_eval = default_after_eval after_eval in
  let fn () =
    Evaluation.after_eval ~mode:(`Collect after_eval) (fun () ->
        Future.await wait;
        fn ())
  in
  let future = Future.spawn ~on:pool fn in
  { future; promise; after_eval }

let make_list ?after_eval l =
  let wait, promise = Future.make () in
  let after_eval = default_after_eval after_eval in
  let futures =
    List.map
      (fun fn ->
        Future.spawn ~on:pool (fun () ->
            Evaluation.after_eval ~mode:(`Collect after_eval) (fun () ->
                Future.await wait;
                fn ())))
      l
  in
  let future = Future.wait_list futures in
  { future; promise; after_eval }

let make_promise () =
  let future, promise = Future.make () in
  ( (fun v -> Future.fulfill_idempotent promise (Result.Ok v)),
    make (fun () -> Future.await future) )

let compute { promise } = Future.fulfill_idempotent promise (Result.Ok ())

let await ?(run_after_eval = true) { future; promise; after_eval } =
  Future.fulfill_idempotent promise (Result.Ok ());
  let v = Future.await future in
  if run_after_eval then Queue.iter after_eval (fun fn -> fn ());
  v

let eval fn = Future.wait_block_exn (Future.spawn ~on:pool fn)
