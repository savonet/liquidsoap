type 'a state =
  | Fun of (unit -> 'a)
  | Run of (unit -> unit) list
  | Val of 'a
  | Exn of (exn * Printexc.raw_backtrace)

type 'a t = 'a state Atomic.t

let is_val v = match Atomic.get v with Val _ -> true | _ -> false
let from_fun th = Atomic.make (Fun th)
let from_val v = Atomic.make (Val v)

let rec force t =
  match Atomic.get t with
    | Val v -> v
    | Exn (e, bt) -> Printexc.raise_with_backtrace e bt
    | Fun fn as before ->
        if Atomic.compare_and_set t before (Run []) then (
          let result =
            match fn () with
              | v -> Val v
              | exception e ->
                  let bt = Printexc.get_raw_backtrace () in
                  Exn (e, bt)
          in
          match Atomic.exchange t result with
            | Val _ | Exn _ | Fun _ -> failwith "impossible"
            | Run waiters ->
                List.iter (( |> ) ()) waiters;
                force t)
        else force t
    | Run waiters as before ->
        let promise, future = Future.make_promise () in
        let after = Run (promise :: waiters) in
        if Atomic.compare_and_set t before after then (
          Future.await future;
          force t)
        else force t
