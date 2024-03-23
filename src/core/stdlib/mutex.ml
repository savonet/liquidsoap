type state = Unlocked | Locked of (unit -> unit) list
type t = state Atomic.t

let create () = Atomic.make Unlocked

let unlock t =
  match Atomic.exchange t Unlocked with
    | Unlocked -> invalid_arg "mutex: already unlocked"
    | Locked awaiters -> List.iter (( |> ) ()) awaiters

let try_lock t =
  match Atomic.get t with
    | Unlocked -> Atomic.compare_and_set t Unlocked (Locked [])
    | _ -> false

let rec lock t =
  match Atomic.get t with
    | Unlocked ->
        if not (Atomic.compare_and_set t Unlocked (Locked [])) then lock t
    | Locked awaiters as before ->
        let promise, future = Future.make_promise () in
        let after = Locked (promise :: awaiters) in
        if Atomic.compare_and_set t before after then Future.await future
        else lock t

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
