(* Two threads reading [Clock.self_sync_type_of_sources] at once must both get
   the value. *)

let entered = Atomic.make 0
let overlapped = Atomic.make false

(* Rendezvous inside the computation so both threads are provably in it at the
   same time. *)
let slow_source =
  object
    method self_sync : Clock.self_sync =
      Atomic.incr entered;
      let rec wait n =
        if Atomic.get entered < 2 && 0 < n then (
          Thread.delay 0.01;
          wait (n - 1))
      in
      wait 200;
      if 2 <= Atomic.get entered then Atomic.set overlapped true;
      (`Static, None)
  end

let self_sync_type = Clock.self_sync_type_of_sources [slow_source]
let results = Array.make 2 None
let errors = Array.make 2 None

let compute pos () =
  try results.(pos) <- Some (self_sync_type ())
  with exn -> errors.(pos) <- Some exn

let () =
  let threads = Array.init 2 (fun pos -> Thread.create (compute pos) ()) in
  Array.iter Thread.join threads;
  Array.iteri
    (fun pos exn ->
      match exn with
        | Some exn ->
            failwith
              (Printf.sprintf "thread %d raised %s" pos (Printexc.to_string exn))
        | None -> ())
    errors;
  assert (Atomic.get overlapped);
  Array.iter (fun result -> assert (result = Some `Static)) results;
  assert (self_sync_type () = `Static);
  assert (Atomic.get entered = 2)
