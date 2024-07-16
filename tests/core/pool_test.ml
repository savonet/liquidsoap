type req = { id : int; destroyed : bool }

module Pool = Pool.Make (struct
  type t = req

  let id { id; _ } = id
  let destroyed id = { id; destroyed = true }
  let is_destroyed { destroyed; _ } = destroyed
end)

let m = Mutex.create ()
let _done = Condition.create ()
let started = Condition.create ()

let fill () =
  (* Create a bunch of requests. *)
  let l =
    List.init 100 (fun _ -> Pool.add (fun id -> { id; destroyed = false }))
  in
  List.iter (fun { id; _ } -> ignore (Printf.sprintf "id: %d\n%!" id)) l;
  (* Delete 15th one. *)
  Pool.remove 15;
  Gc.full_major ();
  List.iter (fun { id; _ } -> ignore (Printf.sprintf "id: %d\n%!" id)) l;
  assert (Pool.size () = 99);
  let r = Pool.add (fun id -> { id; destroyed = false }) in
  assert (Pool.size () = 100);
  assert (r.id = 100);
  assert (List.length l = 100);
  Condition.signal _done

let check () =
  Mutex.lock m;
  Condition.signal started;
  Condition.wait _done m;
  Gc.full_major ();
  assert (Pool.size () = 0);
  Mutex.unlock m

let () =
  Mutex.lock m;
  let th = Thread.create check () in
  Condition.wait started m;
  Mutex.unlock m;
  ignore (Thread.create fill ());
  Thread.join th
