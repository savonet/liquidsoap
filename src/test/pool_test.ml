type req = { id : int; destroyed : bool }

module Pool = Pool.Make (struct
  type t = req

  let id { id } = id
  let destroyed id = { id; destroyed = true }
  let is_destroyed { destroyed } = destroyed
end)

let () =
  (* Create a bunch of requests. *)
  let l =
    List.init 100 (fun _ -> Pool.add (fun id -> { id; destroyed = false }))
  in
  (* Delete 15th one. *)
  Pool.remove 15;
  Gc.full_major ();
  assert (Pool.size () = 99);
  let r = Pool.add (fun id -> { id; destroyed = false }) in
  assert (r.id = 100);
  Gc.full_major ();
  assert (List.length l = 100);
  assert (Pool.size () = 99);
  Gc.full_major ();
  assert (Pool.size () = 0)
