type req = { id : int; destroyed : bool }

module Pool = Pool.Make (struct
  type t = req

  let id { id; _ } = id
  let destroyed id = { id; destroyed = true }
  let is_destroyed { destroyed; _ } = destroyed
end)

let count () = Pool.fold (fun _ _ n -> n + 1) 0

let[@inline never] fill () =
  let l =
    List.init 100 (fun _ -> Pool.add (fun id -> { id; destroyed = false }))
  in
  assert (count () = 100);
  assert (List.map (fun { id; _ } -> id) l = List.init 100 Fun.id);
  assert (Pool.find 15 = Some (List.nth l 15));
  let destroyed = Pool.add (fun id -> { id; destroyed = true }) in
  assert (Pool.find destroyed.id = None);
  assert (count () = 100);
  ignore (Sys.opaque_identity (l, destroyed))

let () =
  fill ();
  Gc.full_major ();
  assert (count () = 0);
  assert (Pool.find 15 = None)
