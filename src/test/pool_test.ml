type req = int

module Pool = Pool.Make (struct
  type t = req
end)

let () =
  (* Create a bunch of requests. *)
  ignore (List.init 100 (fun _ -> Pool.add (fun i -> i)));
  (* Delete 15th one. *)
  Pool.kill 15 0.1;
  Unix.sleepf 1.;
  assert (Pool.size () = 99);
  let id = Pool.add (fun i -> i) in
  assert (id = 15)
