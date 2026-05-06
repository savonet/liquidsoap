let keep =
  let kept = ref [] in
  fun v ->
    kept := Obj.repr v :: !kept;
    v

let () =
  (* iter: empty array visits nothing *)
  let w : < id : int > Weak.t = Weak.create 5 in
  let count = ref 0 in
  Weak_utils.iter w (fun _ -> incr count);
  assert (!count = 0);

  (* iter: visits only set slots, in index order *)
  let w = Weak.create 5 in
  let objs =
    List.map
      (fun i ->
        keep
          (object
             method id = i
          end))
      [0; 2; 4]
  in
  List.iter (fun o -> Weak.set w o#id (Some o)) objs;
  let seen = ref [] in
  Weak_utils.iter w (fun o -> seen := o#id :: !seen);
  assert (List.rev !seen = [0; 2; 4]);

  (* fold_left: count live elements *)
  let n = Weak_utils.fold_left (fun acc _ -> acc + 1) 0 w in
  assert (n = 3);

  (* fold_left: collect ids in order *)
  let ids = Weak_utils.fold_left (fun acc o -> acc @ [o#id]) [] w in
  assert (ids = [0; 2; 4]);

  (* GC: slot with no other live reference becomes empty after collection *)
  let w2 = Weak.create 1 in
  Weak.set w2 0
    (Some
       (object
          method id = 99
       end));
  Gc.full_major ();
  let count = ref 0 in
  Weak_utils.iter w2 (fun _ -> incr count);
  assert (!count = 0);

  Printf.printf "All Weak_utils tests passed!\n%!"
