(* Keep strong references to prevent GC from collecting weak references *)
let kept = ref []

let keep v =
  kept := Obj.repr v :: !kept;
  v

let () =
  (* Test create and basic operations *)
  let q = Queues.WeakQueue.create () in
  assert (Queues.WeakQueue.length q = 0);
  assert (Queues.WeakQueue.elements q = []);

  (* Test push and elements *)
  Queues.WeakQueue.push q (keep "a");
  Queues.WeakQueue.push q (keep "b");
  Queues.WeakQueue.push q (keep "c");
  assert (Queues.WeakQueue.length q = 3);
  assert (Queues.WeakQueue.elements q = ["a"; "b"; "c"]);

  (* Test exists *)
  assert (Queues.WeakQueue.exists q (fun x -> x = "b"));
  assert (not (Queues.WeakQueue.exists q (fun x -> x = "d")));

  (* Test iter *)
  let acc = ref [] in
  Queues.WeakQueue.iter q (fun x -> acc := x :: !acc);
  assert (!acc = ["c"; "b"; "a"]);

  (* Test fold *)
  let result = Queues.WeakQueue.fold q (fun x acc -> x :: acc) [] in
  assert (result = ["c"; "b"; "a"]);

  (* Test filter *)
  let q2 = Queues.WeakQueue.create () in
  Queues.WeakQueue.push q2 (keep "1");
  Queues.WeakQueue.push q2 (keep "2");
  Queues.WeakQueue.push q2 (keep "3");
  Queues.WeakQueue.push q2 (keep "4");
  Queues.WeakQueue.filter q2 (fun x -> x = "2" || x = "4");
  assert (Queues.WeakQueue.elements q2 = ["2"; "4"]);

  (* Test filter_out *)
  let q3 = Queues.WeakQueue.create () in
  Queues.WeakQueue.push q3 (keep "10");
  Queues.WeakQueue.push q3 (keep "20");
  Queues.WeakQueue.push q3 (keep "30");
  Queues.WeakQueue.push q3 (keep "40");
  Queues.WeakQueue.filter_out q3 (fun x -> x = "20" || x = "40");
  assert (Queues.WeakQueue.elements q3 = ["10"; "30"]);

  (* Test flush_elements *)
  let q4 = Queues.WeakQueue.create () in
  Queues.WeakQueue.push q4 (keep "x");
  Queues.WeakQueue.push q4 (keep "y");
  let flushed = Queues.WeakQueue.flush_elements q4 in
  assert (flushed = ["x"; "y"]);
  assert (Queues.WeakQueue.length q4 = 0);
  assert (Queues.WeakQueue.elements q4 = []);

  (* Test flush_iter *)
  let q5 = Queues.WeakQueue.create () in
  Queues.WeakQueue.push q5 (keep "100");
  Queues.WeakQueue.push q5 (keep "200");
  let acc = ref [] in
  Queues.WeakQueue.flush_iter q5 (fun x -> acc := x :: !acc);
  assert (!acc = ["200"; "100"]);
  assert (Queues.WeakQueue.length q5 = 0);

  Printf.printf "All WeakQueue tests passed!\n%!"
