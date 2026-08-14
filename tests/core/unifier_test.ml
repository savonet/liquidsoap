let () =
  let x = Unifier.make 1 in
  let y = Unifier.make 2 in
  let z = Unifier.make 3 in
  Unifier.(x <-- y);
  assert (Unifier.deref x = 2);
  Unifier.set x 5;
  assert (Unifier.deref y = 5);
  Unifier.set y 4;
  assert (Unifier.deref x = 4);
  Unifier.(x <-- z);
  assert (Unifier.deref x = 3);
  assert (Unifier.deref y = 3);
  Unifier.set x 2;
  assert (Unifier.deref y = 2);
  assert (Unifier.deref z = 2);
  Unifier.set y 4;
  assert (Unifier.deref x = 4);
  assert (Unifier.deref z = 4);
  Unifier.set z 1;
  assert (Unifier.deref x = 1);
  assert (Unifier.deref y = 1);
  Unifier.(y <-- x);
  Unifier.set y 4;
  assert (Unifier.deref x = 4);
  assert (Unifier.deref z = 4);
  Unifier.set x 2;
  assert (Unifier.deref y = 2);
  assert (Unifier.deref z = 2)

(* Unifying through the most recently created node each time means nothing
   ever dereferences from the head, so its path is never compressed. *)
let () =
  let n = 100_000 in
  Gc.full_major ();
  let before = (Gc.stat ()).Gc.live_words in
  let head = Unifier.make 0 in
  let cur = ref head in
  for i = 1 to n do
    let fresh = Unifier.make i in
    Unifier.(!cur <-- fresh);
    cur := fresh
  done;
  Gc.full_major ();
  let retained = (Gc.stat ()).Gc.live_words - before in
  assert (Unifier.deref head = n);
  (* Retention must not scale with the number of unifications. *)
  assert (retained < n)
