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
