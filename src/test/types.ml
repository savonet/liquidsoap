open Type

let should_work t t' r =
  let t = make t in
  let t' = make t' in
  let r = make r in
  Printf.printf "Finding min for %s and %s\n%!" (print t) (print t');
  let m = Typing.sup ~pos:None t t' in
  Printf.printf "Got: %s, expect %s\n%!" (print m) (print r);
  Typing.(m <: r);
  Typing.(t <: m);
  Typing.(t' <: m)

let should_fail t t' =
  try
    ignore (Typing.sup ~pos:None (make t) (make t'));
    assert false
  with _ -> ()

let () =
  should_work (var ()).descr (Ground Bool) (Ground Bool);
  should_work (Ground Bool) (var ()).descr (Ground Bool);

  should_fail (Ground Bool) (Ground Int);
  should_fail (List (make (Ground Bool))) (List (make (Ground Int)));

  let m = Meth ("aa", ([], make (Ground Int)), "", make (Ground Bool)) in

  should_work m (Ground Bool) (Ground Bool);

  let n = Meth ("b", ([], make (Ground Bool)), "", make m) in

  should_work m n m;

  let n = Meth ("aa", ([], make (Ground Int)), "", make (Ground Int)) in

  should_fail m n;

  let n = Meth ("aa", ([], make (Ground Bool)), "", make (Ground Bool)) in

  should_fail m n;

  ()
