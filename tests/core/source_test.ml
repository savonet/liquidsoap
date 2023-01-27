open Source.Clock_variables

(* Reference: https://github.com/savonet/liquidsoap/pull/1272 *)

let () =
  let c1 = create_unknown ~sources:[] ~sub_clocks:[] () in
  let c2 = create_unknown ~sources:[] ~sub_clocks:[c1] () in
  let c3 = create_unknown ~sources:[] ~sub_clocks:[c1] () in
  (* Make sure unification of a variable with itself
     works as expected. *)
  unify c2 c2;
  assert (subclocks c2 = [c1]);
  (* Make sure subclocks don't get duplicated during inification. *)
  unify c2 c3;
  assert (subclocks c2 = [c1]);
  assert (subclocks c3 = [c1])
