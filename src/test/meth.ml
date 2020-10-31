module T = Lang_types

(* The the code found in add_builtin. *)
let () =
  (* x.l1.l2.l3 = v means
     x = (x where l1 = (x.l1 where l2 = (x.l1.l2 where l3 = v)))
  *)
  let x = "x" in
  let v = "v" in
  let rec aux prefix = function
    | l :: ll ->
        Printf.sprintf "%s%s%s where (%s = %s)" x
          (if prefix = [] then "" else ".")
          (String.concat "." (List.rev prefix))
          l
          (aux (l :: prefix) ll)
    | [] -> v
  in
  let ll = ["l1"; "l2"; "l3"] in
  Printf.printf "%s = %s\n\n%!" x (aux [] ll)

(* Test adding submethods. *)
let () =
  let print t = Printf.printf "%s\n%!" (T.print t) in
  let t = T.make (T.Ground T.Int) in
  print t;
  let t = T.meth "f" ([], T.make (T.Ground T.Float)) t in
  print t;
  let t = T.meth "ff" ([], T.make (T.Ground T.Float)) t in
  print t;
  let t = T.meths ["f"; "s"] ([], T.make (T.Ground T.String)) t in
  print t;
  let t = T.meths ["f"; "s"; "f'"] ([], T.make (T.Ground T.Float)) t in
  print t

(* Test subtyping. *)
let () =
  (* Make sure unifying variables sees top-level methods:
     We do: t = ('a).{ f : int } <: t' = int.{ ff : int, f : float }
     and make sure that this fails. *)
  let t = T.fresh_evar ~level:(-1) ~pos:None in
  let t = T.meth "f" ([], T.make (T.Ground T.Int)) t in
  let t' = T.make (T.Ground T.Int) in
  let t' = T.meth "f" ([], T.make (T.Ground T.Float)) t' in
  let t' = T.meth "ff" ([], T.make (T.Ground T.Int)) t' in
  assert (
    T.(
      try
        t <: t';
        false
      with _ -> true) )
