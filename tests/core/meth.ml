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
  let print t = Printf.printf "%s\n%!" (Type.to_string t) in
  let t = Type.make Type.Int in
  print t;
  let t = Type.meth "f" ([], Type.make Type.Float) t in
  print t;
  let t = Type.meth "ff" ([], Type.make Type.Float) t in
  print t;
  let t = Type.meths ["f"; "s"] ([], Type.make Type.String) t in
  print t;
  let t = Type.meths ["f"; "s"; "f'"] ([], Type.make Type.Float) t in
  print t

(* Test subtyping. *)
let () =
  (* Make sure unifying variables sees top-level methods:
     We do: t = ('a).{ f : int } <: t' = int.{ ff : int, f : string }
     and make sure that this fails. *)
  let t = Type.var () in
  let t = Type.meth "f" ([], Type.make Type.Int) t in
  let t' = Type.make Type.Int in
  let t' = Type.meth "f" ([], Type.make Type.String) t' in
  let t' = Type.meth "ff" ([], Type.make Type.Int) t' in
  assert (
    try
      Typing.(t <: t');
      false
    with _ -> true)
