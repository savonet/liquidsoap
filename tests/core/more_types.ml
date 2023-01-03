open Liquidsoap_lang

let () =
  (* def f(x) =
       if x?.foo ?? false then
          x.{foo = false}
       else
          x
       end
     end *)
  let _if = Term.make (Term.Var "if") in
  let x = Term.make (Term.Var "x") in
  let _false () = Term.make (Term.Ground (Term.Ground.Bool false)) in
  let cond =
    Term.make
      (Term.Invoke
         { Term.invoked = x; default = Some (_false ()); meth = "foo" })
  in
  let _then =
    Term.make
      (Term.Fun
         ( Term.Vars.empty,
           [],
           Term.make (Meth ({ Term.name = "foo"; meth_value = _false () }, x))
         ))
  in
  let _else = Term.make (Term.Fun (Term.Vars.empty, [], x)) in
  let f =
    Term.make
      (Term.Fun
         ( Term.Vars.empty,
           [("x", "x", Type.var (), None)],
           Term.make
             (Term.App (_if, [("", cond); ("then", _then); ("else", _else)])) ))
  in
  Typechecking.check ~throw:(fun exn -> raise exn) f;
  match (Type.deref f.Term.t).Type.descr with
    | Type.Arrow ([(false, "x", _)], t) -> (
        let meths, _ = Type.split_meths t in
        match meths with
          | [{ Type.meth = "foo"; optional = true; _ }] -> ()
          | _ -> assert false)
    | _ -> assert false
