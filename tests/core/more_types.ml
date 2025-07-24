open Liquidsoap_lang

let () =
  (* def f(x) =
       if x?.foo ?? false then
          x.{foo = false}
       else
          x
       end
     end *)
  let _if = Term.make (`Var "if") in
  let x = Term.make (`Var "x") in
  let _false () = Term.make (`Bool false) in
  let cond =
    Term.make
      (`Invoke
         { Term.invoked = x; invoke_default = Some (_false ()); meth = "foo" })
  in
  let _then =
    Term.make
      (`Fun
         {
           Term.name = None;
           arguments = [];
           body =
             Term.make ~t:x.Term.t
               ~methods:(Term.Methods.add "foo" (_false ()) Term.Methods.empty)
               x.Term.term;
           free_vars = None;
         })
  in
  let _else =
    Term.make
      (`Fun { Term.name = None; arguments = []; body = x; free_vars = None })
  in
  let f =
    Term.make
      (`Fun
         {
           Term.free_vars = None;
           name = None;
           arguments =
             [
               {
                 Term.label = "x";
                 as_variable = None;
                 typ = Type.var ();
                 default = None;
                 pos = None;
               };
             ];
           body =
             Term.make
               (`App (_if, [("", cond); ("then", _then); ("else", _else)]));
         })
  in
  Typechecking.check
    ~throw:(fun ~bt exn -> Printexc.raise_with_backtrace exn bt)
    ~check_top_level_override:false f;
  match (Type.deref f.Term.t).Type.descr with
    | Type.Arrow ([(false, "x", _)], t) -> (
        let meths, _ = Type.split_meths t in
        match meths with
          | [{ Type.meth = "foo"; optional = true; _ }] -> ()
          | _ -> assert false)
    | _ -> assert false

exception Failed

let () =
  (* term = (1 : int.{opt?: string}).foo *)
  let typ = Type.meth ~optional:true "opt" ([], Lang.string_t) Lang.int_t in
  let term =
    {
      Term.t = typ;
      term = `Int 1;
      methods = Term.Methods.empty;
      flags = Flags.empty;
    }
  in
  let invoke =
    {
      Term.t = Lang.univ_t ();
      term =
        `Invoke { Term.invoked = term; invoke_default = None; meth = "opt" };
      methods = Term.Methods.empty;
      flags = Flags.empty;
    }
  in
  try
    Typechecking.check
      ~throw:(fun ~bt exn -> Printexc.raise_with_backtrace exn bt)
      ~check_top_level_override:false invoke;
    raise Failed
  with
    | Failed -> raise Failed
    | _ -> ()
