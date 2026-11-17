(* Scripts catch liquidsoap errors, registered exceptions and standard OCaml
   exceptions under a kind, and a [catch] filtering on another kind lets them
   through with that kind. Any other exception is never caught. *)

exception Registered
exception Unregistered

let registered_pos =
  Pos.of_lexing_pos
    ( {
        Lexing.pos_fname = "registered.ml";
        pos_lnum = 1;
        pos_bol = 0;
        pos_cnum = 0;
      },
      {
        Lexing.pos_fname = "registered.ml";
        pos_lnum = 1;
        pos_bol = 0;
        pos_cnum = 1;
      } )

let () =
  Runtime_error.register_catchable (function
    | Registered -> Some ("registered", [registered_pos])
    | _ -> None)

let catchable =
  [|
    ("not_found", Not_found);
    ("failure", Failure "failure");
    ("invalid", Invalid_argument "invalid");
    ("system", Sys_error "system");
    ("system", Unix.Unix_error (Unix.ENOENT, "open", "file"));
    ("eval", Division_by_zero);
    ("registered", Registered);
    ("custom", Runtime_error.Runtime_error (Runtime_error.make ~pos:[] "custom"));
  |]

let uncatchable = [| Unregistered; Out_of_memory; Assert_failure ("x", 1, 1) |]

let () =
  ignore
    (Lang.add_builtin ~category:`Programming
       ~descr:"Raise the given test exception." "test_raise"
       [("", Lang.int_t, None, None)]
       Lang.unit_t
       (fun p ->
         match List.assoc "" p |> Lang.to_int with
           | n when n < 0 -> raise uncatchable.(-n - 1)
           | n -> raise (snd catchable.(n))))

let eval script =
  Lang_eval.eval ~cache:false ~typecheck:true ~stdlib:`Disabled script

let () =
  Array.iteri
    (fun n (kind, _) ->
      let check name script =
        let caught = Lang.to_string (eval script) in
        if caught <> kind then
          failwith
            (Printf.sprintf "%s %d: caught as %S, expected %S" name n caught
               kind)
      in
      check "matching catch"
        (Printf.sprintf
           {|try test_raise(%d) "none" catch e : [error.register(%S)] do error.methods(e).kind end|}
           n kind);
      check "other catch"
        (Printf.sprintf
           {|try try test_raise(%d) "none" catch _ : [error.register("other")] do "other" end catch e do error.methods(e).kind end|}
           n))
    catchable;
  let position =
    Lang.to_string
      (eval
         {|try test_raise(6) "" catch e do
             let [p, ..._] = error.methods(e).trace
             p.filename
           end|})
  in
  if position <> "registered.ml" then
    failwith (Printf.sprintf "registered position: %S" position);
  Array.iteri
    (fun n exn ->
      match
        eval (Printf.sprintf {|try test_raise(%d) catch _ do () end|} (-n - 1))
      with
        | _ -> failwith (Printexc.to_string exn ^ " was caught")
        (* Reported, then raised as an already reported error. *)
        | exception Runtime.Error -> ())
    uncatchable
