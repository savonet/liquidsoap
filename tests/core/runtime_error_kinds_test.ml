(* Scripts catch OCaml exceptions raised by builtins under a kind, and a
   [catch] filtering on another kind lets them through with that kind. *)

exception Custom

let exceptions =
  [|
    ("not_found", Not_found);
    ("failure", Failure "failure");
    ("invalid", Invalid_argument "invalid");
    ("system", Sys_error "system");
    ("system", Unix.Unix_error (Unix.ENOENT, "open", "file"));
    ("eval", Division_by_zero);
    ("eval", Custom);
    ("custom", Runtime_error.Runtime_error (Runtime_error.make ~pos:[] "custom"));
  |]

let () =
  ignore
    (Lang.add_builtin ~category:`Programming
       ~descr:"Raise the given test exception." "test_raise"
       [("", Lang.int_t, None, None)]
       Lang.unit_t
       (fun p ->
         match List.assoc "" p |> Lang.to_int with
           | -1 -> raise Out_of_memory
           | n -> raise (snd exceptions.(n))))

let eval script =
  Lang_eval.eval ~cache:false ~typecheck:true ~stdlib:`Disabled script

let caught_kind script = Lang.to_string (eval script)

let () =
  Array.iteri
    (fun n (kind, _) ->
      let check name script =
        let caught = caught_kind script in
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
    exceptions;
  match eval {|try test_raise(-1) catch _ do () end|} with
    | _ -> failwith "Out_of_memory was caught"
    (* Reported, then raised as an already reported error. *)
    | exception Runtime.Error -> ()
