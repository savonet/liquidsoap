open Runtime_term

let unit_t = Type.make Type.unit

let rec trim_encoder_params params =
  List.iter
    (function
      | `Anonymous _ -> ()
      | `Encoder enc -> trim_encoder enc
      | `Labelled (_, t) -> trim_term t)
    params

and trim_encoder (_, params) = trim_encoder_params params

and trim_ast = function
  | `Custom _ -> ()
  | `Tuple l -> List.iter trim_term l
  | `Null | `Int _ | `Float _ | `Bool _ | `String _ -> ()
  | `Open (t, t') ->
      trim_term t;
      trim_term t'
  | `Var _ -> ()
  | `Value _ -> ()
  | `Seq (t, t') ->
      t.methods <- Methods.empty;
      trim_term t;
      trim_term t'
  | `Let { def; body } ->
      trim_term def;
      trim_term body
  | `List l -> List.iter trim_term l
  | `Cast c ->
      trim_term c.casted;
      c.typ <- unit_t
  | `App (t, l) ->
      t.methods <- Methods.empty;
      trim_term t;
      List.iter (fun (_, t) -> trim_term t) l
  | `Hide (tm, l) ->
      tm.methods <- Methods.filter (fun m _ -> not (List.mem m l)) tm.methods;
      trim_term tm
  | `Invoke { invoked; invoke_default; meth } -> (
      trim_term invoked;
      invoked.methods <- Methods.filter (fun n _ -> n = meth) invoked.methods;
      Methods.iter (fun _ tm -> trim_term tm) invoked.methods;
      match invoke_default with None -> () | Some t -> trim_term t)
  | `Encoder enc -> trim_encoder enc
  | `Fun { body; arguments } ->
      trim_term body;
      List.iter
        (function { default = Some t } -> trim_term t | _ -> ())
        arguments

and trim_term ({ term; methods } as tm) =
  let fn = !Hooks.trim_type in
  tm.t <- fn tm.t;
  trim_ast term;
  Term.Methods.iter (fun _ t -> trim_term t) methods
