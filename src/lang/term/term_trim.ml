open Runtime_term

let unit_t = Type.make Type.unit

let rec trim_type t =
  let open Type in
  match t with
    | { descr = Arrow (args, ret_t) } as t ->
        {
          t with
          descr =
            Arrow
              ( List.map (fun (b, s, p) -> (b, s, trim_type p)) args,
                trim_type ret_t );
        }
    | { descr = Getter g } as t -> { t with descr = Getter (trim_type g) }
    | { descr = Nullable n } as t -> { t with descr = Nullable (trim_type n) }
    | { descr = Meth (_, t) } -> trim_type t
    | { descr = List repr } as t ->
        { t with descr = List { repr with t = trim_type repr.t } }
    | { descr = Tuple l } as t ->
        { t with descr = Tuple (List.map trim_type l) }
    | { descr = Var { contents = Link (_, t) } } -> trim_type t
    | { descr = Var { contents = Free _ } } as t -> t
    | ( { descr = Constr _ }
      | { descr = Custom _ }
      | { descr = String }
      | { descr = Int }
      | { descr = Float }
      | { descr = Bool }
      | { descr = Never } ) as t ->
        t

let trim_type t = { (trim_type t) with pos = t.pos }

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
  | `Seq (t, t') ->
      trim_term t;
      trim_term t'
  | `Let { def; body } ->
      trim_term def;
      trim_term body
  | `List l -> List.iter trim_term l
  | `Cast c ->
      trim_term c.cast;
      c.typ := unit_t
  | `App (t, l) ->
      trim_term t;
      List.iter (fun (_, t) -> trim_term t) l
  | `Invoke { invoked; invoke_default } -> (
      trim_term invoked;
      Methods.iter (fun _ m -> trim_term m) invoked.methods;
      match invoke_default with None -> () | Some t -> trim_term t)
  | `Encoder enc -> trim_encoder enc
  | `Fun { body; arguments } ->
      trim_term body;
      List.iter
        (function { default = Some t } -> trim_term t | _ -> ())
        arguments

and trim_term ({ term; methods } as tm) =
  tm.t <- trim_type tm.t;
  trim_ast term;
  Term.Methods.iter (fun _ t -> trim_term t) methods
