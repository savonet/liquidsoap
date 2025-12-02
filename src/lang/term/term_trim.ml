open Runtime_term

let rec trim_type t =
  let open Type in
  match t with
    | { descr = Arrow { args; t = ret_t } } as t ->
        {
          t with
          descr =
            Arrow
              {
                args = List.map (fun (b, s, p) -> (b, s, trim_type p)) args;
                t = trim_type ret_t;
              };
        }
    | { descr = Getter g } as t -> { t with descr = Getter (trim_type g) }
    | { descr = Nullable n } as t -> { t with descr = Nullable (trim_type n) }
    | { descr = Meth { t } } -> trim_type t
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
  List.map
    (function
      | `Anonymous _ as v -> v
      | `Encoder enc -> `Encoder (trim_encoder enc)
      | `Labelled (lbl, t) -> `Labelled (lbl, trim_term t))
    params

and trim_encoder (name, params) = (name, trim_encoder_params params)

and trim_ast tm =
  match tm with
    | `Custom _ | `Var _ | `Null | `Int _ | `Float _ | `Bool _ | `String _
    | `Cache_env _ ->
        tm
    | `Tuple l -> `Tuple (List.map trim_term l)
    | `Open (t, t') -> `Open (trim_term t, trim_term t')
    | `Seq (t, t') -> `Seq (trim_term t, trim_term t')
    | `Let ({ def; body } as _let) ->
        `Let { _let with def = trim_term def; body = trim_term body }
    | `List l -> `List (List.map trim_term l)
    | `Cast c -> `Cast { c with cast = trim_term c.cast }
    | `App (t, l) ->
        `App
          ( { (trim_term t) with methods = Methods.empty },
            List.map (fun (lbl, t) -> (lbl, trim_term t)) l )
    | `Invoke { invoked; invoke_default; meth } ->
        let invoked = trim_term invoked in
        `Invoke
          {
            invoked =
              {
                invoked with
                methods =
                  Methods.filter (fun lbl _ -> lbl = meth) invoked.methods;
              };
            invoke_default = Option.map trim_term invoke_default;
            meth;
          }
    | `Hide (tm, l) ->
        let tm = trim_term tm in
        `Hide
          ( {
              tm with
              methods =
                Methods.filter (fun lbl _ -> not (List.mem lbl l)) tm.methods;
            },
            l )
    | `Encoder enc -> `Encoder (trim_encoder enc)
    | `Fun ({ body; arguments } as _fun) ->
        `Fun
          {
            _fun with
            body = trim_term body;
            arguments =
              List.map
                (fun arg ->
                  { arg with default = Option.map trim_term arg.default })
                arguments;
          }

and trim_term ({ t; term; methods } as tm) =
  {
    tm with
    t = trim_type t;
    term = trim_ast term;
    methods = Term.Methods.map trim_term methods;
  }
