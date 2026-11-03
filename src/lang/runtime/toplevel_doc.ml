let scheme_string ~generalized (m : Type.meth) =
  let vars, t = m.scheme in
  Repr.string_of_scheme (vars @ generalized, t)

let fields ~generalized documented t =
  let description (m : Type.meth) =
    match List.assoc_opt m.meth documented with
      | Some f -> f.Doc.Value.meth_description
      | None when m.doc.meth_descr <> "" -> Some m.doc.meth_descr
      | None -> None
  in
  let fields = fst (Type.split_meths t) in
  if List.for_all (fun m -> description m = None) fields then []
  else
    List.map
      (fun (m : Type.meth) ->
        ( (if m.optional then m.meth ^ "?" else m.meth),
          Doc.Value.
            {
              meth_type = scheme_string ~generalized m;
              meth_description = description m;
            } ))
      fields

let rec parameter_types t =
  match (Type.deref t).Type.descr with
    | Type.Arrow (p, _) -> p
    | Type.Meth (_, t) -> parameter_types t
    | _ -> []

let default_values = function
  | Value.Fun { fun_args } -> List.map (fun (l, _, o) -> (l, o)) fun_args
  | _ -> []

(* Unlabeled arguments share a label: each one takes the first documentation
   and default value left for it. *)
let arguments ~generalized (doc : Doc.Value.t) t v =
  let take label l = (List.assoc_opt label l, List.remove_assoc label l) in
  let _, arguments =
    List.fold_left_map
      (fun (documented, defaults) (_, label, t) ->
        let label = if label = "" then None else Some label in
        let argument, documented = take label documented in
        let default, defaults =
          take (Option.value ~default:"" label) defaults
        in
        let description, documented_fields =
          match argument with
            | Some a -> (a.Doc.Value.arg_description, a.arg_fields)
            | None -> (None, [])
        in
        ( (documented, defaults),
          ( label,
            Doc.Value.
              {
                arg_type = Repr.string_of_type ~generalized t;
                arg_default = Option.map Value.to_string (Option.join default);
                arg_description = description;
                arg_fields = fields ~generalized documented_fields t;
              } ) ))
      (doc.arguments, default_values v)
      (parameter_types t)
  in
  arguments

(* A function's own methods are dropped: they are documented as definitions of
   their own, such as [thread.run.recurrent]. *)
let split_methods t =
  let methods, t = Type.split_meths t in
  match (Type.deref t).Type.descr with
    | Type.Arrow (p, a) ->
        let methods, a = Type.split_meths a in
        (methods, Type.make ?pos:t.Type.pos (Type.Arrow (p, a)))
    | _ -> (methods, t)

(* The comment's [@callback] and [@method] override a method's own category
   and description. *)
let method_entry ~generalized (doc : Doc.Value.t) (m : Type.meth) =
  let description, category =
    match List.assoc_opt m.meth doc.callbacks with
      | Some cb -> (cb.meth_description, `Callback)
      | None -> (
          match List.assoc_opt m.meth doc.methods with
            | Some entry -> (entry.meth_description, m.doc.category)
            | None when m.doc.meth_descr = "" -> (None, m.doc.category)
            | None -> (Some m.doc.meth_descr, m.doc.category))
  in
  ( category,
    ( m.meth,
      Doc.Value.
        {
          meth_type = scheme_string ~generalized m;
          meth_description = description;
        } ) )

let description_of name entries =
  match List.assoc_opt name entries with
    | Some Doc.Value.{ meth_description = Some s; _ } when s <> "" -> Some s
    | _ -> None

let fill ~generalized t v doc =
  let arguments = arguments ~generalized doc t v in
  let methods, t = split_methods t in
  let entries = List.map (method_entry ~generalized doc) methods in
  let of_category c =
    List.filter_map (fun (c', e) -> if c = c' then Some e else None) entries
  in
  let methods = of_category `Method in
  let composition = of_category `Composition in
  {
    doc with
    typ = Repr.string_of_type ~generalized t;
    arguments;
    methods;
    callbacks = of_category `Callback;
    composition;
    sync_description = description_of "self_sync_description" methods;
    composition_description = description_of "composition_type" composition;
  }
