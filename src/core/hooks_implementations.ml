module Hooks = Liquidsoap_lang.Hooks

let () = Hooks.liq_libs_dir := Configure.liq_libs_dir

let () =
  let on_change v =
    Hooks.log_path :=
      if v then (try Some Dtools.Log.conf_file_path#get with _ -> None)
      else None
  in
  Dtools.Log.conf_file#on_change on_change;
  ignore (Option.map on_change Dtools.Log.conf_file#get_d)

type sub = Lang.Regexp.sub = {
  matches : string option list;
  groups : (string * string) list;
}

let cflags_of_flags (flags : Liquidsoap_lang.Regexp.flag list) =
  List.fold_left
    (fun l f ->
      match f with
        | `i -> `CASELESS :: l
        (* `g is handled at the call level. *)
        | `g -> l
        | `s -> `DOTALL :: l
        | `m -> `MULTILINE :: l)
    [] flags

let regexp ?(flags = []) s =
  let iflags = Pcre.cflags (cflags_of_flags flags) in
  let rex = Pcre.regexp ~iflags s in
  object
    method split s = Pcre.split ~rex s

    method exec s =
      let sub = Pcre.exec ~rex s in
      let matches = Array.to_list (Pcre.get_opt_substrings sub) in
      let groups =
        List.fold_left
          (fun groups name ->
            try (name, Pcre.get_named_substring rex name sub) :: groups
            with _ -> groups)
          []
          (Array.to_list (Pcre.names rex))
      in
      { Lang.Regexp.matches; groups }

    method test s = Pcre.pmatch ~rex s

    method substitute ~subst s =
      let substitute =
        if List.mem `g flags then Pcre.substitute else Pcre.substitute_first
      in
      substitute ~rex ~subst s
  end

let () =
  Hooks.collect_after := Clock.collect_after;
  Hooks.regexp := regexp;
  (Hooks.make_log := fun name -> (Log.make name :> Hooks.log));
  Hooks.type_of_encoder := Lang_encoder.type_of_encoder;
  Hooks.make_encoder := Lang_encoder.make_encoder;
  Hooks.has_encoder :=
    fun fmt ->
      try
        let (_ : Encoder.factory) =
          Encoder.get_factory (Lang_encoder.V.of_value fmt)
        in
        true
      with _ -> false

let eval_check ~env:_ ~tm v =
  let open Liquidsoap_lang.Term in
  if Lang_source.V.is_value v then
    Typing.(tm.t <: Lang.source_t (Lang_source.V.of_value v)#frame_type)

let () = Hooks.eval_check := eval_check

let mk_field_t ~pos (kind, params) =
  if kind = "any" then Type.var ~pos ()
  else (
    try
      let k = Content.kind_of_string kind in
      match params with
        | [] -> Type.make (Format_type.descr (`Kind k))
        | [("", "any")] -> Type.var ()
        | [("", "internal")] ->
            Type.var ~constraints:[Format_type.internal_media] ()
        | param :: params ->
            let mk_format (label, value) = Content.parse_param k label value in
            let f = mk_format param in
            List.iter (fun param -> Content.merge f (mk_format param)) params;
            assert (k = Content.kind f);
            Type.make (Format_type.descr (`Format f))
    with _ ->
      let params =
        params |> List.map (fun (l, v) -> l ^ "=" ^ v) |> String.concat ","
      in
      let t = kind ^ "(" ^ params ^ ")" in
      raise (Term.Parse_error (pos, "Unknown type constructor: " ^ t ^ ".")))

let mk_source_ty ~pos name args =
  if name <> "source" then
    raise (Term.Parse_error (pos, "Unknown type constructor: " ^ name ^ "."));

  match args with
    | [] -> Lang.source_t (Lang.frame_t (Lang.univ_t ()) Frame.Fields.empty)
    | args ->
        let fields =
          List.fold_left
            (fun fields (lbl, k) ->
              Frame.Fields.add
                (Frame.field_of_string lbl)
                (mk_field_t ~pos k) fields)
            Frame.Fields.empty args
        in

        Lang.source_t (Lang.frame_t Lang.unit_t fields)

let () = Hooks.mk_source_ty := mk_source_ty
