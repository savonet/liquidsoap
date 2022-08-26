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

module Regexp = struct
  open Liquidsoap_lang.Regexp

  type t += Regexp of Pcre.regexp
  type sub += Sub of Pcre.substrings

  let cflags_of_flags (flags : flag list) =
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
    Regexp (Pcre.regexp ~iflags s)

  let regexp_or ?(flags = []) l =
    let iflags = Pcre.cflags (cflags_of_flags flags) in
    Regexp (Pcre.regexp_or ~iflags l)

  let get_rex = Option.map (function Regexp r -> r | _ -> assert false)
  let get_sub = function Sub s -> s | _ -> assert false
  let split ?pat ?rex s = Pcre.split ?pat ?rex:(get_rex rex) s
  let exec ?pat ?rex s = Sub (Pcre.exec ?pat ?rex:(get_rex rex) s)
  let test ?pat ?rex s = Pcre.pmatch ?pat ?rex:(get_rex rex) s
  let num_of_subs sub = Pcre.num_of_subs (get_sub sub)
  let get_substring sub pos = Pcre.get_substring (get_sub sub) pos

  let substitute ?pat ?rex ~subst s =
    Pcre.substitute ?pat ?rex:(get_rex rex) ~subst s

  let substitute_first ?pat ?rex ~subst s =
    Pcre.substitute_first ?pat ?rex:(get_rex rex) ~subst s
end

let () =
  Hooks.collect_after := Clock.collect_after;
  Hooks.regexp := (module Regexp : Hooks.Regexp_t);
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
  let open Liquidsoap_lang in
  let open Term in
  match (Type.deref tm.t).Type.descr with
    | Type.Constr
        { Type.constructor = "source"; params = [(Type.Invariant, k)] } ->
        let frame_content_of_t t =
          match (Type.deref t).Type.descr with
            | Type.Var _ -> `Any
            | Type.Constr { Type.constructor; params = [(_, t)] } -> (
                match (Type.deref t).Type.descr with
                  | Type.Custom { Type.typ = Format_type.Type fmt } ->
                      `Format fmt
                  | Type.Var _ -> `Kind (Content.kind_of_string constructor)
                  | _ -> failwith ("Unhandled content: " ^ Type.to_string tm.t))
            | Type.Constr { Type.constructor = "none" } ->
                `Kind (Content.kind_of_string "none")
            | _ -> failwith ("Unhandled content: " ^ Type.to_string tm.t)
        in
        let k = Core_types.of_frame_kind_t k in
        let k =
          Frame.mk_fields
            ~audio:(frame_content_of_t (Frame.find_audio k))
            ~video:(frame_content_of_t (Frame.find_video k))
            ~midi:(frame_content_of_t (Frame.find_midi k))
            ()
        in
        if not (Lang_source.V.is_value v) then
          raise
            (Term.Internal_error
               ( Option.to_list tm.t.Type.pos,
                 "term has type source but is not a source: "
                 ^ Value.to_string v ));
        Kind.unify (Lang_source.V.of_value v)#kind (Kind.of_kind k)
    | _ -> ()

let () = Hooks.eval_check := eval_check

let mk_kind ~pos (kind, params) =
  if kind = "any" then Type.var ~pos ()
  else (
    try
      let k = Content.kind_of_string kind in
      match params with
        | [] -> Core_types.kind_t (`Kind k)
        | [("", "any")] -> Type.var ()
        | [("", "internal")] ->
            Type.var ~constraints:[Format_type.internal_media] ()
        | param :: params ->
            let mk_format (label, value) = Content.parse_param k label value in
            let f = mk_format param in
            List.iter (fun param -> Content.merge f (mk_format param)) params;
            assert (k = Content.kind f);
            Core_types.kind_t (`Format f)
    with _ ->
      let params =
        params |> List.map (fun (l, v) -> l ^ "=" ^ v) |> String.concat ","
      in
      let t = kind ^ "(" ^ params ^ ")" in
      raise (Term.Parse_error (pos, "Unknown type constructor: " ^ t ^ ".")))

let mk_source_ty ~pos name args =
  if name <> "source" then
    raise (Term.Parse_error (pos, "Unknown type constructor: " ^ name ^ "."));

  let audio = ref ("any", []) in
  let video = ref ("any", []) in
  let midi = ref ("any", []) in

  List.iter
    (function
      | "audio", k -> audio := k
      | "video", k -> video := k
      | "midi", k -> midi := k
      | l, _ ->
          raise (Term.Parse_error (pos, "Unknown type constructor: " ^ l ^ ".")))
    args;

  let audio = mk_kind ~pos !audio in
  let video = mk_kind ~pos !video in
  let midi = mk_kind ~pos !midi in

  Core_types.source_t (Core_types.frame_kind_t audio video midi)

let () = Hooks.mk_source_ty := mk_source_ty
