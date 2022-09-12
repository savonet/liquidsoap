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
  let names = function Regexp r -> Pcre.names r | _ -> assert false
  let test ?pat ?rex s = Pcre.pmatch ?pat ?rex:(get_rex rex) s
  let num_of_subs sub = Pcre.num_of_subs (get_sub sub)
  let get_substring sub pos = Pcre.get_substring (get_sub sub) pos

  let get_named_substring = function
    | Regexp r -> fun name sub -> Pcre.get_named_substring r name (get_sub sub)
    | _ -> assert false

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
  let open Liquidsoap_lang.Term in
  if Lang_source.V.is_value v then
    Typing.(tm.t <: Lang.source_t (Lang_source.V.of_value v)#frame_type)

let () = Hooks.eval_check := eval_check

let mk_kind ~pos (kind, params) =
  if kind = "any" then Type.var ~pos ()
  else (
    try
      let k = Content.kind_of_string kind in
      match params with
        | [] -> Lang.kind_t (`Kind k)
        | [("", "any")] -> Type.var ()
        | [("", "internal")] ->
            Type.var ~constraints:[Format_type.internal_media] ()
        | param :: params ->
            let mk_format (label, value) = Content.parse_param k label value in
            let f = mk_format param in
            List.iter (fun param -> Content.merge f (mk_format param)) params;
            assert (k = Content.kind f);
            Lang.kind_t (`Format f)
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

  Lang.source_t (Frame_type.make ~audio ~video ~midi ())

let () = Hooks.mk_source_ty := mk_source_ty
