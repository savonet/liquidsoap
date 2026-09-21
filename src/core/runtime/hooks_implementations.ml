module Lang = Liquidsoap_lang.Lang

(* For source eval check there are cases of:
     source('a) <: (source('a).{ source methods })?
   b/c of source.dynamic so we want to dig deeper
   than the regular demeth. *)
let rec deep_demeth t =
  match Type.demeth t with
    | Type.{ descr = Nullable t } -> deep_demeth t
    | t -> t

let is_nullable t =
  match Type.demeth t with Type.{ descr = Nullable _ } -> true | _ -> false

let track_demeth t =
  Type.map_meths t (fun ({ Type.scheme = vars, typ } as m) ->
      { m with scheme = (vars, Type.demeth typ) })

let strip_tracks ty =
  let ty = Type.hide_meth "track_marks" ty in
  let ty = Type.hide_meth "metadata" ty in
  track_demeth ty

let strip_source_tracks ty =
  match Type.deref ty with
    | Type.
        {
          descr =
            Constr { constructor = "source"; params = [(`Invariant, frame_t)] };
        } ->
        Type.
          {
            ty with
            descr =
              Constr
                {
                  constructor = "source";
                  params = [(`Invariant, strip_tracks frame_t)];
                };
          }
    | _ -> ty

let eval_check ~env:_ ~tm v =
  if Lang_source.Source_val.is_value v then (
    let s = Lang_source.Source_val.of_value v in
    if not s#has_content_type then (
      let ty = Type.fresh (deep_demeth tm.Term.t) in
      Typing.(
        Lang_source.source_t ~methods:false (strip_tracks s#frame_type)
        <: strip_source_tracks ty);
      s#content_type_computation_allowed))
  else if Source_tracks.is_value v then (
    let s = Source_tracks.source v in
    Typing.(strip_tracks s#frame_type <: strip_tracks (Type.fresh tm.Term.t)))
  else if Track.is_value v then (
    let field, source = Lang_source.to_track v in
    if not source#has_content_type then (
      match field with
        | _ when field = Frame.Fields.metadata -> ()
        | _ when field = Frame.Fields.track_marks -> ()
        | _ -> (
            let ty = Type.fresh (deep_demeth tm.Term.t) in
            let frame_t =
              Frame_type.make (Lang.univ_t ())
                (Frame.Fields.add field ty Frame.Fields.empty)
            in
            try Typing.(source#frame_type <: frame_t)
            with _ when is_nullable tm.Term.t -> ())))

(* A clock is a custom type and a source's methods are a table of values, so
   neither can be built where liquidsoap's core is not linked -- unlike the
   types in liquidsoap.core_lang, which are computed from the language and the
   content formats alone.

   A dump carries both as types instead, and a reader installs them as
   stand-ins: a clock read back that way has no operations of its own, which
   is enough to name one in an annotation. *)
let () =
  Hooks.implement Hooks.mk_clock_ty (fun ?pos () ->
      Type.make
        ?pos:(Option.map Liquidsoap_lang_prelude.Pos.of_lexing_pos pos)
        Lang_clock.ClockValue.base_t.Type.descr)

let register () =
  Hooks.implement Hooks.liq_libs_dir Configure.liq_libs_dir;
  let on_change v =
    Hooks.log_path :=
      if v then (try Some Dtools.Log.conf_file_path#get with _ -> None)
      else None
  in
  Dtools.Log.conf_file#on_change on_change;
  Option.iter on_change Dtools.Log.conf_file#get_d;
  Hooks.implement Hooks.make_log (fun name -> (Log.make name :> Hooks.log));
  Hooks.implement Hooks.make_encoder Lang_encoder.make_encoder;
  Hooks.implement Hooks.eval_check eval_check;
  Hooks.implement Hooks.has_encoder (fun fmt ->
      try
        let (_ : Encoder.factory) =
          Encoder.get_factory (Lang_encoder.V.of_value fmt)
        in
        true
      with _ -> false);
  Hooks.getpwnam := Unix.getpwnam;
  Hooks.implement Hooks.source_methods_t (fun () ->
      Lang_source.source_t ~methods:true (Lang.univ_t ()))
