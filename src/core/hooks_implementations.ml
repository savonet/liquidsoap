module Hooks = Liquidsoap_lang.Hooks
module Lang = Liquidsoap_lang.Lang
module Cache = Liquidsoap_lang.Cache

(* For source eval check there are cases of:
     source('a) <: (source('a).{ source methods })?
   b/c of source.dynamic so we want to dig deeper
   than the regular demeth. *)
let rec deep_demeth t =
  match Type.demeth t with
    | Type.{ descr = Nullable t } -> deep_demeth t
    | t -> t

let eval_check ~env:_ ~tm v =
  if Lang_source.Source_val.is_value v then (
    let s = Lang_source.Source_val.of_value v in
    if not s#has_content_type then (
      let ty = Type.fresh (deep_demeth tm.Term.t) in
      Typing.(Lang_source.source_t ~methods:false s#frame_type <: ty);
      s#content_type_computation_allowed))
  else if Track.is_value v then (
    let field, source = Lang_source.to_track v in
    if not source#has_content_type then (
      match field with
        | _ when field = Frame.Fields.metadata -> ()
        | _ when field = Frame.Fields.track_marks -> ()
        | _ ->
            let ty = Type.fresh (deep_demeth tm.Term.t) in
            let frame_t =
              Frame_type.make (Lang.univ_t ())
                (Frame.Fields.add field ty Frame.Fields.empty)
            in
            Typing.(source#frame_type <: frame_t)))

let render_string = function
  | `Verbatim s -> s
  | `String (pos, (sep, s)) -> Liquidsoap_lang.Lexer.render_string ~pos ~sep s

let mk_field_t ~pos kind params =
  match kind with
    | "any" -> Type.var ~pos:(Pos.of_lexing_pos pos) ()
    | "none" | "never" -> Type.make Type.Never
    | _ -> (
        try
          let k = Content.kind_of_string kind in
          match params with
            | [] -> Type.make (Format_type.descr (`Kind k))
            | [("", `Verbatim "any")] -> Type.var ()
            | [("", `Verbatim "internal")] ->
                Type.var ~constraints:[Format_type.internal_tracks] ()
            | param :: params ->
                let mk_format (label, value) =
                  let value = render_string value in
                  Content.parse_param k label value
                in
                let f = mk_format param in
                List.iter
                  (fun param -> Content.merge f (mk_format param))
                  params;
                assert (k = Content.kind f);
                Type.make (Format_type.descr (`Format f))
        with _ ->
          let params =
            params
            |> List.map (fun (l, v) -> l ^ "=" ^ render_string v)
            |> String.concat ","
          in
          let t = kind ^ "(" ^ params ^ ")" in
          raise
            (Liquidsoap_lang.Term_base.Parse_error
               (pos, "Unknown type constructor: " ^ t ^ ".")))

let () =
  Hooks.mk_clock_ty :=
    fun ?pos () ->
      Type.make
        ?pos:(Option.map Liquidsoap_lang.Pos.of_lexing_pos pos)
        Lang_source.ClockValue.base_t.Type.descr

let mk_source_ty ?pos name { Liquidsoap_lang.Parsed_term.extensible; tracks } =
  let pos = Option.value ~default:(Lexing.dummy_pos, Lexing.dummy_pos) pos in

  if name <> "source" then
    raise
      (Liquidsoap_lang.Term_base.Parse_error
         (pos, "Unknown type constructor: " ^ name ^ "."));

  match tracks with
    | [] ->
        Lang_source.source_t
          (Frame_type.make (Lang.univ_t ()) Frame.Fields.empty)
    | tracks ->
        let fields =
          List.fold_left
            (fun fields
                 {
                   Liquidsoap_lang.Parsed_term.track_name;
                   track_type;
                   track_params;
                 } ->
              Frame.Fields.add
                (Frame.Fields.field_of_string track_name)
                (mk_field_t ~pos track_type track_params)
                fields)
            Frame.Fields.empty tracks
        in
        let base = if extensible then Lang.univ_t () else Lang.unit_t in

        Lang_source.source_t (Frame_type.make base fields)

let register () =
  Hooks.liq_libs_dir := Configure.liq_libs_dir;
  let on_change v =
    Hooks.log_path :=
      if v then (try Some Dtools.Log.conf_file_path#get with _ -> None)
      else None
  in
  Dtools.Log.conf_file#on_change on_change;
  ignore (Option.map on_change Dtools.Log.conf_file#get_d);
  (Hooks.make_log := fun name -> (Log.make name :> Hooks.log));
  Hooks.type_of_encoder := Lang_encoder.type_of_encoder;
  Hooks.make_encoder := Lang_encoder.make_encoder;
  Hooks.eval_check := eval_check;
  (Hooks.has_encoder :=
     fun fmt ->
       try
         let (_ : Encoder.factory) =
           Encoder.get_factory (Lang_encoder.V.of_value fmt)
         in
         true
       with _ -> false);
  Hooks.mk_source_ty := mk_source_ty;
  Hooks.getpwnam := Unix.getpwnam;
  Hooks.source_methods_t :=
    fun () -> Lang_source.source_t ~methods:true (Lang.univ_t ())

let conf_cache =
  Dtools.Conf.void ~p:(Configure.conf#plug "cache") "Cache configuration"

let conf_cache_max_days =
  Dtools.Conf.int
    ~p:(conf_cache#plug "max_days")
    ~d:10
    "Delete cache file that have not been touched after this number of days."

let conf_cache_max_files =
  Dtools.Conf.int
    ~p:(conf_cache#plug "max_files")
    ~d:200 "Keep at most that number of cache files. Delete folder files first."

module Term_cache = Liquidsoap_lang.Term_cache

let cache_log = Log.make ["cache"]

let cache_maintenance () =
  let max_timestamp =
    Unix.time () -. (float conf_cache_max_days#get *. 86400.)
  in
  try
    match Cache.dir () with
      | Some dir when Sys.file_exists dir && Sys.is_directory dir ->
          let files =
            Array.fold_left
              (fun files fname ->
                if String.ends_with ~suffix:".liq-cache" fname then (
                  let filename = Filename.concat dir fname in
                  let stats = Unix.stat filename in
                  match Unix.stat filename with
                    | { Unix.st_atime } when st_atime < max_timestamp ->
                        cache_log#info "File %s is too old, deleting.." fname;
                        Unix.unlink filename;
                        files
                    | _ -> (stats, filename) :: files)
                else files)
              [] (Sys.readdir dir)
          in
          let len = List.length files in
          let max_files = conf_cache_max_files#get in
          if max_files < len then (
            let len = len - max_files in
            cache_log#info "Too many cached files! Deleting %d oldest ones.."
              len;
            let files =
              List.sort
                (fun ({ Unix.st_atime = t }, _) ({ Unix.st_atime = t' }, _) ->
                  Stdlib.compare t t')
                files
            in
            List.iteri
              (fun pos (_, filename) ->
                if pos < len then (
                  cache_log#info "Deleting %s.." (Filename.basename filename);
                  Unix.unlink filename))
              files)
      | _ -> ()
  with exn ->
    let bt = Printexc.get_backtrace () in
    Utils.log_exception ~log:cache_log ~bt
      (Printf.sprintf "Error while cleaning up cache: %s"
         (Printexc.to_string exn))

let () = Hooks.cache_maintenance := cache_maintenance
