(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

module Term = Lang_values
include Term.V
module T = Lang_types

type t = T.t

let log = Log.make ["lang"]

(** Type construction *)

let ground_t x = T.make (T.Ground x)

let int_t = ground_t T.Int

let unit_t = T.make T.unit

let float_t = ground_t T.Float

let bool_t = ground_t T.Bool

let string_t = ground_t T.String

let tuple_t l = T.make (T.Tuple l)

let product_t a b = tuple_t [a; b]

let of_tuple_t t =
  match (T.deref t).T.descr with T.Tuple l -> l | _ -> assert false

let of_product_t t =
  match of_tuple_t t with [a; b] -> (a, b) | _ -> assert false

let fun_t p b = T.make (T.Arrow (p, b))

let list_t t = T.make (T.List t)

let of_list_t t =
  match (T.deref t).T.descr with T.List t -> t | _ -> assert false

let metadata_t = list_t (product_t string_t string_t)

let zero_t = Term.zero_t

let succ_t t = Term.succ_t t

let variable_t = Term.variable_t

let add_t = Term.add_t

let type_of_int = Term.type_of_int

let univ_t ?(constraints = []) () = T.fresh ~level:0 ~constraints ~pos:None

let string_getter_t () = univ_t ~constraints:[T.Getter T.String] ()

let float_getter_t () = univ_t ~constraints:[T.Getter T.Float] ()

let int_getter_t () = univ_t ~constraints:[T.Getter T.Int] ()

let bool_getter_t () = univ_t ~constraints:[T.Getter T.Bool] ()

let frame_kind_t ~audio ~video ~midi = Term.frame_kind_t audio video midi

let of_frame_kind_t t = Term.of_frame_kind_t t

let source_t t = Term.source_t t

let of_source_t t = Term.of_source_t t

let format_t t = Term.format_t t

let request_t t = Term.request_t t

let of_request_t t = Term.of_request_t t

let rec t_of_mul = function
  | Frame.Zero ->
      zero_t
  | Frame.Variable ->
      variable_t
  | Frame.Succ m ->
      succ_t (t_of_mul m)

let kind_type_of_frame_kind kind =
  let audio = t_of_mul kind.Frame.audio in
  let video = t_of_mul kind.Frame.video in
  let midi = t_of_mul kind.Frame.midi in
  frame_kind_t ~audio ~video ~midi

(** Given a Lang type that has been inferred, convert it to a kind.
  * This might require to force some Any_fixed variables. *)
let rec mul_of_type default t =
  match (T.deref t).T.descr with
    | T.Succ t ->
        Frame.Succ (mul_of_type (default - 1) t)
    | T.Zero ->
        Frame.Zero
    | T.Variable ->
        Frame.Variable
    | T.EVar _ ->
        let default = max 0 default in
        T.bind t (type_of_int default) ;
        Frame.mul_of_int default
    | _ ->
        assert false

(* TODO can happen e.g. on request.queue() *)

let frame_kind_of_kind_type t =
  let k = Term.of_frame_kind_t t in
  {
    Frame.audio= mul_of_type (Lazy.force Frame.audio_channels) k.Frame.audio;
    video= mul_of_type (Lazy.force Frame.video_channels) k.Frame.video;
    midi= mul_of_type (Lazy.force Frame.midi_channels) k.Frame.midi;
  }

(** Description of how many of a channel type does an operator want.
  * [Fixed n] means exactly [n] channels.
  * [Any_fixed n] means any fixed numbers of channels that is [>=n].
  * [Variable n] means any (possibly variable) number of channels that
  *   is [>=n]. *)
type lang_kind_format = Fixed of int | Any_fixed of int | Variable of int

type lang_kind_formats =
  | Unconstrained of t
  | Constrained of
      (lang_kind_format, lang_kind_format, lang_kind_format) Frame.fields

let any_fixed =
  Constrained {Frame.audio= Any_fixed 0; video= Any_fixed 0; midi= Any_fixed 0}

let empty = Constrained {Frame.audio= Fixed 0; video= Fixed 0; midi= Fixed 0}

let any_fixed_with ?(audio = 0) ?(video = 0) ?(midi = 0) () =
  Constrained
    {
      Frame.audio= Any_fixed audio;
      video= Any_fixed video;
      midi= Any_fixed midi;
    }

let audio_variable =
  Constrained {Frame.audio= Variable 1; video= Fixed 0; midi= Fixed 0}

let audio_any =
  Constrained {Frame.audio= Any_fixed 1; video= Fixed 0; midi= Fixed 0}

let audio_n n =
  Constrained {Frame.audio= Fixed n; video= Fixed 0; midi= Fixed 0}

let audio_mono = audio_n 1

let audio_stereo = audio_n 2

let video_only =
  Constrained {Frame.audio= Fixed 0; video= Fixed 1; midi= Fixed 0}

let video =
  Constrained {Frame.audio= Any_fixed 0; video= Fixed 1; midi= Any_fixed 0}

let audio_video_any =
  Constrained {Frame.audio= Any_fixed 0; video= Any_fixed 0; midi= Fixed 0}

let video_n n =
  Constrained {Frame.audio= Fixed 0; video= Fixed n; midi= Fixed 0}

let midi_n n =
  Constrained {Frame.audio= Fixed 0; video= Fixed 0; midi= Fixed n}

let midi_only = midi_n 1

let kind_type_of_kind_format fmt =
  match fmt with
    | Unconstrained t ->
        t
    | Constrained fields ->
        let aux = function
          | Fixed i ->
              type_of_int i
          | Any_fixed i ->
              Term.add_t i (univ_t ~constraints:[T.Arity_fixed] ())
          | Variable i ->
              Term.add_t i variable_t
        in
        let audio = aux fields.Frame.audio in
        let video = aux fields.Frame.video in
        let midi = aux fields.Frame.midi in
        frame_kind_t ~audio ~video ~midi

(** Value construction *)

let mk ~t v = {t; value= v}

let unit = mk ~t:unit_t unit

let int i = mk ~t:int_t (Int i)

let bool i = mk ~t:bool_t (Bool i)

let float i = mk ~t:float_t (Float i)

let string i = mk ~t:string_t (String i)

let tuple l = mk ~t:(tuple_t (List.map (fun a -> a.t) l)) (Tuple l)

let product a b = tuple [a; b]

let list ~t l = mk ~t:(list_t t) (List l)

let source s = mk ~t:(source_t (kind_type_of_frame_kind s#kind)) (Source s)

let request r =
  let kind =
    match Request.kind r with
      | Some k ->
          k
      | None ->
          let z = Frame.Zero in
          {Frame.audio= z; video= z; midi= z}
  in
  mk ~t:(request_t (kind_type_of_frame_kind kind)) (Request r)

let val_fun p ~ret_t f =
  let f env t =
    f
      (List.map
         (fun (x, (g, v)) ->
           assert (g = []) ;
           (x, v))
         env)
      t
  in
  let t = fun_t (List.map (fun (l, _, t, d) -> (d <> None, l, t)) p) ret_t in
  let p' = List.map (fun (l, x, _, d) -> (l, x, d)) p in
  mk ~t (FFI (p', [], f))

let val_cst_fun p c =
  let t = fun_t (List.map (fun (l, t, d) -> (d <> None, l, t)) p) c.t in
  let p' = List.map (fun (l, _, d) -> (l, l, d)) p in
  let f tm = mk ~t (Fun (p', [], [], {Term.t= c.t; Term.term= tm})) in
  (* Convert the value into a term if possible,
   * to enable introspection, mostly for printing. *)
  match c.value with
    | Tuple [] ->
        f Term.unit
    | Int i ->
        f (Term.Int i)
    | Bool i ->
        f (Term.Bool i)
    | Float i ->
        f (Term.Float i)
    | String i ->
        f (Term.String i)
    | _ ->
        mk ~t (FFI (p', [], fun _ _ -> c))

let metadata m =
  list
    ~t:(product_t string_t string_t)
    (Hashtbl.fold (fun k v l -> product (string k) (string v) :: l) m [])

(** Helpers for defining protocols. *)

let to_proto_doc ~syntax ~static doc =
  let item = new Doc.item ~sort:false doc in
  item#add_subsection "syntax" (Doc.trivial syntax) ;
  item#add_subsection "static" (Doc.trivial (string_of_bool static)) ;
  item

let add_protocol ~syntax ~doc ~static name resolver =
  let doc = to_proto_doc ~syntax ~static doc in
  let spec = {Request.static; resolve= resolver} in
  Request.protocols#register ~doc name spec

(** Helpers for defining builtin functions. *)

type proto = (string * t * value option * string option) list

let doc_of_prototype_item ~generalized t d doc =
  let doc = match doc with None -> "(no doc)" | Some d -> d in
  let item = new Doc.item doc in
  item#add_subsection "type" (T.doc_of_type ~generalized t) ;
  item#add_subsection "default"
    ( match d with
      | None ->
          Doc.trivial "None"
      | Some d ->
          Doc.trivial (print_value d) ) ;
  item

type doc_flag = Hidden | Deprecated | Experimental

let string_of_flag = function
  | Hidden ->
      "hidden"
  | Deprecated ->
      "deprecated"
  | Experimental ->
      "experimental"

let builtin_type p t =
  T.make
    (T.Arrow (List.map (fun (lbl, t, opt, _) -> (opt <> None, lbl, t)) p, t))

let to_plugin_doc category flags main_doc proto return_t =
  let item = new Doc.item ~sort:false main_doc in
  let t = builtin_type proto return_t in
  let generalized = T.filter_vars (fun _ -> true) t in
  item#add_subsection "_category" (Doc.trivial category) ;
  item#add_subsection "_type" (T.doc_of_type ~generalized t) ;
  List.iter
    (fun f -> item#add_subsection "_flag" (Doc.trivial (string_of_flag f)))
    flags ;
  List.iter
    (fun (l, t, d, doc) ->
      item#add_subsection
        (if l = "" then "(unlabeled)" else l)
        (doc_of_prototype_item ~generalized t d doc))
    proto ;
  item

let add_builtin ~category ~descr ?(flags = []) name proto return_t f =
  let t = builtin_type proto return_t in
  let f env t =
    f
      (List.map
         (fun (s, (g, v)) ->
           assert (g = []) ;
           (s, v))
         env)
      t
  in
  let value =
    {
      t;
      value=
        FFI (List.map (fun (lbl, _, opt, _) -> (lbl, lbl, opt)) proto, [], f);
    }
  in
  let generalized = T.filter_vars (fun _ -> true) t in
  Term.builtins#register
    ~doc:(to_plugin_doc category flags descr proto return_t)
    name (generalized, value)

let add_builtin_base ~category ~descr ?(flags = []) name value t =
  let doc = new Doc.item ~sort:false descr in
  let value = {t; value} in
  let generalized = T.filter_vars (fun _ -> true) t in
  doc#add_subsection "_category" (Doc.trivial category) ;
  doc#add_subsection "_type" (T.doc_of_type ~generalized t) ;
  List.iter
    (fun f -> doc#add_subsection "_flag" (Doc.trivial (string_of_flag f)))
    flags ;
  Term.builtins#register ~doc name (generalized, value)

(** Specialized version for operators, that is builtins returning sources. *)

type category =
  | Input
  | Output
  | Conversions
  | TrackProcessing
  | SoundProcessing
  | VideoProcessing
  | MIDIProcessing
  | Visualization
  | SoundSynthesis
  | Liquidsoap

let string_of_category x =
  "Source / "
  ^
  match x with
    | Input ->
        "Input"
    | Output ->
        "Output"
    | Conversions ->
        "Conversions"
    | TrackProcessing ->
        "Track Processing"
    | SoundProcessing ->
        "Sound Processing"
    | VideoProcessing ->
        "Video Processing"
    | MIDIProcessing ->
        "MIDI Processing"
    | SoundSynthesis ->
        "Sound Synthesis"
    | Visualization ->
        "Visualization"
    | Liquidsoap ->
        "Liquidsoap"

(** An operator is a builtin function that builds a source.
  * It is registered using the wrapper [add_operator].
  * Creating the associated function type (and function) requires some work:
  *  - Specify which content_kind the source will carry:
  *    a given fixed number of channels, any fixed, a variable number?
  *  - The content_kind can also be linked to a type variable,
  *    e.g. the parameter of a format type.
  * From this high-level description a type is created. Often it will
  * carry a type constraint.
  * Once the type has been inferred, the function might be executed,
  * and at this point the type might still not be known completely
  * so we have to force its value withing the acceptable range. *)

let add_operator ~category ~descr ?(flags = []) ?(active = false) name proto
    ~kind f =
  let compare (x, _, _, _) (y, _, _, _) =
    match (x, y) with
      | "", "" ->
          0
      | _, "" ->
          -1
      | "", _ ->
          1
      | x, y ->
          compare x y
  in
  let proto =
    let t = T.make (T.Ground T.String) in
    ( "id",
      t,
      Some {t; value= String ""},
      Some "Force the value of the source ID." )
    :: List.stable_sort compare proto
  in
  let f env t =
    let kind_t = Term.of_source_t t in
    let k = frame_kind_of_kind_type kind_t in
    let src : Source.source = f env k in
    let id =
      match (List.assoc "id" env).value with
        | String s ->
            s
        | _ ->
            assert false
    in
    if id <> "" then src#set_id id ;
    {t; value= Source src}
  in
  let f env t =
    try f env t with
      | Source.Clock_conflict (a, b) ->
          raise (Lang_errors.Clock_conflict (t.T.pos, a, b))
      | Source.Clock_loop (a, b) ->
          raise (Lang_errors.Clock_loop (t.T.pos, a, b))
  in
  let kind_type = kind_type_of_kind_format kind in
  let return_t = Term.source_t ~active kind_type in
  let category = string_of_category category in
  add_builtin ~category ~descr ~flags name proto return_t f

exception Found

(** List of references for which iter_sources had to give up --- see below. *)
let static_analysis_failed = ref []

let iter_sources f v =
  let rec iter_term env v =
    match v.Term.term with
      | Term.Bool _
      | Term.String _
      | Term.Int _
      | Term.Float _
      | Term.Encoder _ ->
          ()
      | Term.List l ->
          List.iter (iter_term env) l
      | Term.Ref a | Term.Get a ->
          iter_term env a
      | Term.Tuple l ->
          List.iter (iter_term env) l
      | Term.Let {Term.def= a; body= b; _} | Term.Seq (a, b) | Term.Set (a, b)
        ->
          iter_term env a ; iter_term env b
      | Term.Var v -> (
        (* If it's locally bound it won't be in [env]. *)
        (* TODO since inner-bound variables don't mask outer ones in [env],
         *   we are actually checking values that may be out of reach. *)
        try
          let gv = List.assoc v env in
          if Lazy.is_val gv then (
            let _, v = Lazy.force gv in
            iter_value v )
          else ()
        with Not_found -> () )
      | Term.App (a, l) ->
          iter_term env a ;
          List.iter (fun (_, v) -> iter_term env v) l
      | Term.Fun (_, proto, body) | Term.RFun (_, _, proto, body) ->
          iter_term env body ;
          List.iter
            (fun (_, _, _, v) ->
              match v with Some v -> iter_term env v | None -> ())
            proto
  and iter_value v =
    match v.value with
      | Source s ->
          f s
      | Bool _ | Int _ | Float _ | String _ | Request _ | Encoder _ ->
          ()
      | List l ->
          List.iter iter_value l
      | Tuple l ->
          List.iter iter_value l
      | Fun (proto, pe, env, body) ->
          (* The following is necessarily imprecise: we might see
           * sources that will be unused in the execution of the function. *)
          iter_term env body ;
          List.iter (fun (_, (_, v)) -> iter_value v) pe ;
          List.iter (function _, _, Some v -> iter_value v | _ -> ()) proto
      | FFI (proto, pe, _) ->
          List.iter (fun (_, (_, v)) -> iter_value v) pe ;
          List.iter (function _, _, Some v -> iter_value v | _ -> ()) proto
      | Ref r ->
          if List.memq r !static_analysis_failed then ()
          else (
            (* Do not walk inside references, otherwise the list of "contained"
             * sources may change from one time to the next, which makes it
             * impossible to avoid ill-balanced activations.
             * Not walking inside references does not break things more than they
             * are already: detecting sharing in presence of references to sources
             * cannot be done statically anyway.)
             * Display a fat log message to warn about this risky situation,
             * which probably won't prevent users to get biffled... *)
            let may_have_source =
              try
                let _, has_var_pos =
                  Lang_types.iter_constr
                    (fun pos c ->
                      if
                        pos
                        &&
                        match c with
                          | {T.name= "source"; _} ->
                              true
                          | _ ->
                              false
                      then raise Found)
                    v.t
                in
                has_var_pos
              with Found -> true
            in
            static_analysis_failed := r :: !static_analysis_failed ;
            if may_have_source then
              log#severe
                "WARNING! Found a reference, potentially containing sources, \
                 inside a dynamic source-producing function. Static analysis \
                 cannot be performed: make sure you are not sharing sources \
                 contained in references!" )
  in
  iter_value v

let apply f p ~t = Clock.collect_after (fun () -> Term.apply f p ~t)

(** {1 High-level manipulation of values} *)

let to_unit t = match t.value with Tuple [] -> () | _ -> assert false

let to_bool t = match t.value with Bool b -> b | _ -> assert false

let to_bool_getter t =
  match t.value with
    | Bool b ->
        fun () -> b
    | Fun _ | FFI _ -> (
        fun () ->
          match (apply ~t:bool_t t []).value with
            | Bool b ->
                b
            | _ ->
                assert false )
    | _ ->
        assert false

let to_fun ~t f =
  match f.value with
    | Fun _ | FFI _ ->
        fun args -> apply ~t f args
    | _ ->
        assert false

let to_string t = match t.value with String s -> s | _ -> assert false

let to_string_getter t =
  match t.value with
    | String s ->
        fun () -> s
    | Fun _ | FFI _ -> (
        fun () ->
          match (apply ~t:string_t t []).value with
            | String s ->
                s
            | _ ->
                assert false )
    | _ ->
        assert false

let to_float t = match t.value with Float s -> s | _ -> assert false

let to_float_getter t =
  match t.value with
    | Float s ->
        fun () -> s
    | Fun _ | FFI _ -> (
        fun () ->
          match (apply ~t:float_t t []).value with
            | Float s ->
                s
            | _ ->
                assert false )
    | _ ->
        assert false

let to_source t = match t.value with Source s -> s | _ -> assert false

let to_format t = match t.value with Encoder f -> f | _ -> assert false

let to_request t = match t.value with Request x -> x | _ -> assert false

let to_int t = match t.value with Int s -> s | _ -> assert false

let to_int_getter t =
  match t.value with
    | Int n ->
        fun () -> n
    | Fun _ | FFI _ -> (
        fun () ->
          match (apply ~t:int_t t []).value with
            | Int n ->
                n
            | _ ->
                assert false )
    | _ ->
        assert false

let to_list t = match t.value with List l -> l | _ -> assert false

let to_tuple t = match t.value with Tuple l -> l | _ -> assert false

let to_product t =
  match t.value with Tuple [a; b] -> (a, b) | _ -> assert false

let to_metadata_list t =
  let pop v =
    let f (a, b) = (to_string a, to_string b) in
    f (to_product v)
  in
  List.map pop (to_list t)

let to_metadata t =
  let t = to_metadata_list t in
  let metas = Hashtbl.create 10 in
  List.iter (fun (a, b) -> Hashtbl.add metas a b) t ;
  metas

let to_string_list l = List.map to_string (to_list l)

let to_int_list l = List.map to_int (to_list l)

let to_source_list l = List.map to_source (to_list l)

(** [assoc lbl n l] returns the [n]th element in [l]
  * of which the first component is [lbl]. *)
let rec assoc label n = function
  | [] ->
      raise Not_found
  | (l, e) :: tl ->
      if l = label then if n = 1 then e else assoc label (n - 1) tl
      else assoc label n tl

(** {1 Parsing} *)

let type_and_run ~lib ast =
  Clock.collect_after (fun () ->
      (* Type checking *)
      Term.check ~ignored:true ast ;
      (* Check for unused variables, relies on types *)
      Term.check_unused ~lib ast ;
      ignore (Term.eval_toplevel ast))

let mk_expr ~pwd processor lexbuf =
  let processor = MenhirLib.Convert.Simplified.traditional2revised processor in
  let tokenizer = Lang_pp.mk_tokenizer ~pwd lexbuf in
  let tokenizer () =
    let token, (startp, endp) = tokenizer () in
    (token, startp, endp)
  in
  processor tokenizer

let from_in_channel ?(dir = Unix.getcwd ()) ?(parse_only = false) ~ns ~lib
    in_chan =
  let lexbuf = Sedlexing.Utf8.from_channel in_chan in
  begin
    match ns with Some ns -> Sedlexing.set_filename lexbuf ns | None -> ()
  end ;
  try
    Lang_errors.report lexbuf (fun () ->
        let expr = mk_expr ~pwd:dir Lang_parser.program lexbuf in
        if not parse_only then type_and_run ~lib expr)
  with Lang_errors.Error -> exit 1

let from_file ?parse_only ~ns ~lib filename =
  let ic = open_in filename in
  from_in_channel ~dir:(Filename.dirname filename) ?parse_only ~ns ~lib ic ;
  close_in ic

let load_libs ?parse_only () =
  let dir = Configure.libs_dir in
  let file = Filename.concat dir "pervasives.liq" in
  if Sys.file_exists file then
    from_file ?parse_only ~ns:(Some file) ~lib:true file

let from_file = from_file ~ns:None

let from_string ?parse_only ~lib expr =
  let i, o = Unix.pipe () in
  let i = Unix.in_channel_of_descr i in
  let o = Unix.out_channel_of_descr o in
  output_string o expr ;
  close_out o ;
  from_in_channel ?parse_only ~ns:None ~lib i ;
  close_in i

let eval s =
  try
    let lexbuf = Sedlexing.Utf8.from_string s in
    let expr = mk_expr ~pwd:"/nonexistent" Lang_parser.program lexbuf in
    Clock.collect_after (fun () ->
        Term.check ~ignored:false expr ;
        Some (Term.eval ~env:Term.builtins#get_all expr))
  with e ->
    Printf.eprintf "Evaluating %S failed: %s!" s (Printexc.to_string e) ;
    None

let from_in_channel ?parse_only ~lib x =
  from_in_channel ?parse_only ~ns:None ~lib x

let interactive () =
  Format.printf
    "\n\
     Welcome to the liquidsoap interactive loop.\n\n\
     You may enter any sequence of expressions, terminated by \";;\".\n\
     Each input will be fully processed: parsing, type-checking,\n\
     evaluation (forces default types), output startup (forces default clock).\n\
     @." ;
  if Dtools.Log.conf_file#get then
    Format.printf "Logs can be found in %S.\n@." Dtools.Log.conf_file_path#get ;
  let lexbuf =
    (* See ocaml-community/sedlex#45 *)
    let chunk_size = 512 in
    let buf = Bytes.create chunk_size in
    let cached = ref (-1) in
    let position = ref (-1) in
    let rec gen () =
      match (!position, !cached) with
        | _, 0 ->
            None
        | -1, _ ->
            position := 0 ;
            cached := input stdin buf 0 chunk_size ;
            gen ()
        | len, c when len = c ->
            position := -1 ;
            (* This means that the last read was a full chunk. Safe to try a new
            one right away. *)
            if len = chunk_size then gen () else None
        | len, _ ->
            position := len + 1 ;
            Some (Bytes.get buf len)
    in
    Sedlexing.Utf8.from_gen gen
  in
  let rec loop () =
    Format.printf "# %!" ;
    if
      try
        Lang_errors.report lexbuf (fun () ->
            let expr =
              mk_expr ~pwd:(Unix.getcwd ()) Lang_parser.interactive lexbuf
            in
            Term.check ~ignored:false expr ;
            Term.check_unused ~lib:true expr ;
            Clock.collect_after (fun () ->
                ignore (Term.eval_toplevel ~interactive:true expr))) ;
        true
      with
        | End_of_file ->
            Format.printf "Bye bye!@." ; false
        | Lang_errors.Error ->
            true
        | e ->
            let e = Console.colorize [`white; `bold] (Printexc.to_string e) in
            Format.printf "Exception: %s!@." e ;
            true
    then loop ()
  in
  loop () ; Tutils.shutdown ()
