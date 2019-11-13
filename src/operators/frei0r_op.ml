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

open Source
open Extralib

type t = Float | Int | Bool

let log = Log.make ["frei0r"]

let frei0r_enable =
  try
    let venv = Unix.getenv "LIQ_FREI0R" in
    venv = "1" || venv = "true"
  with Not_found -> true

let plugin_dirs =
  try
    let path = Unix.getenv "LIQ_FREI0R_PATH" in
    Pcre.split ~pat:":" path
  with Not_found -> Frei0r.default_paths

class frei0r_filter ~kind ~name bgra instance params (source : source) =
  let fps = Lazy.force Frame.video_rate in
  let dt = 1. /. float fps in
  object
    inherit operator ~name:("frei0r." ^ name) kind [source]

    method stype = source#stype

    method remaining = source#remaining

    method seek = source#seek

    method is_ready = source#is_ready

    method self_sync = source#self_sync

    method abort_track = source#abort_track

    val mutable t = 0.

    method private get_frame buf =
      match VFrame.get_content buf source with
        | None ->
            ()
        | Some (rgb, offset, length) ->
            params () ;
            let rgb = rgb.(0) in
            for i = offset to offset + length - 1 do
              (* TODO: we could try to be more efficient than converting to/from RGBA32 and swap colors... *)
              let img = Video.get rgb i in
              let img = Image.YUV420.to_RGBA32 img in
              if bgra then Image.RGBA32.swap_rb img ;
              let src = Image.RGBA32.data (Image.RGBA32.copy img) in
              let dst = Image.RGBA32.data img in
              Frei0r.update1 instance t src dst ;
              if bgra then Image.RGBA32.swap_rb img ;
              let img = Image.YUV420.of_RGBA32 img in
              Video.set rgb i img ;
              t <- t +. dt
            done
  end

class frei0r_mixer ~kind ~name bgra instance params (source : source) source2 =
  let fps = Lazy.force Frame.video_rate in
  let dt = 1. /. float fps in
  object
    inherit operator ~name:("frei0r." ^ name) kind [source; source2]

    method stype =
      match (source#stype, source2#stype) with
        | Infallible, Infallible ->
            Infallible
        | _ ->
            Fallible

    method remaining =
      match (source#remaining, source2#remaining) with
        | -1, x | x, -1 ->
            x
        | x, y ->
            min x y

    method is_ready = source#is_ready && source2#is_ready

    method self_sync = source#self_sync || source2#self_sync

    method abort_track = source#abort_track ; source2#abort_track

    val mutable t = 0.

    val tmp = Frame.create kind

    method private get_frame buf =
      (* Prepare buffer for the second source
       * at the same position as final buffer. *)
      Frame.clear tmp ;
      Frame.set_breaks tmp [Frame.position buf] ;
      (* Get content in respective buffers *)
      let c = VFrame.get_content buf source in
      let c2 = VFrame.get_content tmp source2 in
      match (c, c2) with
        | Some (rgb, offset, length), Some (rgb', offset', length') ->
            params () ;
            (* Mix content where the two streams are available.
             * We could cut one stream when the other is too short,
             * and/or attempt to get some more data in the buffers...
             * each solution has its downsides and it'll rarely matter
             * because there's usually only one image per video frame. *)
            assert (offset = offset') ;
            let length = min length length' in
            let rgb = rgb.(0) in
            let rgb' = rgb'.(0) in
            for i = offset to offset + length - 1 do
              (* TODO: we could try to be more efficient than converting to/from RGBA32 and swap colors... *)
              let img = Video.get rgb i in
              let img = Image.YUV420.to_RGBA32 img in
              let img' = Video.get rgb' i in
              let img' = Image.YUV420.to_RGBA32 img' in
              if bgra then Image.RGBA32.swap_rb img ;
              if bgra then Image.RGBA32.swap_rb img' ;
              let src = Image.RGBA32.data (Image.RGBA32.copy img) in
              let src' = Image.RGBA32.data img' in
              let dst = Image.RGBA32.data img in
              Frei0r.update2 instance t src src' dst ;
              if bgra then Image.RGBA32.swap_rb img ;
              let img = Image.YUV420.of_RGBA32 img in
              Video.set rgb i img ;
              t <- t +. dt
            done
        | _ ->
            ()
  end

class frei0r_source ~kind ~name bgra instance params =
  let fps = Lazy.force Frame.video_rate in
  let dt = 1. /. float fps in
  object
    inherit source ~name:("frei0r." ^ name) kind

    method stype = Infallible

    method is_ready = true

    method self_sync = false

    val mutable must_fail = false

    method abort_track = must_fail <- true

    method remaining = if must_fail then 0 else -1

    val mutable t = 0.

    method private get_frame frame =
      if must_fail then (
        must_fail <- false ;
        VFrame.add_break frame (VFrame.position frame) )
      else (
        params () ;
        let start = VFrame.position frame in
        let stop = VFrame.size () in
        let rgb = VFrame.content_of_type frame ~channels:1 in
        let rgb = rgb.(0) in
        for i = start to stop - 1 do
          let img = Video.get rgb i in
          let img = Image.YUV420.to_RGBA32 img in
          let dst = Image.RGBA32.data img in
          Frei0r.update0 instance t dst ;
          if bgra then Image.RGBA32.swap_rb img ;
          let img = Image.YUV420.of_RGBA32 img in
          Video.set rgb i img ;
          t <- t +. dt
        done ;
        VFrame.add_break frame stop )
  end

(** Make a list of parameters. *)
let params plugin info =
  (* This is only to get default parameters... *)
  let instance = Frei0r.create plugin 8 8 in
  let liq_params =
    List.init info.Frei0r.num_params (fun i ->
        try
          let info = Frei0r.param_info plugin i in
          let name = Utils.normalize_parameter_string info.Frei0r.param_name in
          let t, d =
            match info.Frei0r.param_type with
              | Frei0r.Bool ->
                  ( Lang.bool_t,
                    Some (Lang.bool (Frei0r.get_param_bool instance i)) )
              | Frei0r.Double ->
                  ( Lang.float_getter_t (),
                    Some (Lang.float (Frei0r.get_param_float instance i)) )
              | Frei0r.Color ->
                  let r, g, b = Frei0r.get_param_color instance i in
                  let r = int_of_float (r *. 255.) in
                  let g = int_of_float (g *. 255.) in
                  let b = int_of_float (b *. 255.) in
                  let v = Lang.int ((r lsl 16) + (g lsl 8) + b) in
                  (Lang.int_t, Some v)
              | Frei0r.Position ->
                  let t = Lang.product_t Lang.float_t Lang.float_t in
                  let x, y = Frei0r.get_param_position instance i in
                  let x = Lang.float x in
                  let y = Lang.float y in
                  let v = Lang.product x y in
                  (t, Some v)
              | Frei0r.String ->
                  ( Lang.string_t,
                    Some (Lang.string (Frei0r.get_param_string instance i)) )
          in
          Some (name, t, d, Some (info.Frei0r.param_explanation ^ "."))
        with Exit -> None)
  in
  let liq_params = List.may_map id liq_params in
  (* Initialize parameters and produce function to update float getters. *)
  let params instance p =
    let on_changed x0 =
      let x0 = ref x0 in
      fun f x ->
        if x <> !x0 then f x ;
        x0 := x
    in
    let f v = List.assoc v p in
    let act =
      List.init info.Frei0r.num_params (fun i ->
          try
            let info = Frei0r.param_info plugin i in
            let name =
              Utils.normalize_parameter_string info.Frei0r.param_name
            in
            let v = f name in
            match info.Frei0r.param_type with
              | Frei0r.Bool ->
                  Frei0r.set_param_bool instance i (Lang.to_bool v) ;
                  None
              | Frei0r.Double ->
                  let x = Lang.to_float_getter v in
                  let x0 = x () in
                  let f x = Frei0r.set_param_float instance i x in
                  let oc = on_changed x0 in
                  f x0 ;
                  Some (fun () -> oc f (x ()))
              | Frei0r.Color ->
                  let c = Lang.to_int v in
                  let r = (c lsr 16) land 0xff in
                  let g = (c lsr 8) land 0xff in
                  let b = c land 0xff in
                  let r = float r /. 255. in
                  let g = float g /. 255. in
                  let b = float b /. 255. in
                  Frei0r.set_param_color instance i (r, g, b) ;
                  None
              | Frei0r.Position ->
                  let x, y = Lang.to_product v in
                  let x = Lang.to_float x in
                  let y = Lang.to_float y in
                  Frei0r.set_param_position instance i (x, y) ;
                  None
              | Frei0r.String ->
                  Frei0r.set_param_string instance i (Lang.to_string v) ;
                  None
          with Not_found -> None)
    in
    let act = List.may_map id act in
    fun () -> List.iter (fun f -> f ()) act
  in
  (liq_params, params)

exception Unhandled_number_of_inputs

exception Blacklisted

let register_plugin fname =
  let plugin = Frei0r.load fname in
  let info = Frei0r.info plugin in
  let name = Utils.normalize_parameter_string info.Frei0r.name in
  if
    List.mem name
      ["curves"; (* Bad characters in doc. *) "keyspillm0pup" (* idem *)]
  then raise Blacklisted ;
  let bgra = info.Frei0r.color_model = Frei0r.BGRA8888 in
  let inputs, _ =
    match info.Frei0r.plugin_type with
      | Frei0r.Filter ->
          (1, 1)
      | Frei0r.Source ->
          (0, 1)
      | Frei0r.Mixer2 ->
          (2, 1)
      | Frei0r.Mixer3 ->
          (3, 1)
  in
  if inputs > 2 then raise Unhandled_number_of_inputs ;
  let k =
    if inputs = 0 then Lang.video_only else Lang.any_fixed_with ~video:1 ()
  in
  let k = Lang.kind_type_of_kind_format k in
  let liq_params, params = params plugin info in
  let liq_params =
    let inputs =
      List.init inputs (fun _ -> ("", Lang.source_t k, None, None))
    in
    liq_params @ inputs
  in
  let explanation =
    let e = info.Frei0r.explanation in
    let e = String.capitalize_ascii e in
    let e = Pcre.substitute ~pat:"@" ~subst:(fun _ -> "(at)") e in
    if e = "" then e
    else if e.[String.length e - 1] = '.' then
      String.sub e 0 (String.length e - 1)
    else e
  in
  let author =
    let a = info.Frei0r.author in
    let a = Pcre.substitute ~pat:"@" ~subst:(fun _ -> "(at)") a in
    a
  in
  let descr = Printf.sprintf "%s (by %s)." explanation author in
  Lang.add_operator ("video.frei0r." ^ name) liq_params
    ~kind:(Lang.Unconstrained k) ~category:Lang.VideoProcessing ~flags:[]
    ~descr (fun p kind ->
      let instance =
        let width = Lazy.force Frame.video_width in
        let height = Lazy.force Frame.video_height in
        Frei0r.create plugin width height
      in
      let f v = List.assoc v p in
      let params = params instance p in
      if inputs = 1 then (
        let source = Lang.to_source (f "") in
        new frei0r_filter ~kind ~name bgra instance params source )
      else if inputs = 2 then (
        let source = Lang.to_source (f "") in
        let source' = Lang.to_source (Lang.assoc "" 2 p) in
        new frei0r_mixer ~kind ~name bgra instance params source source' )
      else if inputs = 0 then
        new frei0r_source ~kind ~name bgra instance params
      else assert false)

let register_plugin plugin =
  try register_plugin plugin with
    | Unhandled_number_of_inputs ->
        ()
    | Blacklisted ->
        ()
    | e ->
        Printf.eprintf "Failed to register plugin %s: %s\n%!" plugin
          (Printexc.to_string e)

let register_plugins () =
  let add plugins_dir =
    try
      let dir = Unix.opendir plugins_dir in
      try
        while true do
          let f = Unix.readdir dir in
          if f <> "." && f <> ".." then register_plugin (plugins_dir ^ "/" ^ f)
        done
      with End_of_file -> Unix.closedir dir
    with Unix.Unix_error (e, _, _) ->
      log#info "Error while loading directory %s: %s" plugins_dir
        (Unix.error_message e)
  in
  List.iter add plugin_dirs

let () =
  Configure.at_init (fun () -> if frei0r_enable then register_plugins ())
