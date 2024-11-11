(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

open Mm
open Source
open Extralib

let video_frei0r = Lang.add_module ~base:Modules.video "frei0r"

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
    Re.Pcre.split ~rex:(Re.Pcre.regexp ":") path
  with Not_found -> Frei0r.default_paths

class frei0r_filter ~name bgra instance params (source : source) =
  let fps = Lazy.force Frame.video_rate in
  let dt = 1. /. float fps in
  object (self)
    inherit operator ~name:("frei0r." ^ name) [source]
    method fallible = source#fallible
    method remaining = source#remaining
    method seek_source = source#seek_source
    method private can_generate_frame = source#is_ready
    method self_sync = source#self_sync
    method abort_track = source#abort_track
    val mutable t = 0.

    method private render img =
      let img = Video.Canvas.Image.render img in
      let img = Image.YUV420.to_RGBA32 img in
      if bgra then Image.RGBA32.swap_rb img;
      let src = Image.RGBA32.data (Image.RGBA32.copy img) in
      let dst = Image.RGBA32.data img in
      Frei0r.update1 instance t src dst;
      if bgra then Image.RGBA32.swap_rb img;
      t <- t +. dt;
      Video.Canvas.Image.make (Image.YUV420.of_RGBA32 img)

    method private generate_frame =
      let buf =
        Content.Video.get_data (source#get_mutable_content Frame.Fields.video)
      in
      params ();
      source#set_frame_data Frame.Fields.video Content.Video.lift_data
        {
          buf with
          Content.Video.data =
            List.map
              (fun (pos, img) -> (pos, self#render img))
              buf.Content.Video.data;
        }
  end

class frei0r_mixer ~name bgra instance params (source : source) source2 =
  let fps = Lazy.force Frame.video_rate in
  let dt = 1. /. float fps in
  let self_sync = Clock_base.self_sync [source; source2] in
  object (self)
    inherit operator ~name:("frei0r." ^ name) [source; source2]
    method seek_source = (self :> Source.source)
    method fallible = source#fallible && source2#fallible

    method remaining =
      match (source#remaining, source2#remaining) with
        | -1, x | x, -1 -> x
        | x, y -> min x y

    method private can_generate_frame = source#is_ready && source2#is_ready
    method self_sync = self_sync ~source:self ()

    method abort_track =
      source#abort_track;
      source2#abort_track

    val mutable t = 0.

    method private generate_frame =
      let length = min source#frame_position source2#frame_position in
      let c =
        Frame.get
          (source#get_partial_frame (fun f -> Frame.slice f length))
          Frame.Fields.video
      in
      let c' =
        Frame.get
          (source2#get_partial_frame (fun f -> Frame.slice f length))
          Frame.Fields.video
      in

      let rgb = Content.Video.get_data c in
      let rgb =
        self#generate_video ~field:Frame.Fields.video
          ~create:(fun ~pos ~width:_ ~height:_ () ->
            self#nearest_image ~pos
              ~last_image:(source#last_image Frame.Fields.video)
              rgb)
          length
      in
      let rgb' = Content.Video.get_data c' in

      params ();

      (* Mix content where the two streams are available.
       * We could cut one stream when the other is too short,
       * and/or attempt to get some more data in the buffers...
       * each solution has its downsides and it'll rarely matter
       * because there's usually only one image per video frame. *)
      let data =
        List.map
          (fun (pos, img) ->
            let img = Video.Canvas.Image.render img in
            let img = Image.YUV420.to_RGBA32 img in
            let img' =
              self#nearest_image ~pos
                ~last_image:(source2#last_image Frame.Fields.video)
                rgb'
            in
            let img' = Video.Canvas.Image.render img' in
            let img' = Image.YUV420.to_RGBA32 img' in
            if bgra then Image.RGBA32.swap_rb img;
            if bgra then Image.RGBA32.swap_rb img';
            let src = Image.RGBA32.data (Image.RGBA32.copy img) in
            let src' = Image.RGBA32.data img' in
            let dst = Image.RGBA32.data img in
            Frei0r.update2 instance t src src' dst;
            if bgra then Image.RGBA32.swap_rb img;
            let img = Image.YUV420.of_RGBA32 img in
            t <- t +. dt;
            (pos, Video.Canvas.Image.make img))
          rgb.Content.Video.data
      in
      source#set_frame_data Frame.Fields.video Content.Video.lift_data
        { rgb with Content.Video.data }
  end

class frei0r_source ~name bgra instance params =
  let fps = Lazy.force Frame.video_rate in
  let dt = 1. /. float fps in
  object (self)
    inherit source ~name:("frei0r." ^ name) ()
    method seek_source = (self :> Source.source)
    method fallible = false
    method private can_generate_frame = true
    method self_sync = (`Static, None)
    val mutable must_fail = false
    method abort_track = must_fail <- true
    method remaining = if must_fail then 0 else -1
    val mutable t = 0.

    method private render_image img =
      let img = Video.Canvas.Image.render img in
      let img = Image.YUV420.to_RGBA32 img in
      let dst = Image.RGBA32.data img in
      Frei0r.update0 instance t dst;
      if bgra then Image.RGBA32.swap_rb img;
      let img = Image.YUV420.of_RGBA32 img in
      t <- t +. dt;
      Video.Canvas.Image.make img

    method private generate_frame =
      if must_fail then (
        must_fail <- false;
        self#end_of_track)
      else (
        params ();
        let length = Lazy.force Frame.size in
        let buf = Frame.create ~length self#content_type in
        let rgb = self#generate_video ~field:Frame.Fields.video length in
        let data =
          List.map
            (fun (pos, img) -> (pos, self#render_image img))
            rgb.Content.Video.data
        in
        Frame.set_data buf Frame.Fields.video Content.Video.lift_data
          { rgb with Content.Video.data })
  end

(** Make a list of parameters. *)
let params plugin info =
  let liq_params =
    List.init info.Frei0r.num_params (fun i ->
        try
          let info = Frei0r.param_info plugin i in
          let name = Utils.normalize_parameter_string info.Frei0r.param_name in
          let t =
            match info.Frei0r.param_type with
              | Frei0r.Bool -> Lang.bool_t
              | Frei0r.Double -> Lang.getter_t Lang.float_t
              | Frei0r.Color -> Lang.int_t
              | Frei0r.Position -> Lang.product_t Lang.float_t Lang.float_t
              | Frei0r.String -> Lang.string_t
          in
          Some
            ( name,
              Lang.nullable_t t,
              Some Lang.null,
              Some (info.Frei0r.param_explanation ^ ".") )
        with Exit -> None)
  in
  let liq_params = List.filter_map id liq_params in
  (* Initialize parameters and produce function to update float getters. *)
  let params instance p =
    let on_changed x0 =
      let x0 = ref x0 in
      fun f x ->
        if x <> !x0 then f x;
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
            let v = Lang.to_option (f name) in
            match (v, info.Frei0r.param_type) with
              | None, _ -> None
              | Some v, Frei0r.Bool ->
                  Frei0r.set_param_bool instance i (Lang.to_bool v);
                  None
              | Some v, Frei0r.Double ->
                  let x = Lang.to_float_getter v in
                  let x0 = x () in
                  let f x = Frei0r.set_param_float instance i x in
                  let oc = on_changed x0 in
                  f x0;
                  Some (fun () -> oc f (x ()))
              | Some v, Frei0r.Color ->
                  let c = Lang.to_int v in
                  let r = (c lsr 16) land 0xff in
                  let g = (c lsr 8) land 0xff in
                  let b = c land 0xff in
                  let r = float r /. 255. in
                  let g = float g /. 255. in
                  let b = float b /. 255. in
                  Frei0r.set_param_color instance i (r, g, b);
                  None
              | Some v, Frei0r.Position ->
                  let x, y = Lang.to_product v in
                  let x = Lang.to_float x in
                  let y = Lang.to_float y in
                  Frei0r.set_param_position instance i (x, y);
                  None
              | Some v, Frei0r.String ->
                  Frei0r.set_param_string instance i (Lang.to_string v);
                  None
          with Not_found -> None)
    in
    let act = List.filter_map id act in
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
  then raise Blacklisted;
  let bgra = info.Frei0r.color_model = Frei0r.BGRA8888 in
  let inputs, _ =
    match info.Frei0r.plugin_type with
      | Frei0r.Filter -> (1, 1)
      | Frei0r.Source -> (0, 1)
      | Frei0r.Mixer2 -> (2, 1)
      | Frei0r.Mixer3 -> (3, 1)
  in
  if inputs > 2 then raise Unhandled_number_of_inputs;
  let return_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~video:(Format_type.video ()) ())
  in
  let liq_params, params = params plugin info in
  let liq_params =
    let inputs =
      List.init inputs (fun _ -> ("", Lang.source_t return_t, None, None))
    in
    liq_params @ inputs
  in
  let explanation =
    let e = info.Frei0r.explanation in
    let e = String.capitalize_ascii e in
    let e =
      Re.Pcre.substitute ~rex:(Re.Pcre.regexp "@") ~subst:(fun _ -> "(at)") e
    in
    if e = "" then e
    else if e.[String.length e - 1] = '.' then
      String.sub e 0 (String.length e - 1)
    else e
  in
  let author =
    let a = info.Frei0r.author in
    let a =
      Re.Pcre.substitute ~rex:(Re.Pcre.regexp "@") ~subst:(fun _ -> "(at)") a
    in
    a
  in
  let descr = Printf.sprintf "%s (by %s)." explanation author in
  ignore
    (Lang.add_operator ~base:video_frei0r name liq_params ~return_t
       ~category:`Video ~flags:[`Extra] ~descr (fun p ->
         let instance =
           let width = Lazy.force Frame.video_width in
           let height = Lazy.force Frame.video_height in
           Frei0r.create plugin width height
         in
         let f v = List.assoc v p in
         let params = params instance p in
         if inputs = 1 then (
           let source = Lang.to_source (f "") in
           new frei0r_filter ~name bgra instance params source)
         else if inputs = 2 then (
           let source = Lang.to_source (f "") in
           let source' = Lang.to_source (Lang.assoc "" 2 p) in
           new frei0r_mixer ~name bgra instance params source source')
         else if inputs = 0 then new frei0r_source ~name bgra instance params
         else assert false))

let register_plugin plugin =
  try register_plugin plugin with
    | Unhandled_number_of_inputs -> ()
    | Blacklisted -> ()
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
  Lifecycle.on_load ~name:"frei0r plugin registration" (fun () ->
      if frei0r_enable then
        Startup.time "Frei0r plugin registration" register_plugins)
